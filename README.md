# =========================
# ANALISIS Pterygoplichthys
# =========================

install.packages(c("readxl", "dplyr", "ggplot2", "patchwork", "openxlsx"))

library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(openxlsx)

# Importar datos
datos <- read_excel("C:/Users/jois_/OneDrive/Escritorio/pez diablo/Rst/datos.xlsx")

# Orden de sitios
datos$`Collection site` <- factor(
  datos$`Collection site`,
  levels = c(
    "Humaya river",
    "Tamazula river",
    "Culiacan river",
    "Chiricahueto lagoon",
    "Irrigation canal"
  )
)

# =========================
# Normalidad y comparación por sexo
# =========================

resultados <- data.frame()
sitios <- unique(datos$`Collection site`)

for(s in sitios){
  
  datos_s <- subset(datos, `Collection site` == s)
  
  # TL
  pF <- shapiro.test(datos_s$`Total length (TL)`[datos_s$Sex == "Female"])$p.value
  pM <- shapiro.test(datos_s$`Total length (TL)`[datos_s$Sex == "Male"])$p.value
  
  if(pF > 0.05 & pM > 0.05){
    prueba <- t.test(`Total length (TL)` ~ Sex, data = datos_s)
    prueba_nombre <- "Student's t-test"
  } else {
    prueba <- wilcox.test(`Total length (TL)` ~ Sex, data = datos_s, exact = FALSE)
    prueba_nombre <- "Wilcoxon rank-sum test"
  }
  
  resultados <- rbind(resultados,
                      data.frame(
                        Sitio = s,
                        Variable = "TL",
                        Prueba = prueba_nombre,
                        Estadistico = as.numeric(prueba$statistic),
                        p.value = prueba$p.value
                      ))
  
  # W
  pF <- shapiro.test(datos_s$`Body weight (W)`[datos_s$Sex == "Female"])$p.value
  pM <- shapiro.test(datos_s$`Body weight (W)`[datos_s$Sex == "Male"])$p.value
  
  if(pF > 0.05 & pM > 0.05){
    prueba <- t.test(`Body weight (W)` ~ Sex, data = datos_s)
    prueba_nombre <- "Student's t-test"
  } else {
    prueba <- wilcox.test(`Body weight (W)` ~ Sex, data = datos_s, exact = FALSE)
    prueba_nombre <- "Wilcoxon rank-sum test"
  }
  
  resultados <- rbind(resultados,
                      data.frame(
                        Sitio = s,
                        Variable = "W",
                        Prueba = prueba_nombre,
                        Estadistico = as.numeric(prueba$statistic),
                        p.value = prueba$p.value
                      ))
}

resultados

# =========================
# Boxplot por sexo y sitio
# =========================

p1 <- ggplot(datos,
             aes(x = `Collection site`,
                 y = `Total length (TL)`,
                 fill = Sex)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(aes(color = Sex), width = 0.15, alpha = 0.6, size = 2) +
  labs(x = "", y = "Total length (cm)", title = "A") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

p2 <- ggplot(datos,
             aes(x = `Collection site`,
                 y = `Body weight (W)`,
                 fill = Sex)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(aes(color = Sex), width = 0.15, alpha = 0.6, size = 2) +
  labs(x = "Collection site", y = "Body weight (g)", title = "B") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

figura <- p1 / p2
figura

ggsave("Figura_sexo_sitio_TL_W.tiff",
       figura,
       width = 18,
       height = 20,
       units = "cm",
       dpi = 600,
       compression = "lzw")

# =========================
# Base sin sexo: Fish catch por sitio
# =========================

datos_fish <- datos %>%
  group_by(`Collection site`) %>%
  mutate(`Fish catch` = row_number()) %>%
  ungroup() %>%
  select(
    `Collection site`,
    `Fish catch`,
    `Total length (TL)`,
    `Body weight (W)`
  )

View(datos_fish)

# =========================
# Detección de outliers por sitio
# Criterio: residuos > ±2 DE
# =========================

datos_fish$logTL <- log10(datos_fish$`Total length (TL)`)
datos_fish$logW  <- log10(datos_fish$`Body weight (W)`)

outliers <- datos_fish %>%
  group_by(`Collection site`) %>%
  do({
    modelo <- lm(logW ~ logTL, data = .)
    residuos <- residuals(modelo)
    DE <- sd(residuos)
    datos_temp <- .
    datos_temp$Residuo <- residuos
    datos_temp$Outlier <- abs(residuos) > (2 * DE)
    datos_temp
  }) %>%
  ungroup()

outliers %>%
  filter(Outlier == TRUE)

datos_sin_outliers <- outliers %>%
  filter(Outlier == FALSE)

# Gráfico de outliers
figura_outliers <- ggplot(outliers,
                          aes(x = logTL,
                              y = logW)) +
  geom_point(aes(color = Outlier), size = 3) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "black",
              linewidth = 0.8) +
  facet_wrap(~ `Collection site`, scales = "free") +
  scale_color_manual(
    values = c("black", "red"),
    labels = c("Normal", "Outlier")
  ) +
  theme_bw() +
  labs(
    x = expression(log[10]*" Total length (TL)"),
    y = expression(log[10]*" Body weight (W)"),
    color = ""
  )

figura_outliers

# =========================
# Relación longitud-peso por sitio
# log10(W) = log10(a) + b log10(L)
# =========================

resultados_LWR <- data.frame()
sitios <- unique(datos_sin_outliers$`Collection site`)

for(s in sitios){
  
  datos_s <- subset(datos_sin_outliers,
                    `Collection site` == s)
  
  modelo <- lm(
    log10(`Body weight (W)`) ~ log10(`Total length (TL)`),
    data = datos_s
  )
  
  resumen <- summary(modelo)
  
  log10_a <- coef(modelo)[1]
  a <- 10^(coef(modelo)[1])
  b <- coef(modelo)[2]
  SE <- resumen$coefficients[2,2]
  IC_inf <- b - (1.96 * SE)
  IC_sup <- b + (1.96 * SE)
  t_calc <- (b - 3) / SE
  gl <- nrow(datos_s) - 2
  p_valor <- 2 * pt(-abs(t_calc), df = gl)
  
  resultados_LWR <- rbind(
    resultados_LWR,
    data.frame(
      Sitio = s,
      n = nrow(datos_s),
      log10_a = log10_a,
      a = a,
      b = b,
      SE = SE,
      IC95_inf = IC_inf,
      IC95_sup = IC_sup,
      t = t_calc,
      gl = gl,
      p.value = p_valor,
      R2 = resumen$r.squared
    )
  )
}

resultados_LWR$Crecimiento <- ifelse(
  resultados_LWR$p.value > 0.05,
  "Isometric",
  ifelse(resultados_LWR$b > 3,
         "Positive allometry",
         "Negative allometry")
)

resultados_LWR

# =========================
# Kc y Kn
# =========================

datos_condicion <- datos_sin_outliers %>%
  left_join(
    resultados_LWR %>%
      select(Sitio, a, b),
    by = c("Collection site" = "Sitio")
  ) %>%
  mutate(
    Kc = 100 * `Body weight (W)` / (`Total length (TL)`^3),
    Kn = `Body weight (W)` / (a * (`Total length (TL)`^b))
  )

tabla_condicion <- datos_condicion %>%
  group_by(`Collection site`) %>%
  summarise(
    n = n(),
    Kc_media = mean(Kc, na.rm = TRUE),
    Kc_DE = sd(Kc, na.rm = TRUE),
    Kn_media = mean(Kn, na.rm = TRUE),
    Kn_DE = sd(Kn, na.rm = TRUE),
    .groups = "drop"
  )

tabla_condicion

# =========================
# Relación longitud-peso global
# =========================

tabla_global <- datos_sin_outliers %>%
  mutate(`Fish catch` = row_number()) %>%
  select(
    `Fish catch`,
    `Collection site`,
    `Total length (TL)`,
    `Body weight (W)`
  )

modelo_global <- lm(
  log10(`Body weight (W)`) ~ log10(`Total length (TL)`),
  data = tabla_global
)

summary(modelo_global)

resumen <- summary(modelo_global)

resultado_global <- data.frame(
  n = nrow(tabla_global),
  log10_a = coef(modelo_global)[1],
  a = 10^(coef(modelo_global)[1]),
  b = coef(modelo_global)[2],
  SE = resumen$coefficients[2,2],
  IC95_inf = coef(modelo_global)[2] - (1.96 * resumen$coefficients[2,2]),
  IC95_sup = coef(modelo_global)[2] + (1.96 * resumen$coefficients[2,2]),
  t = (coef(modelo_global)[2] - 3) / resumen$coefficients[2,2],
  gl = nrow(tabla_global) - 2,
  p.value = 2 * pt(-abs((coef(modelo_global)[2] - 3) / resumen$coefficients[2,2]),
                   df = nrow(tabla_global) - 2),
  R2 = resumen$r.squared
)

resultado_global$Crecimiento <- ifelse(
  resultado_global$p.value > 0.05,
  "Isometric",
  ifelse(resultado_global$b > 3,
         "Positive allometry",
         "Negative allometry")
)

resultado_global

# =========================
# Gráfica log10 TL vs log10 W por sitio y global
# =========================

datos_global <- datos_sin_outliers
datos_global$Grupo <- "Global"

datos_sitios <- datos_sin_outliers
datos_sitios$Grupo <- datos_sitios$`Collection site`

datos_plot <- rbind(datos_sitios, datos_global)

datos_plot$Grupo <- factor(
  datos_plot$Grupo,
  levels = c(
    "Humaya river",
    "Tamazula river",
    "Culiacan river",
    "Chiricahueto lagoon",
    "Irrigation canal",
    "Global"
  )
)

ecuaciones <- datos_plot %>%
  group_by(Grupo) %>%
  do({
    modelo <- lm(
      log10(`Body weight (W)`) ~ log10(`Total length (TL)`),
      data = .
    )
    data.frame(
      R2 = summary(modelo)$r.squared,
      n = nrow(.)
    )
  })

posiciones <- datos_plot %>%
  group_by(Grupo) %>%
  summarise(
    x = min(log10(`Total length (TL)`)),
    y = max(log10(`Body weight (W)`)),
    .groups = "drop"
  )

ecuaciones <- left_join(ecuaciones,
                        posiciones,
                        by = "Grupo")

ecuaciones$texto <- paste0(
  "R² = ", round(ecuaciones$R2, 3),
  "\n",
  "n = ", ecuaciones$n
)

figura_dispersion <- ggplot(datos_plot,
                            aes(x = log10(`Total length (TL)`),
                                y = log10(`Body weight (W)`))) +
  geom_point(size = 1.8, alpha = 0.7) +
  geom_smooth(method = "lm",
              se = TRUE,
              color = "black") +
  geom_text(
    data = ecuaciones,
    aes(x = x,
        y = y,
        label = texto),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1,
    size = 3
  ) +
  facet_wrap(~ Grupo, scales = "free") +
  theme_bw() +
  labs(
    x = expression(log[10]*" Total length (TL)"),
    y = expression(log[10]*" Body weight (W)")
  )

figura_dispersion

ggsave(
  "Relacion_logTL_logW_Global_Sitios.tiff",
  figura_dispersion,
  width = 24,
  height = 18,
  units = "cm",
  dpi = 600,
  compression = "lzw"
)

# =========================
# Exportar tablas
# =========================

write.xlsx(resultados,
           "Comparacion_sexos_por_sitio.xlsx",
           rowNames = FALSE)

write.xlsx(outliers,
           "Outliers_revision.xlsx",
           rowNames = FALSE)

write.xlsx(datos_sin_outliers,
           "Datos_sin_outliers.xlsx",
           rowNames = FALSE)

write.xlsx(resultados_LWR,
           "Parametros_LWR_por_sitio.xlsx",
           rowNames = FALSE)

write.xlsx(datos_condicion,
           "Datos_Kc_Kn.xlsx",
           rowNames = FALSE)

write.xlsx(tabla_condicion,
           "Resumen_Kc_Kn_por_sitio.xlsx",
           rowNames = FALSE)

write.xlsx(tabla_global,
           "Tabla_global.xlsx",
           rowNames = FALSE)

write.xlsx(resultado_global,
           "Resultado_LWR_global.xlsx",
           rowNames = FALSE)
