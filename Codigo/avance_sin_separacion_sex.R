set.seed(123)

# --------------------------------------------------------
# LIBRERÍAS
# --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(rstanarm)
library(bayesplot)
library(bayestestR)
library(loo)
library(VIM)
library(pROC)

options(mc.cores = parallel::detectCores())


# --------------------------------------------------------
# CARGAR BASE DE DATOS
# --------------------------------------------------------

ruta_datos <- "../Data/reporte_USA.csv"
df_trabajo <- read_csv(ruta_datos)


# --------------------------------------------------------
# PREPARACIÓN DE DATOS
# --------------------------------------------------------

# Pregunta: ¿Cuáles características del atleta 
# actúan como factores predictivos para alcanzar nivel elite?

# Definir umbral elite como percentil 95 
# Para mi el umbral tiene que ser de a lo mas 5% (top)
umbral_elite <- quantile(df_trabajo$Dots, 0.95, na.rm = TRUE)

df_modelo_pre <- df_trabajo %>%
  mutate(
    Date = as.Date(Date),
    Year = year(Date),
    Age = floor(Age),
    
    Elite = if_else(Dots >= umbral_elite, 1L, 0L),
    Elite_texto = if_else(Elite == 1, "Elite", "No elite"),
    
    Equipo = case_when(
      Equipment == "Raw" ~ "Raw",
      Equipment %in% c("Wraps", "Single-ply", "Multi-ply") ~ "Equipado",
      TRUE ~ NA_character_
    ),
    
    Tested = if_else(is.na(Tested) | Tested != "Yes", 0L, 1L),
    
    # Sexo: 0 = Hombre, 1 = Mujer
    Sexo = if_else(Sex == "M", 0L, 1L),
    
    Equipo = factor(Equipo, levels = c("Raw", "Equipado"))
  ) %>%
  filter(
    Sex %in% c("M", "F"),
    !is.na(Equipo),
    !is.na(Age),
    !is.na(BodyweightKg),
    !is.na(Dots),
    !is.na(Year),
    Age >= 15,
    Age <= 100,
    BodyweightKg > 0,
    Dots > 10
  ) %>%
  select(Elite, Sexo, Age, BodyweightKg, Tested, Year, Equipo, Elite_texto, Sex)

dim(df_modelo_pre)

# --------------------------------------------------------
# REVISIÓN DE LA VARIABLE ELITE
# --------------------------------------------------------
df_modelo_pre %>%
  count(Elite_texto, sort = TRUE)

df_modelo_pre %>%
  count(Sexo) %>%
  mutate(Sexo_label = if_else(Sexo == 0, "Hombres", "Mujeres"))

# Proporción de elite por sexo (para contextualizar)
df_modelo_pre %>%
  mutate(Sexo_label = if_else(Sexo == 0, "Hombres", "Mujeres")) %>%
  group_by(Sexo_label) %>%
  summarise(
    Proporcion_Elite = mean(Elite),
    Porcentaje_Elite = mean(Elite) * 100,
    n = n(),
    .groups = "drop"
  )


## df final para el modelo
n_muestra_modelo <- min(15000, nrow(df_modelo_pre))
df_modelo <- df_modelo_pre %>%
  slice_sample(n = n_muestra_modelo) %>%
  mutate(
    Age_std = as.numeric(scale(Age)),
    Bw_std = as.numeric(scale(BodyweightKg)),
    Year_std = as.numeric(scale(Year)),
    Sexo_factor = factor(Sexo, levels = c(0, 1), labels = c("Hombre", "Mujer"))
  )

dim(df_modelo)

df_modelo %>%
  count(Elite_texto)

df_modelo %>%
  count(Sexo_factor, Equipo)


# grafico equipado y no equipado 
df_plot_elite <- df_modelo %>%
  mutate(
    RangoEdad = cut(
      Age,
      breaks = seq(15, 105, by = 5),
      right = FALSE
    ),
    Sexo_label = if_else(Sexo == 0, "Hombre", "Mujer")
  ) %>%
  group_by(RangoEdad, Sexo_label, Equipo) %>%
  summarise(
    n = n(),
    Porcentaje_Elite = mean(Elite) * 100,
    .groups = "drop"
  ) %>%
  filter(n >= 10)

grafico_elite_edad <- ggplot(
  df_plot_elite,
  aes(x = RangoEdad, y = Porcentaje_Elite, fill = Sexo_label)  
) +
  geom_col(position = "dodge") + 
  facet_wrap(~ Equipo) + 
  scale_fill_manual(
    values = c("Hombre" = "#87CEFA", "Mujer" = "#FF6EB4"),
    name = "Sexo"
  ) +
  labs(
    title = "Porcentaje de atletas Elite por edad, sin separar subgrupos",
    subtitle = "Elite = Percentil 95 en DOTS | Sexo como predictor en el modelo",
    x = "Rango de edad",
    y = "Porcentaje Elite"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    legend.position = "bottom"
  )

grafico_elite_edad


# -----------------
# DEFINIR PRIORIS
# -----------------

prior_coef <- normal(
  location = 0,
  scale = 2.5,
  autoscale = TRUE
)

prior_intercept <- normal(
  location = 0,
  scale = 5,
  autoscale = TRUE
)


# ---------
# MODELOS  
# ----------

# Modelo 0: Nulo
modelo_0 <- stan_glm(
  Elite ~ 1,
  data = df_modelo,
  family = binomial(link = "logit"),
  prior_intercept = prior_intercept,
  seed = 123,
  chains = 4,
  iter = 2000,
  refresh = 0
)

# Modelo 1: Solo variables físicas 
modelo_1 <- stan_glm(
  Elite ~ Age_std + I(Age_std^2) + Bw_std,
  data = df_modelo,
  family = binomial(link = "logit"),
  prior = prior_coef,
  prior_intercept = prior_intercept,
  seed = 123,
  chains = 4,
  iter = 2000,
  refresh = 0
)

# Modelo 2: Variables físicas + SEXO 
# este modelo responde ¿es sexo un factor para ser elite?
modelo_2 <- stan_glm(
  Elite ~ Age_std + I(Age_std^2) + Bw_std + Sexo,
  data = df_modelo,
  family = binomial(link = "logit"),
  prior = prior_coef,
  prior_intercept = prior_intercept,
  seed = 123,
  chains = 4,
  iter = 2000,
  refresh = 0
)

# Modelo 3: Variables físicas + Sexo + Equipo
modelo_3 <- stan_glm(
  Elite ~ Age_std + I(Age_std^2) + Bw_std + Sexo + Equipo,
  data = df_modelo,
  family = binomial(link = "logit"),
  prior = prior_coef,
  prior_intercept = prior_intercept,
  seed = 123,
  chains = 4,
  iter = 2000,
  refresh = 0
)

# Modelo 4: Modelo 3 + Tested
modelo_4 <- stan_glm(
  Elite ~ Age_std + I(Age_std^2) + Bw_std + Sexo + Equipo + Tested,
  data = df_modelo,
  family = binomial(link = "logit"),
  prior = prior_coef,
  prior_intercept = prior_intercept,
  seed = 123,
  chains = 4,
  iter = 2000,
  refresh = 0
)

# Modelo 5: Modelo 4 + Year
modelo_5 <- stan_glm(
  Elite ~ Age_std + I(Age_std^2) + Bw_std + Sexo + Equipo + Tested + Year_std,
  data = df_modelo,
  family = binomial(link = "logit"),
  prior = prior_coef,
  prior_intercept = prior_intercept,
  seed = 123,
  chains = 4,
  iter = 2000,
  refresh = 0
)


# ---------------------------------
# COMPARACIÓN DE MODELOS (LOO-CV)
# ---------------------------------

loo_0 <- loo(modelo_0)
loo_1 <- loo(modelo_1)
loo_2 <- loo(modelo_2)
loo_3 <- loo(modelo_3)
loo_4 <- loo(modelo_4)
loo_5 <- loo(modelo_5)

comparacion_loo <- loo_compare(loo_0, loo_1, loo_2, loo_3, loo_4, loo_5)
comparacion_loo

#info mejor modelo
nombre_mejor_modelo <- rownames(comparacion_loo)[1]
modelo_final <- get(nombre_mejor_modelo)

cat("Modelo:", nombre_mejor_modelo, "\n")
summary(modelo_final)


# --------------------------------------------------------
# RESUMEN POSTERIOR DE COEFICIENTES
# --------------------------------------------------------

posterior_coef <- as.data.frame(modelo_final)

resumen_coeficientes <- posterior_coef %>%
  pivot_longer(
    cols = everything(),
    names_to = "Parametro",
    values_to = "Valor"
  ) %>%
  group_by(Parametro) %>%
  summarise(
    Media = mean(Valor),
    Mediana = median(Valor),
    Q025 = quantile(Valor, 0.025),
    Q975 = quantile(Valor, 0.975),
    Prob_positivo = mean(Valor > 0),
    Prob_negativo = mean(Valor < 0),
    .groups = "drop"
  )

print(resumen_coeficientes)

# ========================================================
# INTERPRETACIÓN DEL IMPACTO DE SEXO
# ========================================================
# el ic no deberia pasar el 95% si el sexo es un factor importnte

coef_sexo <- resumen_coeficientes %>%
  filter(Parametro == "Sexo")

## esto esta hecho con IA, no se si dejarlo 
# el ic no deberia pasar el 95% si el sexo es un factor importnte
if (nrow(coef_sexo) > 0) {
  cat("\n=== EFECTO DE SEXO EN PROBABILIDAD DE ELITE ===\n")
  cat("Media del coeficiente:", round(coef_sexo$Media, 4), "\n")
  cat("IC 95%: [", round(coef_sexo$Q025, 4), ", ", round(coef_sexo$Q975, 4), "]\n")
  cat("Prob(Sexo > 0):", round(coef_sexo$Prob_positivo, 3), "\n")
  cat("Prob(Sexo < 0):", round(coef_sexo$Prob_negativo, 3), "\n")
  
  if (coef_sexo$Q025 > 0 | coef_sexo$Q975 < 0) {
    cat("\n✓ El coeficiente de SEXO es SIGNIFICATIVO (IC95% no incluye 0)\n")
    if (coef_sexo$Media > 0) {
      cat("  → Mujeres (Sexo=1) tienen MAYOR probabilidad de ser elite que hombres\n")
    } else {
      cat("  → Hombres (Sexo=0) tienen MAYOR probabilidad de ser elite que mujeres\n")
    }
  } else {
    cat("\n✗ El coeficiente de SEXO NO es significativo (IC95% incluye 0)\n")
    cat("  → No hay evidencia de que sexo sea un factor diferenciador de elite\n")
  }
}
##


# --------------------------------------------------------
# ODDS RATIOS
# --------------------------------------------------------

resumen_odds_ratios <- resumen_coeficientes %>%
  mutate(
    OR_Media = exp(Media),
    OR_Q025 = exp(Q025),
    OR_Q975 = exp(Q975)
  )

print(resumen_odds_ratios)


# --------------------------------------------------------
# GRÁFICO DE EFECTOS POSTERIORES
# --------------------------------------------------------

grafico_efectos <- plot(
  modelo_final,
  "areas",
  prob = 0.95,
  prob_outer = 1
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Distribuciones posteriores de los coeficientes",
    subtitle = "Modelo logístico bayesiano: ¿es SEXO un predictor de elite?"
  ) +
  theme_minimal()

grafico_efectos


# --------------------------------------------------------
# PREDICCIÓN POSTERIOR SEGÚN EDAD (por sexo)
# --------------------------------------------------------

edades <- seq(15, 70, by = 1)

edad_media <- mean(df_modelo$Age)
edad_sd <- sd(df_modelo$Age)

datos_pred_edad <- expand_grid(
  Edad = edades,
  Sexo = c(0, 1),
  Equipo = "Raw"
) %>%
  mutate(
    Age_std = (Edad - edad_media) / edad_sd,
    Bw_std = 0,
    Tested = 0L,
    Year_std = 0,
    Sexo_label = if_else(Sexo == 0, "Hombre", "Mujer")
  )


pred_post_edad <- posterior_epred(
  modelo_final,
  newdata = datos_pred_edad
)

df_curva_edad <- datos_pred_edad %>%
  mutate(
    Media = apply(pred_post_edad, 2, mean),
    Q05 = apply(pred_post_edad, 2, quantile, 0.05),
    Q95 = apply(pred_post_edad, 2, quantile, 0.95)
  )

grafico_prob_edad <- ggplot(
  df_curva_edad,
  aes(x = Edad, y = Media, ymin = Q05, ymax = Q95, color = Sexo_label, fill = Sexo_label)
) +
  geom_ribbon(alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c("Hombre" = "#87CEFA", "Mujer" = "#FF6EB4"),
    name = "Sexo"
  ) +
  scale_fill_manual(
    values = c("Hombre" = "#87CEFA", "Mujer" = "#FF6EB4"),
    name = "Sexo"
  ) +
  labs(
    title = "Probabilidad posterior de ser Elite según edad",
    subtitle = "Sin separar subgrupos | Intervalo creíble 90%",
    x = "Edad",
    y = "P(Elite)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom"
  )

grafico_prob_edad


# --------------------------------------------------------
# PREDICCIÓN POSTERIOR SEGÚN PESO CORPORAL (por sexo)
# --------------------------------------------------------

pesos <- seq(
  from = quantile(df_modelo$BodyweightKg, 0.05),
  to = quantile(df_modelo$BodyweightKg, 0.95),
  by = 1
)

peso_media <- mean(df_modelo$BodyweightKg)
peso_sd <- sd(df_modelo$BodyweightKg)

datos_pred_peso <- expand_grid(
  Peso = pesos,
  Sexo = c(0, 1),
  Equipo = "Raw"
) %>%
  mutate(
    Age_std = 0,
    Bw_std = (Peso - peso_media) / peso_sd,
    Tested = 0L,
    Year_std = 0,
    Sexo_label = if_else(Sexo == 0, "Hombre", "Mujer")
  )

pred_post_peso <- posterior_epred(
  modelo_final,
  newdata = datos_pred_peso
)

df_curva_peso <- datos_pred_peso %>%
  mutate(
    Media = apply(pred_post_peso, 2, mean),
    Q05 = apply(pred_post_peso, 2, quantile, 0.05),
    Q95 = apply(pred_post_peso, 2, quantile, 0.95)
  )

grafico_prob_peso <- ggplot(
  df_curva_peso,
  aes(x = Peso, y = Media, ymin = Q05, ymax = Q95, color = Sexo_label, fill = Sexo_label)
) +
  geom_ribbon(alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c("Hombre" = "#87CEFA", "Mujer" = "#FF6EB4"),
    name = "Sexo"
  ) +
  scale_fill_manual(
    values = c("Hombre" = "#87CEFA", "Mujer" = "#FF6EB4"),
    name = "Sexo"
  ) +
  labs(
    title = "Probabilidad posterior de ser Elite según peso corporal",
    subtitle = "Sin separar subgrupos | Intervalo creíble 90%",
    x = "Peso corporal (kg)",
    y = "P(Elite)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom"
  )

grafico_prob_peso


# --------------------------------------------------------
# POSTERIOR PREDICTIVE CHECK
# --------------------------------------------------------

grafico_pp_check <- pp_check(
  modelo_final,
  plotfun = "bars",
  nreps = 100
) +
  labs(
    title = "Posterior Predictive Check",
    subtitle = "Datos observados vs simulados desde el modelo"
  ) +
  theme_minimal()

grafico_pp_check


# --------------------------------------------------------
# DIAGNÓSTICOS MCMC
# --------------------------------------------------------

# Traceplot
parametros_trace <- names(coef(modelo_final))

grafico_trace <- mcmc_trace(
  as.array(modelo_final),
  pars = parametros_trace
) +
  labs(
    title = "Traceplot del modelo final"
  ) +
  theme_minimal()

grafico_trace

# Autocorrelación
plot(modelo_final, "acf_bar")


# --------------------------------------------------------
# CURVA ROC Y AUC
# --------------------------------------------------------

pred_post_obs <- posterior_epred(
  modelo_final,
  newdata = df_modelo
)

df_modelo_roc <- df_modelo %>%
  mutate(
    Prob_Elite = apply(pred_post_obs, 2, mean)
  )

roc_modelo <- roc(
  response = df_modelo_roc$Elite,
  predictor = df_modelo_roc$Prob_Elite,
  levels = c(0, 1),
  direction = "<"
)

auc_modelo <- auc(roc_modelo)
auc_modelo

grafico_roc <- ggroc(roc_modelo) +
  geom_abline(
    intercept = 1,
    slope = 1,
    linetype = "dashed"
  ) +
  labs(
    title = "Curva ROC del modelo final",
    subtitle = paste("AUC =", round(auc_modelo, 3)),
    x = "Especificidad",
    y = "Sensibilidad"
  ) +
  theme_minimal()

grafico_roc
