# --------------------------------------------------------
# JUSTIFICACION: ELECCION DEL PERCENTIL PARA "ELITE"
# Este codigo en esencia, es para fundamentar nuestra 
# eleccion del percentil.
# --------------------------------------------------------

set.seed(123)
library(tidyverse)
library(lubridate)
library(rstanarm)
library(rstan)
library(pROC)

#Cargamos la base de datos
df_trabajo <- read_csv("../../Data/reporte_USA.csv")

df_modelo_pre <- df_trabajo %>%
  mutate(
    Date = as.Date(Date),
    Year = year(Date),
    Age = floor(Age),
    Equipo = case_when(
      Equipment == "Raw" ~ "Raw",
      Equipment %in% c("Wraps", "Single-ply", "Multi-ply") ~ "Equipado",
      TRUE ~ NA_character_
    ),
    Tested_bin = if_else(is.na(Tested) | Tested != "Yes", 0L, 1L),
    Sex = factor(Sex, levels = c("M", "F")),
    Equipo = factor(Equipo, levels = c("Raw", "Equipado")),
    Event = factor(Event)
  ) %>%
  filter(
    Event == "SBD", Sex %in% c("M", "F"), !is.na(Equipo), !is.na(Age),
    !is.na(BodyweightKg), !is.na(Dots), !is.na(Year),
    Age >= 15, Age <= 70, BodyweightKg >= 35, BodyweightKg <= 200, Dots > 10
  )

#Se construye ambas versiones de Elite
construir_base <- function(percentil) {
  umbral <- quantile(df_modelo_pre$Dots, percentil, na.rm = TRUE)
  df_modelo_pre %>%
    mutate(
      Elite = if_else(Dots >= umbral, 1L, 0L),
      Elite_texto = if_else(Elite == 1, "Elite", "No elite"),
      Age_std = as.numeric(scale(Age)),
      Bw_std = as.numeric(scale(BodyweightKg)),
      Year_std = as.numeric(scale(Year))
    )
}

#y aqui creamos nuestros dos dfs con tal de comparar
df_p95 <- construir_base(0.95)
df_p80 <- construir_base(0.80)

#Ajustamos los modelos (son los mismos que se utilizan en los otros archivos)
prior_coef <- normal(0, 2.5, autoscale = TRUE)
prior_int  <- normal(0, 5, autoscale = TRUE)
formula_final <- Elite ~ Age_std + I(Age_std^2) + Bw_std + Sex + Equipo + Tested_bin + Sex:Equipo
ajustar <- function(data) {
  stan_glm(formula_final, data = data, family = binomial(link = "logit"),
           prior = prior_coef, prior_intercept = prior_int,
           seed = 123, chains = 4, iter = 2000, refresh = 0,
           control = list(adapt_delta = 0.95))
}

modelo_final_p95 <- ajustar(df_p95)
modelo_final_p80 <- ajustar(df_p80)

#Y se compara
comparar_percentiles <- function(modelo, data, etiqueta) {
  resumen <- summary(modelo)
  prob <- colMeans(posterior_epred(modelo))
  roc_obj <- roc(data$Elite, prob, levels = c(0,1), direction = "<", quiet = TRUE)
  tibble(
    version = etiqueta,
    n = nrow(data),
    pct_elite = mean(data$Elite) * 100,
    rhat_max = max(resumen[, "Rhat"], na.rm = TRUE),
    ess_min = min(resumen[, "n_eff"], na.rm = TRUE),
    auc = as.numeric(auc(roc_obj))
  )
}
#Y creamos la tabla para visualizar la comparación
tabla_comparacion_percentiles <- bind_rows(
  comparar_percentiles(modelo_final_p95, df_p95, "p95"),
  comparar_percentiles(modelo_final_p80, df_p80, "p80")
)

tabla_comparacion_percentiles
write_csv(tabla_comparacion_percentiles, "tabla_comparacion_percentiles.csv")

# Por lo tanto elegimos percentil 95% (mejor AUC: 0.781 vs 0.716;
# convergencia adecuada en ambos casos; mas consistente conceptualmente
# con la nocion de Elite en powerlifting).