
# DIAGNOSTICO DEL MODELO FINAL

# Modelo final:
# MEJOR: (modelo6 stepward) Elite ~ Age_std + I(Age_std^2) + Bw_std + Sex + Equipo + Tested_bin + Sex:Equipo

# SEGUNDO MEJOR: (modelo 4)
# Elite ~ Age_std + I(Age_std^2) + Bw_std + Sex * Equipo + Tested_bin + Year_std

# --------------------------------------------------------
# --------------------------------------------------------
# 1. LIBRERIAS
# --------------------------------------------------------
# --------------------------------------------------------

set.seed(123)

library(tidyverse)
library(rstanarm)
library(bayesplot)
library(bayestestR)
library(loo)
library(pROC)
library(car)

options(mc.cores = parallel::detectCores())

# --------------------------------------------------------
# --------------------------------------------------------
# 2. CARGAR BASE LIMPIA
# --------------------------------------------------------
# --------------------------------------------------------

ruta_base <- "base_modelo_USA.csv"

df_modelo <- read_csv(ruta_base) %>%
  mutate(
    Elite = as.integer(Elite),
    Sex = factor(Sex, levels = c("M", "F")),
    Equipo = factor(Equipo, levels = c("Raw", "Equipado")),
    Elite_texto = factor(Elite_texto, levels = c("No elite", "Elite"))
  )

# --------------------------------------------------------
# --------------------------------------------------------
# 3. DEFINIR PRIORIS
# --------------------------------------------------------
# --------------------------------------------------------

prior_coeficientes <- normal(
  location = 0,
  scale = 2.5,
  autoscale = TRUE
)

prior_intercepto <- normal(
  location = 0,
  scale = 5,
  autoscale = TRUE
)


# --------------------------------------------------------
# --------------------------------------------------------
# 4. AJUSTAR MODELO FINAL BAYESIANO
# --------------------------------------------------------
# --------------------------------------------------------

#Formila Final 
formula_final <- Elite ~  Age_std + I(Age_std^2)+ Bw_std + Sex + Equipo + Tested_bin + Sex:Equipo
modelo_final <- stan_glm(
  formula = formula_final,
  data = df_modelo,
  family = binomial(link = "logit"),
  prior = prior_coeficientes,
  prior_intercept = prior_intercepto,
  seed = 123,
  chains = 4,
  iter = 2000,
  refresh = 0,
  control = list(adapt_delta = 0.95)
)

summary(modelo_final)
prior_summary(modelo_final)


# --------------------------------------------------------
# --------------------------------------------------------
# 5. RESUMEN POSTERIOR DE LOS COEFICIENTES
# --------------------------------------------------------
# --------------------------------------------------------

coeficientes_modelo <- coef(modelo_final)
intervalos_90 <- posterior_interval(modelo_final, prob = 0.90)

resumen_coeficientes <- data.frame(
  Parametro = names(coeficientes_modelo),
  Media = as.numeric(coeficientes_modelo),
  LI_90 = intervalos_90[, 1],
  LS_90 = intervalos_90[, 2]
)
resumen_coeficientes


# --------------------------------------------------------
# --------------------------------------------------------
# 6. ODDS RATIOS
# --------------------------------------------------------
# --------------------------------------------------------

# Al aplicar exp(), se interpretan como odds ratios, porque estamos en log.

odds_ratios <- resumen_coeficientes %>%
  mutate(OR = exp(Media), OR_LI_90 = exp(LI_90), OR_LS_90 = exp(LS_90) )

odds_ratios


# --------------------------------------------------------
# --------------------------------------------------------
# 7. GRAFICO DE EFECTOS POSTERIORES
# --------------------------------------------------------
# --------------------------------------------------------

mcmc_areas(
  as.matrix(modelo_final),
  pars = c(
    "Age_std",
    "I(Age_std^2)",
    "SexF",
    "EquipoEquipado",
    "Tested_bin",
    "Bw_std",
    "SexF:EquipoEquipado"
  ),
  prob = 0.95
) +
  labs(
    title = "Distribuciones posteriores de los coeficientes",
    subtitle = "Intervalos posteriores al 95%",
    x = "Valor del coeficiente",
    y = "Parametro"
  ) +
  theme_minimal()


# --------------------------------------------------------
# --------------------------------------------------------
# 8. DIAGNOSTICO MCMC
# --------------------------------------------------------
# --------------------------------------------------------

# Rhat cercano a 1 y n_eff alto indican buen comportamiento de las cadenas.

summary(modelo_final)

mcmc_trace(
  as.array(modelo_final),
  pars = c(
    "Age_std",
    "I(Age_std^2)",
    "SexF",
    "EquipoEquipado",
    "Tested_bin",
    "Bw_std",
    "SexF:EquipoEquipado"
  )
) +
  labs(
    title = "Traceplot del modelo final"
  ) +
  theme_minimal()


# --------------------------------------------------------
# --------------------------------------------------------
# 9. POSTERIOR PREDICTIVE CHECK
# --------------------------------------------------------
# --------------------------------------------------------

# Compara datos observados con datos simulados desde el modelo.
# Cambiar color Y diferentes
pp_check(modelo_final, type = "bars", ndraws = 100) +
  labs(
    title = "Posterior predictive check del modelo final"
  ) + theme_minimal()


# --------------------------------------------------------
# --------------------------------------------------------
# 10. PREDICCION POSTERIOR PROMEDIO
# --------------------------------------------------------
# --------------------------------------------------------

# se usa para dsp graficar

prob_posterior <- posterior_epred(modelo_final)
prob_media <- colMeans(prob_posterior)

df_modelo <- df_modelo %>% mutate(prob_predicha = prob_media)


# --------------------------------------------------------
# --------------------------------------------------------
# 11. CURVA ROC Y AUC
# --------------------------------------------------------
# --------------------------------------------------------

# tal vez borrar porque no es mucho

#roc_modelo <- roc(
#  response = df_modelo$Elite,
#  predictor = df_modelo$prob_predicha,
#  levels = c(0, 1),
#  direction = "<",
#  quiet = TRUE
#)
#
#auc_modelo <- auc(roc_modelo)
#auc_modelo
#
#plot(
#  roc_modelo,
#  main = paste("Curva ROC del modelo final - AUC =", round(auc_modelo, 3)) )
## --------------------------------------------------------
## --------------------------------------------------------
# 12. GRAFICO: PROBABILIDAD PREDICHA SEGUN EDAD
# --------------------------------------------------------
# --------------------------------------------------------

grafico_prob_edad <- ggplot(
  df_modelo,
  aes(x = Age, y = prob_predicha, color = Sex)
) +
  geom_point(alpha = 0.15) +
  geom_smooth(se = FALSE, method = "loess") +
  facet_wrap(~ Equipo) +
  labs(
    title = "Probabilidad predicha de ser Elite segun edad",
    subtitle = "Modelo final bayesiano logistico",
    x = "Edad",
    y = "Probabilidad predicha de ser Elite",
    color = "Sexo"
  ) +
  theme_minimal()

grafico_prob_edad


# --------------------------------------------------------
# --------------------------------------------------------
# 13. GRAFICO: PROBABILIDAD PREDICHA SEGUN AÑO
# --------------------------------------------------------
# --------------------------------------------------------

grafico_prob_year <- ggplot(
  df_modelo,
  aes(x = Year, y = prob_predicha, color = Equipo)
) +
  geom_point(alpha = 0.15) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(
    title = "Probabilidad predicha de ser Elite segun año",
    subtitle = "Modelo final bayesiano logistico",
    x = "Año",
    y = "Probabilidad predicha de ser Elite",
    color = "Equipamiento"
  ) +
  theme_minimal()

grafico_prob_year



# --------------------------------------------------------
# --------------------------------------------------------
# 14. MODELO GLM AUXILIAR PARA DIAGNOSTICOS CLASICOS
# --------------------------------------------------------
# --------------------------------------------------------

# Este modelo NO reemplaza al modelo bayesiano.
# Se usa solo como apoyo para calcular residuos, leverage y Cook.

modelo_glm_diag <- glm(
  formula_final,
  data = df_modelo,
  family = binomial(link = "logit")
)

summary(modelo_glm_diag)


# --------------------------------------------------------
# --------------------------------------------------------
# 15. VALOR-P Y TEST DE WALD
# --------------------------------------------------------
# --------------------------------------------------------

tabla_wald <- as.data.frame(summary(modelo_glm_diag)$coefficients) %>%
  rownames_to_column("Parametro") %>%
  rename(
    Estimacion = Estimate,
    Error_Estandar = `Std. Error`,
    Valor_z = `z value`,
    Valor_p = `Pr(>|z|)`
  )

tabla_wald

test_wald_global <- Anova(
  modelo_glm_diag,
  type = 2,
  test.statistic = "Wald"
)

test_wald_global


# --------------------------------------------------------
# --------------------------------------------------------
# 16. GVIF / VIF
# --------------------------------------------------------
# --------------------------------------------------------

# GVIF sirve para revisar multicolinealidad.

vif_modelo <- vif(modelo_glm_diag)
tabla_gvif <- data.frame(Variable = names(vif_modelo), VIF = as.numeric(vif_modelo))

tabla_gvif


# --------------------------------------------------------
# --------------------------------------------------------
# 18. RESIDUOS, LEVERAGE Y COOK
# --------------------------------------------------------
# --------------------------------------------------------

# En regresion logistica no se espera normalidad en los residuos.
# Por eso uso residuos de Pearson, deviance y studentizados.

residuos_pearson <- residuals(modelo_glm_diag, type = "pearson")
residuos_deviance <- residuals(modelo_glm_diag, type = "deviance")
valores_ajustados <- fitted(modelo_glm_diag)

leverage <- hatvalues(modelo_glm_diag)
cook <- cooks.distance(modelo_glm_diag)

residuos_student <- residuos_pearson / sqrt(1 - leverage)


# --------------------------------------------------------
# --------------------------------------------------------
# 19. UMBRALES DE DIAGNOSTICO
# --------------------------------------------------------
# --------------------------------------------------------

n <- nrow(df_modelo)
p <- length(coef(modelo_glm_diag))

umbral_leverage <- 2 * p / n
umbral_cook <- 4 / n

umbrales_diagnostico <- tibble(
  Diagnostico = c(
    "Leverage",
    "Distancia de Cook"
  ),
  Umbral = c(
    umbral_leverage,
    umbral_cook
  )
)

umbrales_diagnostico


# --------------------------------------------------------
# --------------------------------------------------------
# 20. BASE PARA GRAFICOS DE DIAGNOSTICO
# --------------------------------------------------------
# --------------------------------------------------------

df_diagnostico <- df_modelo %>%
  mutate(
    id = row_number(),
    ajustado_glm = as.numeric(valores_ajustados),
    resid_pearson = as.numeric(residuos_pearson),
    resid_deviance = as.numeric(residuos_deviance),
    resid_student = as.numeric(residuos_student),
    leverage = as.numeric(leverage),
    cook = as.numeric(cook),
    
    flag_resid_student = abs(resid_student) > 3,
    flag_leverage = leverage > umbral_leverage,
    flag_cook = cook > umbral_cook,
    
    observacion_influyente = flag_resid_student |
      flag_leverage |
      flag_cook
  )

resumen_flags <- df_diagnostico %>%
  summarise(
    n = n(),
    residuos_student_altos = sum(flag_resid_student, na.rm = TRUE),
    leverage_altos = sum(flag_leverage, na.rm = TRUE),
    cook_altos = sum(flag_cook, na.rm = TRUE),
    observaciones_influyentes = sum(observacion_influyente, na.rm = TRUE)
  )

resumen_flags

observaciones_influyentes <- df_diagnostico %>%
  filter(observacion_influyente) %>%
  arrange(desc(cook)) %>%
  select(
    id,
    Elite,
    Elite_texto,
    Sex,
    Equipo,
    Tested_bin,
    Age,
    Year,
    prob_predicha,
    ajustado_glm,
    resid_pearson,
    resid_deviance,
    resid_student,
    leverage,
    cook
  )

observaciones_influyentes

nrow(observaciones_influyentes)


# --------------------------------------------------------
# --------------------------------------------------------
# 21. GRAFICO: RESIDUOS VS VALORES AJUSTADOS
# --------------------------------------------------------
# --------------------------------------------------------

grafico_residuos_ajustados <- ggplot(
  df_diagnostico,
  aes(x = ajustado_glm, y = resid_pearson)
) +
  geom_point(alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuos de Pearson vs valores ajustados",
    subtitle = "Diagnostico auxiliar del modelo logistico",
    x = "Valores ajustados",
    y = "Residuos de Pearson"
  ) +
  theme_minimal()

grafico_residuos_ajustados


# --------------------------------------------------------
# --------------------------------------------------------
# 22. GRAFICO: RESIDUOS DE DEVIANCE VS VALORES AJUSTADOS
# --------------------------------------------------------
# --------------------------------------------------------

grafico_deviance_ajustados <- ggplot(
  df_diagnostico,
  aes(x = ajustado_glm, y = resid_deviance)
) +
  geom_point(alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuos de deviance vs valores ajustados",
    subtitle = "Diagnostico auxiliar del modelo logistico",
    x = "Valores ajustados",
    y = "Residuos de deviance"
  ) +
  theme_minimal()

grafico_deviance_ajustados


# --------------------------------------------------------
# --------------------------------------------------------
# 23. GRAFICO: RESIDUOS STUDENTIZADOS
# --------------------------------------------------------
# --------------------------------------------------------

grafico_residuos_student <- ggplot(
  df_diagnostico,
  aes(x = id, y = resid_student)
) +
  geom_point(alpha = 0.45) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted") +
  labs(
    title = "Residuos studentizados",
    subtitle = "Lineas punteadas en -3 y 3",
    x = "Observacion",
    y = "Residuo studentizado"
  ) +
  theme_minimal()

grafico_residuos_student


# --------------------------------------------------------
# --------------------------------------------------------
# 24. GRAFICO: LEVERAGE
# --------------------------------------------------------
# --------------------------------------------------------

grafico_leverage <- ggplot(
  df_diagnostico,
  aes(x = id, y = leverage)
) +
  geom_point(alpha = 0.45) +
  geom_hline(yintercept = umbral_leverage, linetype = "dashed") +
  labs(
    title = "Leverage por observacion",
    subtitle = paste("Umbral aproximado =", round(umbral_leverage, 4)),
    x = "Observacion",
    y = "Leverage"
  ) +
  theme_minimal()

grafico_leverage


# --------------------------------------------------------
# --------------------------------------------------------
# 25. GRAFICO: DISTANCIA DE COOK
# --------------------------------------------------------
# --------------------------------------------------------

df_cook_plot <- df_diagnostico %>%
  select(id, cook) %>%
  filter(
    !is.na(id),
    !is.na(cook),
    is.finite(cook)
  )

plot(
  x = df_cook_plot$id,
  y = df_cook_plot$cook,
  pch = 16,
  cex = 0.5,
  xlab = "Observacion",
  ylab = "Distancia de Cook",
  main = paste("Distancia de Cook por observacion\nUmbral aproximado =", round(umbral_cook, 4))
)

abline(
  h = umbral_cook,
  lty = 2
)


# --------------------------------------------------------
# --------------------------------------------------------
# 26. RESUMEN FINAL DEL DIAGNOSTICO
# --------------------------------------------------------
# --------------------------------------------------------

resumen_final_diagnostico <- tibble(
  Medida = c(
    #"AUC",
    "N observaciones",
    "N parametros",
    "Observaciones con residuo studentizado alto",
    "Observaciones con leverage alto",
    "Observaciones con Cook alto",
    "Observaciones influyentes totales"
  ),
  Valor = c(
    #as.numeric(auc_modelo),
    n,
    p,
    resumen_flags$residuos_student_altos,
    resumen_flags$leverage_altos,
    resumen_flags$cook_altos,
    resumen_flags$observaciones_influyentes
  )
)

resumen_final_diagnostico

