
# ELEGIR MEJOR MODELO

# --------------------------------------------------------
# --------------------------------------------------------
# 1. LIBRERIAS
# --------------------------------------------------------
# --------------------------------------------------------
seed <- 123
set.seed(seed)
install.packages("bridgesampling")
library(tidyverse)
library(rstanarm)
library(bridgesampling)
library(loo)

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

dim(df_modelo)

df_modelo %>%
  count(Elite_texto)

df_modelo %>%
  count(Sex, Equipo)


# --------------------------------------------------------
# --------------------------------------------------------
# 3. DEFINIR PRIORIS
# --------------------------------------------------------
# --------------------------------------------------------

# como las variables continuas fueron estandarizadas,
# usamos prioris normales centradas en 0.
# la priori normal(0, 2.5) es comun en regresion logistica bayesiana.

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
# 4. FUNCION PARA AJUSTAR MODELOS BAYESIANOS
# --------------------------------------------------------
# --------------------------------------------------------

ajustar_modelo <- function(formula_modelo, data, iter = 2000, seed = seed) {
  
  stan_glm(
    formula = formula_modelo,
    data = data,
    family = binomial(link = "logit"),
    prior = prior_coeficientes,
    prior_intercept = prior_intercepto,
    seed = seed,
    chains = 4,
    iter = iter,
    refresh = 0,
    control = list(adapt_delta = 0.95)
  )
}


# --------------------------------------------------------
# --------------------------------------------------------
# 5. STEPWISE COMO MODELO CANDIDATO
# --------------------------------------------------------
# --------------------------------------------------------

# El stepwise se usa solo para proponer una formula candidata.
# La seleccion final no se hace con stepwise, sino con LOO.

formula_nula <- Elite ~ 1

formula_completa <- Elite ~ 
  Age_std + I(Age_std^2) + Bw_std + Sex + Equipo + Tested_bin + Year_std + Sex:Equipo

modelo_glm_completo <- glm(
  formula_completa,
  data = df_modelo,
  family = binomial(link = "logit")
)

modelo_step_glm <- step(
  modelo_glm_completo,
  scope = list(
    lower = formula_nula,
    upper = formula_completa
  ),
  direction = "both",
  trace = TRUE
)

formula_step <- formula(modelo_step_glm)
formula_step


# --------------------------------------------------------
# --------------------------------------------------------
# 6. MODELOS BAYESIANOS LOGISTICOS
# --------------------------------------------------------
# --------------------------------------------------------

# Modelo 0:
# Modelo nulo, no usa caracteristicas del atleta.

modelo_0 <- ajustar_modelo(
  Elite ~ 1,
  data = df_modelo,
  iter = 2000,
  seed = seed
)


# Modelo 1:
# Solo variables fisicas.

modelo_1 <- ajustar_modelo(
  Elite ~ Age_std + I(Age_std^2) + Bw_std,
  data = df_modelo,
  iter = 2000,
  seed = seed
)


# Modelo 2:
# Variables fisicas + sexo + tipo de equipo.

modelo_2 <- ajustar_modelo(
  Elite ~ Age_std + I(Age_std^2) + Bw_std + Sex + Equipo,
  data = df_modelo,
  iter = 2000,
  seed = seed
)


# Modelo 3:
# Variables fisicas + sexo + tipo de equipo + testeo + año.

modelo_3 <- ajustar_modelo(
  Elite ~ Age_std + I(Age_std^2) + Bw_std + Sex + Equipo + Tested_bin + Year_std,
  data = df_modelo,
  iter = 2000,
  seed = seed
)


# Modelo 4:
# Modelo con interaccion entre sexo y tipo de equipo.

modelo_4 <- ajustar_modelo(
  Elite ~ Age_std + I(Age_std^2) + Bw_std + Sex * Equipo + Tested_bin + Year_std,
  data = df_modelo,
  iter = 2000,
  seed = seed
)


# Modelo 5:
# Modelo mas simple con edad, edad cuadratica y sexo.

modelo_5 <- ajustar_modelo(
  Elite ~ Age_std + I(Age_std^2) + Sex,
  data = df_modelo,
  iter = 2000,
  seed = seed
)


# Modelo 6:
# Modelo propuesto por stepwise.

modelo_6_step <- ajustar_modelo(
  formula_step,
  data = df_modelo,
  iter = 2000,
  seed = seed
)


# --------------------------------------------------------
# --------------------------------------------------------
# 7. GUARDAR MODELOS EN UNA LISTA
# --------------------------------------------------------
# --------------------------------------------------------

modelos <- list(
  modelo_0 = modelo_0,
  modelo_1 = modelo_1,
  modelo_2 = modelo_2,
  modelo_3 = modelo_3,
  modelo_4 = modelo_4,
  modelo_5 = modelo_5,
  modelo_6_step = modelo_6_step
)


# --------------------------------------------------------
# --------------------------------------------------------
# 8. RESUMEN DE LOS MODELOS
# --------------------------------------------------------
# --------------------------------------------------------

summary(modelo_0)
summary(modelo_1)
summary(modelo_2)
summary(modelo_3)
summary(modelo_4)
summary(modelo_5)
summary(modelo_6_step)


# --------------------------------------------------------
# --------------------------------------------------------
# 9. COMPARACION DE MODELOS CON LOO
# --------------------------------------------------------
# --------------------------------------------------------

# LOO compara los modelos segun capacidad predictiva.
# El mejor modelo aparece primero en loo_compare.

loo_0 <- loo(modelo_0)
loo_1 <- loo(modelo_1)
loo_2 <- loo(modelo_2)
loo_3 <- loo(modelo_3)
loo_4 <- loo(modelo_4)
loo_5 <- loo(modelo_5)
loo_6_step <- loo(modelo_6_step)

comparacion_loo <- loo_compare(
  loo_0,
  loo_1,
  loo_2,
  loo_3,
  loo_4,
  loo_5,
  loo_6_step
)

comparacion_loo

# --------------------------------------------------------
# --------------------------------------------------------
# 10. TOMAR EL MEJOR MODELO
# --------------------------------------------------------
# --------------------------------------------------------

# El mejor modelo es el que aparece primero en la comparacion LOO

nombre_modelo_final <- rownames(comparacion_loo)[1]
nombre_modelo_final
nombre_modelo_final_2 <- rownames(comparacion_loo)[2]
nombre_modelo_final_2

modelo_final <- modelos[[nombre_modelo_final]]
modelo_final2 <- modelos[[nombre_modelo_final_2]]

summary(modelo_final)
formula(modelo_final)

summary(modelo_final2)
formula(modelo_final2)

# --------------------------------------------------------
# --------------------------------------------------------
# 11. FACTOR DE BAYES: modelo_6_step vs modelo_final
# --------------------------------------------------------
# --------------------------------------------------------

# Carpeta temporal para los archivos de diagnostico
dir_diag <- file.path(tempdir(), "diagnostics")
dir.create(dir_diag, showWarnings = FALSE)


# Se debieron reentrenar los modelos para incluir archivos de diagnóstico de Stan,
# ya que stan_glm() no los guarda por defecto. Esto es necesario para poder usar
# bridge_sampler() correctamente en el cálculo del Factor de Bayes.
modelo_final_bf <- stan_glm(
  formula        = formula(modelo_final),
  data           = df_modelo,
  family         = binomial(link = "logit"),
  prior          = prior_coeficientes,
  prior_intercept = prior_intercepto,
  seed           = seed,
  chains         = 4,
  iter           = 2000,
  refresh        = 0,
  control        = list(adapt_delta = 0.95),
  diagnostic_file = file.path(dir_diag, "modelo_final_%i.csv")
)

modelo_final2_bf <- stan_glm(
  formula        = formula(modelo_final2),
  data           = df_modelo,
  family         = binomial(link = "logit"),
  prior          = prior_coeficientes,
  prior_intercept = prior_intercepto,
  seed           = seed,
  chains         = 4,
  iter           = 2000,
  refresh        = 0,
  control        = list(adapt_delta = 0.95),
  diagnostic_file = file.path(dir_diag, "modelo_final2_%i.csv")
)

# Verosimilitud marginal de cada modelo
marginal_modelo_final <- bridge_sampler(modelo_final_bf, silent = TRUE)
marginal_modelo_final2  <- bridge_sampler(modelo_final2_bf,  silent = TRUE)

# Resumen de cada estimacion (incluye error de aproximacion)
summary(marginal_modelo_final)
summary(marginal_modelo_final2)

# Factor de Bayes
BF_final_vs_final2 <- bayes_factor(
  x1 = marginal_modelo_final,
  x2 = marginal_modelo_final2
)

BF_final_vs_final2

# Comparar con escala de Jeffreys / Kass & Raftery
log10(BF_final_vs_final2$bf)