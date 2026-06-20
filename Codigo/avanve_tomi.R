# --------------------------------------------------------
# --------------------------------------------------------
# 1. LIBRERIAS
# --------------------------------------------------------
# --------------------------------------------------------

set.seed(123)

library(tidyverse)
library(lubridate)
library(rstanarm)
library(bayesplot)
library(bayestestR)
library(loo)
library(VIM)
library(pROC)

options(mc.cores = parallel::detectCores()) # no tocar


# --------------------------------------------------------
# --------------------------------------------------------
# 2. CARGAR BASE DE DATOS
# --------------------------------------------------------
# --------------------------------------------------------

ruta_datos <- "../Data/reporte_USA.csv"
df_trabajo <- read_csv(ruta_datos)


# --------------------------------------------------------
# --------------------------------------------------------
# 3. REVISION INICIAL
# --------------------------------------------------------
# --------------------------------------------------------

dim(df_trabajo)
head(df_trabajo)
names(df_trabajo)
str(df_trabajo)
summary(df_trabajo)


# --------------------------------------------------------
# --------------------------------------------------------
# 4. REVISION DE VARIABLES IMPORTANTES
# --------------------------------------------------------
# --------------------------------------------------------

df_trabajo %>%
  count(Sex)

df_trabajo %>%
  count(Equipment, sort = TRUE)

df_trabajo %>%
  count(Tested, sort = TRUE)

df_trabajo %>%
  summarise(
    edad_minima = min(Age, na.rm = TRUE),
    edad_maxima = max(Age, na.rm = TRUE),
    peso_minimo = min(BodyweightKg, na.rm = TRUE),
    peso_maximo = max(BodyweightKg, na.rm = TRUE),
    dots_minimo = min(Dots, na.rm = TRUE),
    dots_maximo = max(Dots, na.rm = TRUE)
  )


# --------------------------------------------------------
# --------------------------------------------------------
# 5. PREPARAR DATOS PARA LA PREGUNTA 
# --------------------------------------------------------
# --------------------------------------------------------

# Pregunta
# Cuales caracteristicas del atleta actuan como factores predictivos
# para alcanzar el nivel de elite en USA?

# La variable respuesta sera binaria: # podemos jugar con esto 
# Elite = 1 si Dots >= P_80
# Elite = 0 si Dots < P_80

# Importante:
# Dots NO se usa como predictor, porque Elite se construye desde Dots
# Si usamos Dots como predictor seria "circular"

umbral_elite <- quantile(df_trabajo$Dots, 0.80, na.rm = TRUE)

df_modelo_pre <- df_trabajo %>%
  # Quite las columans que la porfe dijo que no eran muy utiles
  select(-c(
    BirthYearClass, Squat1Kg, Squat2Kg, Squat3Kg, Squat4Kg, 
    Bench1Kg, Bench2Kg, Bench3Kg, Bench4Kg,Deadlift1Kg, Deadlift2Kg, 
    Deadlift4Kg, Best3SquatKg, Best3BenchKg, Place, ParentFederation,
    MeetCountry, MeetState, MeetName, WeightClassKg, State, Deadlift3Kg
  )) %>%
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
    
    Tested_bin = if_else(is.na(Tested) | Tested != "Yes", 0L, 1L),
    
    Sex = factor(Sex, levels = c("M", "F")),
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
    Age <= 70,
    BodyweightKg >= 35,
    BodyweightKg <= 200,
    Dots > 10
  )

dim(df_modelo_pre)
colnames(df_modelo_pre)


# --------------------------------------------------------
# --------------------------------------------------------
# 6. REVISION DE LA VARIABLE ELITE
# --------------------------------------------------------
# --------------------------------------------------------

df_modelo_pre %>%
  count(Elite_texto, sort = TRUE)

df_modelo_pre %>%
  mutate(Elite_texto = factor(Elite_texto, levels = c("No elite", "Elite"))) %>%
  count(Sex, Equipo, Elite_texto) %>%
  group_by(Sex, Equipo) %>%
  mutate(porcentaje = n / sum(n) * 100) %>%
  ungroup()

df_modelo_pre %>%
  summarise(
    proporcion_elite = mean(Elite),
    porcentaje_elite = mean(Elite) * 100
  )

names(df_modelo_pre)

# --------------------------------------------------------
# --------------------------------------------------------
# 7. REVISION DE DATOS FALTANTES
# --------------------------------------------------------
# --------------------------------------------------------

# Creo q lo podemos borrar
colSums(is.na(df_modelo_pre))
aggr(df_modelo_pre, numbers = TRUE, prop = FALSE)


# --------------------------------------------------------
# --------------------------------------------------------
# 8. CREAR BASE FINAL PARA MODELAR
# --------------------------------------------------------
# --------------------------------------------------------

# Si el modelo se demora mucho, se puede bajar

n_muestra_modelo <- min(10000, nrow(df_modelo_pre))

df_modelo <- df_modelo_pre %>%
  slice_sample(n = n_muestra_modelo) %>%
  mutate(
    Age_std = as.numeric(scale(Age)),
    Bw_std = as.numeric(scale(BodyweightKg)),
    Year_std = as.numeric(scale(Year))
  )

dim(df_modelo)

df_modelo %>%
  count(Elite_texto)

df_modelo %>%
  count(Sex, Equipo)


# --------------------------------------------------------
# --------------------------------------------------------
# 9. GRAFICO DESCRIPTIVO DE ELITE
# --------------------------------------------------------
# --------------------------------------------------------

df_plot_elite <- df_modelo %>%
  mutate(
    RangoEdad = cut(
      Age,
      breaks = seq(15, 75, by = 5),
      right = FALSE
    )
  ) %>%
  group_by(RangoEdad, Sex, Equipo) %>%
  summarise(
    n = n(),
    Porcentaje_Elite = mean(Elite) * 100,
    .groups = "drop"
  ) %>%
  filter(n >= 20)

grafico_elite_edad <- ggplot(
  df_plot_elite,
  aes(x = RangoEdad, y = Porcentaje_Elite, fill = Sex)  
) +
  geom_col() + 
  facet_grid(Sex ~ Equipo, scales = "free_x") + 
  scale_fill_manual(
    values = c("F" = "#FF6EB4", "M" = "#87CEFA")  
  ) +
  labs(
    title = "Porcentaje de atletas Elite por edad",
    subtitle = "Clasificacion Elite definida como el 20% superior de DOTS",
    x = "Rango de edad",
    y = "Porcentaje Elite"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7),
    legend.position = "none"
  )

grafico_elite_edad


# --------------------------------------------------------
# --------------------------------------------------------
# 10. DEFINIR PRIORIS
# --------------------------------------------------------
# --------------------------------------------------------

# Como las variables continuas fueron estandarizadas
# una priori normal(0, 2.5) es razonable para los coeficientes
# podriamos cambiar los parametros, pero segun yo estan bien

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
# 11. MODELOS BAYESIANOS LOGISTICOS
# --------------------------------------------------------
# --------------------------------------------------------

# Modelo nulo:
# No usa caracteristicas del atleta.

modelo_0 <- stan_glm(
  Elite ~ 1,
  data = df_modelo,
  family = binomial(link = "logit"),
  prior_intercept = prior_intercepto,
  seed = 123,
  chains = 4,
  iter = 2000,
  refresh = 0
)


# Modelo 1:
# Solo variables fisicas.

modelo_1 <- stan_glm(
  Elite ~ Age_std + I(Age_std^2) + Bw_std,
  data = df_modelo,
  family = binomial(link = "logit"),
  prior = prior_coeficientes,
  prior_intercept = prior_intercepto,
  seed = 123,
  chains = 4,
  iter = 2000,
  refresh = 0
)


# Modelo 2:
# Variables fisicas + sexo + tipo de equipo.

modelo_2 <- stan_glm(
  Elite ~ Age_std + I(Age_std^2) + Bw_std + Sex + Equipo,
  data = df_modelo,
  family = binomial(link = "logit"),
  prior = prior_coeficientes,
  prior_intercept = prior_intercepto,
  seed = 123,
  chains = 4,
  iter = 2000,
  refresh = 0
)


# Modelo 3:
# Variables fisicas + sexo + tipo de equipo + testeo + año.

modelo_3 <- stan_glm(
  Elite ~ Age_std + I(Age_std^2) + Bw_std + Sex + Equipo + Tested_bin + Year_std,
  data = df_modelo,
  family = binomial(link = "logit"),
  prior = prior_coeficientes,
  prior_intercept = prior_intercepto,
  seed = 123,
  chains = 4,
  iter = 2000,
  refresh = 0
)


# Modelo 4:
# Modelo con interaccion entre sexo y tipo de equipo.

modelo_4 <- stan_glm(
  Elite ~ Age_std + I(Age_std^2) + Bw_std + Sex * Equipo + Tested_bin + Year_std,
  data = df_modelo,
  family = binomial(link = "logit"),
  prior = prior_coeficientes,
  prior_intercept = prior_intercepto,
  seed = 123,
  chains = 4,
  iter = 2000,
  refresh = 0
)


# --------------------------------------------------------
# --------------------------------------------------------
# 12. RESUMEN DE LOS MODELOS
# --------------------------------------------------------
# --------------------------------------------------------

summary(modelo_0)
summary(modelo_1)
summary(modelo_2)
summary(modelo_3)
summary(modelo_4)


# --------------------------------------------------------
# --------------------------------------------------------
# 13. COMPARACION DE MODELOS CON LOO
# --------------------------------------------------------
# --------------------------------------------------------

# LOO permite comparar modelos segun capacidad predictiva.
# El mejor modelo aparece primero en loo_compare.

loo_0 <- loo(modelo_0)
loo_1 <- loo(modelo_1)
loo_2 <- loo(modelo_2)
loo_3 <- loo(modelo_3)
loo_4 <- loo(modelo_4)

comparacion_loo <- loo_compare(
  loo_0,
  loo_1,
  loo_2,
  loo_3,
  loo_4
)

comparacion_loo


# --------------------------------------------------------
# --------------------------------------------------------
# 14. ELEGIR MODELO FINAL
# --------------------------------------------------------
# --------------------------------------------------------

nombre_mejor_modelo <- rownames(comparacion_loo)[1]
modelo_final <- get(nombre_mejor_modelo)

nombre_mejor_modelo
summary(modelo_final)


# --------------------------------------------------------
# --------------------------------------------------------
# 15. RESUMEN POSTERIOR DE LOS COEFICIENTES
# --------------------------------------------------------
# --------------------------------------------------------

# Resumen de los coeficientes en escala logit.

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

resumen_coeficientes


# --------------------------------------------------------
# --------------------------------------------------------
# 16. ODDS RATIOS
# --------------------------------------------------------
# --------------------------------------------------------

# Los odds ratios son exp(beta).
# Si OR > 1 aumenta la chance de ser Elite.
# Si OR < 1 disminuye la chance de ser Elite.

resumen_odds_ratios <- resumen_coeficientes %>%
  mutate(
    OR_Media = exp(Media),
    OR_Q025 = exp(Q025),
    OR_Q975 = exp(Q975)
  )

resumen_odds_ratios


# --------------------------------------------------------
# --------------------------------------------------------
# 17. GRAFICO DE EFECTOS POSTERIORES
# --------------------------------------------------------
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
    subtitle = "Modelo logistico bayesiano para probabilidad de ser Elite"
  ) +
  theme_minimal()

grafico_efectos


# --------------------------------------------------------
# --------------------------------------------------------
# 18. PREDICCION POSTERIOR SEGUN EDAD
# --------------------------------------------------------
# --------------------------------------------------------

# se calcula la probabilidad posterior de ser Elite segun edad,
# separando por sexo y tipo de equipo.

edades <- seq(15, 70, by = 1)

edad_media <- mean(df_modelo$Age)
edad_sd <- sd(df_modelo$Age)

datos_pred_edad <- expand_grid(
  Edad = edades,
  Sex = factor(c("M", "F"), levels = levels(df_modelo$Sex)),
  Equipo = factor(c("Raw", "Equipado"), levels = levels(df_modelo$Equipo))
) %>%
  mutate(
    Age_std = (Edad - edad_media) / edad_sd,
    Bw_std = 0,
    Tested_bin = 0L,
    Year_std = 0
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
  aes(x = Edad, y = Media, ymin = Q05, ymax = Q95, color = Equipo, fill = Equipo)
) +
  geom_ribbon(alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ Sex) +
  labs(
    title = "Probabilidad posterior de ser Elite segun edad",
    subtitle = "Intervalo creible posterior del 90%",
    x = "Edad",
    y = "P(Elite)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(face = "bold")
  )

grafico_prob_edad


# --------------------------------------------------------
# --------------------------------------------------------
# 19. PREDICCION POSTERIOR SEGUN PESO CORPORAL
# --------------------------------------------------------
# --------------------------------------------------------

# Se calcula la probabilidad posterior de ser Elite segun peso corporal,
# fijando edad promedio, Tested = 0 y Year promedio.

pesos <- seq(
  from = quantile(df_modelo$BodyweightKg, 0.05),
  to = quantile(df_modelo$BodyweightKg, 0.95),
  by = 1
)

peso_media <- mean(df_modelo$BodyweightKg)
peso_sd <- sd(df_modelo$BodyweightKg)

datos_pred_peso <- expand_grid(
  Peso = pesos,
  Sex = factor(c("M", "F"), levels = levels(df_modelo$Sex)),
  Equipo = factor(c("Raw", "Equipado"), levels = levels(df_modelo$Equipo))
) %>%
  mutate(
    Age_std = 0,
    Bw_std = (Peso - peso_media) / peso_sd,
    Tested_bin = 0L,
    Year_std = 0
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
  aes(x = Peso, y = Media, ymin = Q05, ymax = Q95, color = Equipo, fill = Equipo)
) +
  geom_ribbon(alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ Sex) +
  labs(
    title = "Probabilidad posterior de ser Elite segun peso corporal",
    subtitle = "Intervalo creible posterior del 90%",
    x = "Peso corporal (kg)",
    y = "P(Elite)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(face = "bold")
  )

grafico_prob_peso


# --------------------------------------------------------
# --------------------------------------------------------
# 20. POSTERIOR PREDICTIVE CHECK
# --------------------------------------------------------
# --------------------------------------------------------

# compara los datos observados con datos simulados
# desde el modelo posterior.

grafico_pp_check <- pp_check(
  modelo_final,
  plotfun = "bars",
  nreps = 100
) +
  labs(
    title = "Posterior Predictive Check",
    subtitle = "Datos observados versus datos simulados desde el modelo"
  ) +
  theme_minimal()

grafico_pp_check


# --------------------------------------------------------
# --------------------------------------------------------
# 21. DIAGNOSTICOS MCMC
# --------------------------------------------------------
# --------------------------------------------------------

# Traceplot para revisar mezcla de las cadenas.

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


# Autocorrelacion de las cadenas.

plot(modelo_final, "acf_bar")


# --------------------------------------------------------
# --------------------------------------------------------
# 21. CURVA ROC Y AUC
# --------------------------------------------------------
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
