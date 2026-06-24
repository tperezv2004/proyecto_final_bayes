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
    MeetCountry, MeetState, MeetName, WeightClassKg, State, Deadlift3Kg,
    Division, Goodlift, Best3DeadliftKg
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
    title = "Porcentaje de atletas Elite por edad y sexo",
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

df_plot_elite <- df_modelo %>%
  mutate(
    RangoEdad = cut(
      Age,
      breaks = seq(15, 85, by = 5), 
      right = FALSE
    ),
    Sexo_Label = if_else(Sex == "M", "Hombre", "Mujer")
  ) %>%
  group_by(RangoEdad, Sexo_Label, Equipo) %>%
  summarise(
    n = n(),
    Porcentaje_Elite = mean(Elite) * 100,
    .groups = "drop"
  ) %>%
  filter(n >= 20)

grafico_elite_edad_2 <- ggplot(
  df_plot_elite,
  aes(x = RangoEdad, y = Porcentaje_Elite, fill = Sexo_Label)  
) +
  geom_col(position = position_dodge(width = 0.9)) + 
  facet_wrap(~ Equipo) + 
  scale_fill_manual(
    name = "Sexo",
    values = c("Hombre" = "#63B8FF", "Mujer" = "#FF6EB4")  
  ) +
  labs(
    title = "Porcentaje de atletas Elite por edad, sin separar subgrupos",
    subtitle = "Elite = Percentil 95 en DOTS | Sexo como predictor en el modelo",
    x = "Rango de edad",
    y = "Porcentaje Elite"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 9),
    legend.position = "bottom", 
    legend.title = element_text(face = "bold")
  )
grafico_elite_edad_2

# --------------------------------------------------------
# GRAFICO DESCRIPTIVO: DENSIDAD DE PESO CORPORAL
# --------------------------------------------------------

grafico_peso_elite <- ggplot(
  df_modelo,
  aes(x = BodyweightKg, fill = Elite_texto)
) +
  geom_density(alpha = 0.6, color = "white") + 
  # se lo sacamos se ve la dencidad "completa" 
  facet_wrap(
    ~ Sex, 
    labeller = as_labeller(c("F" = "Mujeres", "M" = "Hombres"))
  ) +
  scale_fill_manual(
    name = "Nivel del Atleta",
    values = c("Elite" = "#FFC125", "No elite" = "#A2B5CD") 
  ) +
  labs(
    title = "Distribución del peso corporal según nivel del atleta",
    subtitle = "Comparación de densidad entre atletas Elite y No Elite",
    x = "Peso Corporal (kg)",
    y = "Densidad"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )
grafico_peso_elite

# GRAFICO DESCRIPTIVO: EVOLUCION TEMPORAL (YEAR)
df_plot_year <- df_modelo %>%
  group_by(Year, Equipo) %>% 
  summarise(
    n = n(),
    Porcentaje_Elite = mean(Elite) * 100,
    .groups = "drop"
  ) %>%
  filter(n >= 20) 

grafico_tendencia_year <- ggplot(
  df_plot_year,
  aes(x = Year, y = Porcentaje_Elite, color = Equipo, group = Equipo)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(
    name = "Equipamiento",
    values = c("Raw" = "#4CAF50", "Equipado" = "#FF9800")
  ) +
  scale_x_continuous(breaks = seq(min(df_plot_year$Year), max(df_plot_year$Year), by = 2)) +
  labs(
    title = "Evolución de atletas Elite a lo largo del tiempo",
    subtitle = "Porcentaje de clasificación Elite por año y tipo de equipamiento",
    x = "Año de competencia",
    y = "Porcentaje Elite"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )
grafico_tendencia_year

# --------------------------------------------------------
# GRAFICO DESCRIPTIVO: CONTROL ANTIDOPAJE (TESTED)
# --------------------------------------------------------

df_plot_tested <- df_modelo %>%
  mutate(
    Control_Dopaje = if_else(Tested_bin == 1, "Con testeo antidopaje", "Sin testeo (o no reportado)")
  ) %>%
  group_by(Equipo, Control_Dopaje) %>% 
  summarise(
    n = n(),
    Porcentaje_Elite = mean(Elite) * 100,
    .groups = "drop"
  ) %>%
  filter(n >= 20)

grafico_dopaje <- ggplot(
  df_plot_tested,
  aes(x = Equipo, y = Porcentaje_Elite, fill = Control_Dopaje)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = paste0(round(Porcentaje_Elite, 1), "%")),
    position = position_dodge(width = 0.7),
    vjust = -0.8,
    size = 4,
    fontface = "bold",
    color = "black"
  ) +
  scale_fill_manual(
    name = "Estatus de la Competencia",
    values = c("Con testeo antidopaje" = "#5DADE2", "Sin testeo (o no reportado)" = "#E74C3C")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Proporción de atletas Elite según control antidopaje",
    subtitle = "Comparación entre competencias testeadas y no testeadas por equipamiento",
    x = "Tipo de Equipamiento",
    y = "Porcentaje Elite"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )
grafico_dopaje

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
# 13. DIAGNOSTICOS DE LOS MODELOS
# --------------------------------------------------------
# --------------------------------------------------------

diagnosticos<- function(modelo, n_modelo) {
  nombre_modelo <- paste("Modelo", n_modelo)
  cat("\n--- Diagnósticos del", paste("Modelo", n_modelo), "---\n")
  
  
  # 1. Residuos de deviance
  residuos_deviance <- residuals(modelo, type = "deviance")
  plot(
    residuos_deviance,
    main = paste("Residuos de deviance del", nombre_modelo),
    ylab = "Residuos de deviance",
    xlab = "Índice de observación"
  )
  
  
  # 2. Convergencia MCMC
  bayesplot::mcmc_trace(as.array(modelo))
  
  
  # 3. Validación Predictiva Global (PPC)
  p <- bayesplot::pp_check(modelo, plotfun = "stat", stat = "mean") +
    ggplot2::labs(title = paste("PP check (media) -", nombre_modelo))
  print(p)
  invisible(p)
  
  
  # 4. Verificación de Linealidad Estructural (Binned Residuals)
  residuos_agrupados <- binned_residuals(modelo, n_bins = 10)
  plot(residuos_agrupados, main = paste("Residuos agrupados del", nombre_modelo))
  residuos_agrupados
  
  
  # 5. Análisis de Multicolinealidad (VIF)
  if (n_modelo > 1) { #No aplica al modelo 0 ni 1 
    colinealidad <- check_collinearity(modelo)
    print(colinealidad)
    plot(colinealidad, main = paste("VIF del", paste("Modelo", n_modelo)))
  }
  
  
  # 6. Identificación de Observaciones Influyentes (PSIS-LOO)
  objeto_loo <- loo(modelo)
  cat("\n--- LOO:", nombre_modelo, "---\n")
  print(objeto_loo)
  plot(objeto_loo, main = paste("LOO -", nombre_modelo))
  
  # 7. Detección Visual de Sesgos Sistemáticos (LOESS)
  df_diagnostico <- data.frame(
    valores_ajustados = fitted(modelo),
    residuos_deviance  = residuos_deviance,
    real_elite         = as.factor(df_modelo$Elite)
  )
  
  p <- ggplot(df_diagnostico, aes(x = valores_ajustados, y = residuos_deviance)) +
    geom_point(aes(color = real_elite), alpha = 0.5, size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_smooth(method = "loess", se = FALSE, color = "darkred", linewidth = 1) +
    labs(
      title = "Residuos vs. Valores Ajustados",
      subtitle = paste("Modelo Logístico Bayesiano (", nombre_modelo, ")"),
      x = "Probabilidad Ajustada (Valores Predichos)",
      y = "Residuos de Deviance",
      color = "Clase Real (Elite)"
    ) +
    theme_minimal()
  
  print(p)
  invisible(p)
  
  objeto_loo
}

loo_0 <- diagnosticos(modelo_0, 0)
loo_1 <- diagnosticos(modelo_1, 1)
loo_2 <- diagnosticos(modelo_2, 2)
loo_3 <- diagnosticos(modelo_3, 3)
loo_4 <- diagnosticos(modelo_4, 4)

# --------------------------------------------------------
# --------------------------------------------------------
# 14. COMPARACION DE MODELOS CON LOO
# --------------------------------------------------------
# --------------------------------------------------------

# LOO permite comparar modelos segun capacidad predictiva.
# El mejor modelo aparece primero en loo_compare.
comparacion_loo <- loo_compare(
  list(
    "modelo_0" = loo_0,
    "modelo_1" = loo_1,
    "modelo_2" = loo_2,
    "modelo_3" = loo_3,
    "modelo_4" = loo_4
  )
)

comparacion_loo
# --------------------------------------------------------
# --------------------------------------------------------
# 15. ELEGIR MODELO FINAL
# --------------------------------------------------------
# --------------------------------------------------------

nombre_mejor_modelo <- rownames(comparacion_loo)[1]
nombre_mejor_modelo
modelo_final <- get(nombre_mejor_modelo)

nombre_mejor_modelo
summary(modelo_final)


# --------------------------------------------------------
# --------------------------------------------------------
# 16. RESUMEN POSTERIOR DE LOS COEFICIENTES
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
# 17. ODDS RATIOS
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
# 18. GRAFICO DE EFECTOS POSTERIORES
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
# 19. PREDICCION POSTERIOR SEGUN EDAD
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
# 20. PREDICCION POSTERIOR SEGUN PESO CORPORAL
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
# 21. POSTERIOR PREDICTIVE CHECK
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
# 22. DIAGNOSTICOS MCMC
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
# 23. CURVA ROC Y AUC
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
