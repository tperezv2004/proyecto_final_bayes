set.seed(123)

# install.packages("VIM")
#install.packages("rstanarm")
library(bayestestR)
library(bayesplot)
library(rstanarm)
library(tidyverse)    
library(lubridate)    
library(VIM)        
library(rstanarm)   
library(loo)


ruta_datos <- "Data/reporte_USA.csv"
df_trabajo <- read_csv(ruta_datos)

#leer cols 
colnames(df_trabajo)
str(df_trabajo)

# la edad minimima que encotre son 8-14 años, eliminaré los mas chicos
min(df_trabajo$Age)
df_trabajo <- df_trabajo %>%
  filter(Age >= 15)

# la maxima fue 1 persona de 100, así que lo dejare en 100
max(df_trabajo$Age)
df_trabajo <- df_trabajo %>%
  filter(!is.na(Age), !is.na(Dots)) %>%
  filter(Age <= 100)

max(df_trabajo$Age)
min(df_trabajo$Age)

# que edad sea un numero entero
df_trabajo <- df_trabajo %>%
  mutate(Age = floor(Age))

# Preguntas propoeustas

# 1 ¿Cuales caracteristicas del atleta actuan como factores predictivos 
#   para alcanzar el nivel de elite en USA?

# para esta separare por sub grupos, pq un hombre y mujer con o sin equipo deben tener una
# diferencia significativa
# por lo que busque "elite" es un 1% pero hare un 5% para poder hacer un mejor analisis 

# la pregunta busca repondre ¿si es o no elite? dependiendo de distintos factores, básicamente 
# resp binaria, por eso se utiliza REGRESIÓN LOGISTICA
# Ahora, cual es la info previa¿? 

df_1 <- df_trabajo %>%
  filter(Sex %in% c("M", "F")) %>%
  select(-c(
    BirthYearClass, Squat1Kg, Squat2Kg, Squat3Kg, Squat4Kg, 
    Bench1Kg, Bench2Kg, Bench3Kg, Deadlift1Kg, Deadlift2Kg, 
    Deadlift4Kg, Best3SquatKg, Best3BenchKg, Place, ParentFederation,
    MeetCountry, MeetState, MeetName, WeightClassKg, State, Deadlift3Kg
  )) %>%
  mutate(
    Equipo = if_else(Equipment == "Raw", "Raw", "Equipado"),
    Sexo = if_else(Sex == "M", 0, 1),
    Tested = if_else(is.na(Tested) | Tested != "Yes", 0L, 1L),
    Year = year(Date),
    Elite = if_else(Dots >= 400, 1L, 0L)      
  )

df_plot_barras <- df_1 %>%
  group_by(Age, Sexo, Equipo) %>%
  summarise(Promedio_Dots = mean(Dots), .groups = "drop")%>%
  mutate(Sexo = factor(Sexo, levels = c(0, 1), labels = c("Hombres", "Mujeres")))

ggplot(df_plot_barras, aes(x = Age, y = Promedio_Dots, fill = Equipo)) +
  geom_col(aes(color = Equipo), alpha = 1, width = 1) +
  scale_x_continuous(breaks = seq(15, 100, by = 5)) +
  facet_grid(Sexo ~ Equipo) +
  theme_minimal() +
  labs(
    title = "Rendimiento Promedio por Rango de Edad",
    subtitle = "Puntaje Dots promedio mediante gráfico de barras",
    x = "Rango de Edad (AgeClass)",
    y = "Puntaje Dots Promedio"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "grey30"),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 80, vjust = 0.5, hjust = 1),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

summary(df_1)
colSums(is.na(df_1))
colnames(df_1)
# 
aggr(df_1, numbers = TRUE, prop = FALSE)

# MODELO LOGISTICO

#hombre raw 

#Elite ~ Bernoulli(p) (si o no)
#logit(p) = ß0 + ß1Age + ß1·Age^2 + ß3·BodyweightKg 

# En stan_glm:
# Prior

df_hombres_raw <- df_1 %>%
  filter(Sex == "M", Equipo == "Raw") %>% # quitar sexo y agregar a lo modelos 
  # ver si es necesraio separar por raw
  slice_sample(n = 15000, replace = FALSE) %>%  # un sample, pq se demora mucho en correr :(
  # SACARLO ANTES DE LA ENTERGA (supongo)
  mutate(
    # estandarizar las var continuas para que la priori tenga sentido
    Age_std  = (Age - mean(Age)) / sd(Age),        
    Bw_std   = (BodyweightKg - mean(BodyweightKg)) / sd(BodyweightKg),
    Year_std = (Year - mean(Year)) / sd(Year)
  )

head(df_hombres_raw)
# no se cual de las dos es mejor, tengo que ver 

n_prior <- normal(0, 2.5)    
t_prior <- student_t(df = 7, location = 0, scale = 2.5)

modelo_1 <- stan_glm(
  Elite ~ Age_std + I(Age_std^2) + Bw_std + Tested + Year_std,      
  data = df_hombres_raw,
  family = binomial(link = "logit"),
  prior = n_prior,
  prior_intercept = n_prior,
  # iter = 12000, warmup = 2000, BF necesita mas muestras, pero no me corre :( 
  seed = 123,
  refresh = 0
)

# Saco Bw_std, pq no aporta nada 
modelo_2 <- stan_glm(
  Elite ~ Age_std + I(Age_std^2) + Tested + Year_std,      
  data = df_hombres_raw,
  family = binomial(link = "logit"),
  prior = n_prior,
  prior_intercept = n_prior,
  seed = 123,
  refresh = 0
) 


modelo_3 <- stan_glm(
  Elite ~ Age_std + I(Age_std^2) + Tested,       
  data = df_hombres_raw,
  family = binomial(link = "logit"),
  prior = n_prior,
  prior_intercept = n_prior,
  seed = 123,
  refresh = 0
)

modelo_4 <- stan_glm(
  Elite ~ Age_std + I(Age_std^2) + Year_std,      
  data = df_hombres_raw,
  family = binomial(link = "logit"),
  prior = n_prior,
  prior_intercept = n_prior,
  seed = 123,
  refresh = 0
)
modelo_5 <- stan_glm(
  Elite ~ Tested + Year_std,      
  data = df_hombres_raw,
  family = binomial(link = "logit"),
  prior = n_prior,
  prior_intercept = n_prior,
  seed = 123,
  refresh = 0
)

for (i in 1:5) {
  print(summary(get(paste0("modelo_", i))))
}
# modelo nulo 
modelo_0 <- update(modelo_2, formula = Elite ~ 1, QR = FALSE, refresh = 0)
loo_0 <- loo(modelo_0)

# Con esto hasta donde entiendo no necesito test y train
loo_1 <- loo(modelo_1)
loo_2 <- loo(modelo_2)
loo_3 <- loo(modelo_3)
loo_4 <- loo(modelo_4)
loo_5 <- loo(modelo_5)
loo_compare(loo_0, loo_1, loo_2, loo_3, loo_4, loo_5)

# porcentaje de Tested en df_hombres_raw
prop_tested <- mean(df_hombres_raw$Tested)
prop_tested

#graficos
edades <- seq(15, 70, by = 1)
edad_media <- mean(df_hombres_raw$Age)
edad_sd <- sd(df_hombres_raw$Age)
datos_pred <- data.frame(
  Age_std  = (edades - edad_media) / edad_sd,
  Tested   = 0L,
  Year_std = 0
)
pred_post <- posterior_epred(modelo_2, newdata = datos_pred)
df_curva <- data.frame(
  Edad = edades,
  Media = apply(pred_post, 2, mean),
  Q10   = apply(pred_post, 2, quantile, 0.10),
  Q90   = apply(pred_post, 2, quantile, 0.90)
)

# Graficar
ggplot(df_curva, aes(x = Edad, y = Media)) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90), alpha = 0.3, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1.2) +
  labs(
    title = "Probabilidad de ser Elite según Edad",
    subtitle = "Hombres Raw | Intervalo de credibilidad 80%",
    x = "Edad",
    y = "P(Elite)"
  ) +
  theme_minimal()

pp_check(modelo_2, plotfun = "bars", nreps = 100) +
  labs(title = "Posterior Predictive Check",
       subtitle = "Datos observados (oscuro) vs simulados (claro)") +
  theme_minimal()

#
out_new <- as.array(modelo_2)
mcmc_trace(out_new, 
           pars = c("(Intercept)", "Age_std", "I(Age_std^2)", "Tested", "Year_std")) +
  labs(title = "Traceplot - Modelo 2 Hombres Raw") +
  theme_minimal()

#
plot(modelo_2,"acf_bar")

#
pplot<-plot(modelo_2 , "areas", prob = 0.95, prob_outer = 1)
pplot+ geom_vline(xintercept = 0)

#
pp_check(modelo_2, "stat_2d", nreps = 100)


# 2 ¿Cual es la relacion entre la edad, el peso corporal y el puntaje 
#   DOTS en atletas de powerlifting en USA?

#prop extra
# 3 ¿Cuál es la probabilidad de alcanzar elite según la edad, 
#   controlando por la relación no lineal entre edad y peso corporal?
#   ¿Cuáles características del atleta predicen el nivel de consistency 
#   en rendimiento (variabilidad entre intentos)?


# Agregar preguntas al modelo de regresión logística, si es elite o no 
# hacer BF para los modelos si o si. 







#ref: 
# https://statswithr.github.io/book/
# modelo logistico
# https://rpubs.com/BayesianN/claims