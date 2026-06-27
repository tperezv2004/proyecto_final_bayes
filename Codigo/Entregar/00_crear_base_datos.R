###################
#   NO SE SUBE    #
####################
# --------------------------------------------------------
# --------------------------------------------------------
# 1. LIBRERIAS
# --------------------------------------------------------
# --------------------------------------------------------

set.seed(123)

library(tidyverse)
library(lubridate)
library(VIM)

options(mc.cores = parallel::detectCores()) # no tocar


# --------------------------------------------------------
# --------------------------------------------------------
# 2. CARGAR BASE DE DATOS
# --------------------------------------------------------
# --------------------------------------------------------

ruta_datos <- "../../Data/reporte_USA.csv"
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

# Pregunta a responder
# Cuales caracteristicas del atleta actuan como factores predictivos
# para alcanzar el nivel de elite en USA?

# La variable respuesta sera binaria: # podemos jugar con esto 
# Elite = 1 si Dots >= P_95
# Elite = 0 si Dots < P_95

# Importante:
# Dots NO se usa como predictor, porque Elite se construye desde Dots
# Si usamos Dots como predictor seria "circular"

df_modelo_pre <- df_trabajo %>%
  select(-any_of(c(
    "BirthYearClass", "Squat1Kg", "Squat2Kg", "Squat3Kg", "Squat4Kg", 
    "Bench1Kg", "Bench2Kg", "Bench3Kg", "Bench4Kg", "Deadlift1Kg", "Deadlift2Kg", 
    "Deadlift4Kg", "Best3SquatKg", "Best3BenchKg", "Place", "ParentFederation",
    "MeetCountry", "MeetState", "MeetName", "WeightClassKg", "State", "Deadlift3Kg",
    "Division", "Goodlift", "Best3DeadliftKg"
  ))) %>%
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
    Event == "SBD",
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

#Vamos a intentar probar con 95% y 80%
percentil_elite = 0.95
# Ahora el umbral se calcula sobre la base final filtrada
umbral_elite <- quantile(df_modelo_pre$Dots, percentil_elite, na.rm = TRUE)

df_modelo_pre <- df_modelo_pre %>%
  mutate(
    Elite = if_else(Dots >= umbral_elite, 1L, 0L),
    Elite_texto = if_else(Elite == 1, "Elite", "No elite")
  )



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

n_muestra_modelo <- nrow(df_modelo_pre)

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
# 9. GUARDAR BASE FINAL PARA MODELAR
# --------------------------------------------------------
# --------------------------------------------------------

write_csv(
  df_modelo,
  "base_modelo_USA.csv"
)

