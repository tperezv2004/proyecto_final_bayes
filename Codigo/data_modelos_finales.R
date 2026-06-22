# --------------------------------------------------------
# 1. LIBRERIAS
# --------------------------------------------------------
set.seed(123)
library(tidyverse)
library(lubridate)

options(mc.cores = parallel::detectCores()) # no tocar
# --------------------------------------------------------
# 2. CARGAR BASE DE DATOS
# --------------------------------------------------------
ruta_datos <- "../Data/reporte_USA.csv"
df_trabajo <- read_csv(ruta_datos)

# Top 5% es el umbral elite definido 
umbral_elite <- quantile(df_trabajo$Dots, 0.95, na.rm = TRUE)
# --------------------------------------------------------
# CARGAR BASE DE DATOS
# --------------------------------------------------------
ruta_datos <- "../Data/reporte_USA.csv"
df_trabajo <- read_csv(ruta_datos)

# --------------------------------------------------------
# Prep Datos Finales
# --------------------------------------------------------

umbral_elite <- quantile(df_trabajo$Dots, 0.95, na.rm = TRUE)
df_modelos_finales <- df_trabajo %>%
  # columnas que no tienen un aporte significativo para lo que buscamos predecir
  select(-c(
    BirthYearClass, Squat1Kg, Squat2Kg, Squat3Kg, Squat4Kg,
    Bench1Kg, Bench2Kg, Bench3Kg, Bench4Kg, Deadlift1Kg, Deadlift2Kg,
    Deadlift4Kg, Best3SquatKg, Best3BenchKg, Place, ParentFederation,
    MeetCountry, MeetState, MeetName, WeightClassKg, State, Deadlift3Kg,
    Division, Goodlift, Best3DeadliftKg, Wilks, Country, Federation, Event
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
    Equipo = factor(Equipo, levels = c("Raw", "Equipado")),
    
    # Tested en sus dos nombres, por si algún script usa uno u otro
    Tested_bin = if_else(is.na(Tested) | Tested != "Yes", 0L, 1L),
    Tested     = Tested_bin,
    
    # Sexo en sus dos formas
    Sex   = factor(Sex, levels = c("M", "F")),
    Sexo  = if_else(Sex == "M", 0L, 1L)
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
    Dots > 10) 
  
dim(df_modelos_finales)
colnames(df_modelos_finales)

# info varianle Elite (a estimar)
df_modelos_finales %>%
  count(Elite_texto, sort = TRUE)
df_modelos_finales %>%
  mutate(Elite_texto = factor(Elite_texto, levels = c("No elite", "Elite"))) %>%
  count(Sex, Equipo, Elite_texto) %>%
  group_by(Sex, Equipo) %>%
  mutate(porcentaje = n / sum(n) * 100) %>%
  ungroup()
df_modelos_finales %>%
  summarise(
    proporcion_elite = mean(Elite),
    porcentaje_elite = mean(Elite) * 100
  )


n_muestra_modelo <- min(10000, nrow(df_modelos_finales))
df_modelos_finales <- df_modelos_finales %>%
  slice_sample(n = n_muestra_modelo) %>%
  mutate(
    Age_std = as.numeric(scale(Age)),
    Bw_std = as.numeric(scale(BodyweightKg)),
    Year_std = as.numeric(scale(Year))
  )

dim(df_modelos_finales)

df_modelos_finales %>%
  count(Elite_texto)

df_modelos_finales %>%
  count(Sex, Equipo)

head(df_modelos_finales)
names(df_modelos_finales)


# guardar df_modelos_finales para usarlo en los scripts de modelos
# como csv
write_csv(df_modelos_finales, "../Data/df_modelos_finales.csv")



