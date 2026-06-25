
# ANALISIS DESCRIPTIVO

# --------------------------------------------------------
# 1. LIBRERIAS
# --------------------------------------------------------
# --------------------------------------------------------

set.seed(123)

library(tidyverse)
library(lubridate)


# --------------------------------------------------------
# --------------------------------------------------------
# 2. CARGAR BASE LIMPIA
# --------------------------------------------------------
# --------------------------------------------------------

ruta_base <- "base_modelo_USA.csv"

df_modelo <- read_csv(ruta_base) %>%
  mutate(
    Sex = factor(Sex, levels = c("M", "F")),
    Equipo = factor(Equipo, levels = c("Raw", "Equipado")),
    Elite_texto = factor(Elite_texto, levels = c("No elite", "Elite"))
  )


# --------------------------------------------------------
# --------------------------------------------------------
# 3. INFORMACION GENERAL DE LA BASE
# --------------------------------------------------------
# --------------------------------------------------------

dim(df_modelo)

glimpse(df_modelo)

summary(df_modelo)

df_modelo %>%
  count(Sex)

df_modelo %>%
  count(Equipment, sort = TRUE)

df_modelo %>%
  count(Equipo, sort = TRUE)

df_modelo %>%
  count(Tested_bin, sort = TRUE)

df_modelo %>%
  count(Elite_texto, sort = TRUE)

df_trabajo %>%
  count(Event, sort = TRUE)

df_modelo %>%
  summarise(
    n = n(),
    edad_minima = min(Age, na.rm = TRUE),
    edad_maxima = max(Age, na.rm = TRUE),
    peso_minimo = min(BodyweightKg, na.rm = TRUE),
    peso_maximo = max(BodyweightKg, na.rm = TRUE),
    dots_minimo = min(Dots, na.rm = TRUE),
    dots_maximo = max(Dots, na.rm = TRUE),
    porcentaje_elite = mean(Elite, na.rm = TRUE) * 100
  )


# --------------------------------------------------------
# --------------------------------------------------------
# 4. REVISION DE LA VARIABLE ELITE
# --------------------------------------------------------
# --------------------------------------------------------

df_modelo %>%
  count(Elite_texto, sort = TRUE)

df_modelo %>%
  count(Sex, Equipo, Elite_texto) %>%
  group_by(Sex, Equipo) %>%
  mutate(porcentaje = n / sum(n) * 100) %>%
  ungroup()

df_modelo %>%
  summarise(
    proporcion_elite = mean(Elite, na.rm = TRUE),
    porcentaje_elite = mean(Elite, na.rm = TRUE) * 100
  )


# --------------------------------------------------------
# --------------------------------------------------------
# 5. DATOS FALTANTES
# --------------------------------------------------------
# --------------------------------------------------------

colSums(is.na(df_modelo))


# --------------------------------------------------------
# --------------------------------------------------------
# 6. GRAFICO: PORCENTAJE ELITE POR EDAD, SEXO Y EQUIPO
# --------------------------------------------------------
# --------------------------------------------------------

df_plot_elite_edad <- df_modelo %>%
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
    Porcentaje_Elite = mean(Elite, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  filter(n >= 20)

grafico_elite_edad <- ggplot(
  df_plot_elite_edad,
  aes(x = RangoEdad, y = Porcentaje_Elite, fill = Sex)
) +
  geom_col() +
  facet_grid(Sex ~ Equipo, scales = "free_x") +
  scale_fill_manual(
    values = c("F" = "#FF6EB4", "M" = "#87CEFA")
  ) +
  labs(
    title = "Porcentaje de atletas Elite por edad y sexo",
    subtitle = "Clasificación Elite definida como el 20% superior de DOTS",
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
# 7. GRAFICO: PORCENTAJE ELITE POR EDAD, SIN SEPARAR SEXO
# --------------------------------------------------------
# --------------------------------------------------------

df_plot_elite_edad_2 <- df_modelo %>%
  mutate(
    RangoEdad = cut(
      Age,
      breaks = seq(15, 75, by = 5),
      right = FALSE
    ),
    Sexo_Label = if_else(Sex == "M", "Hombre", "Mujer")
  ) %>%
  group_by(RangoEdad, Sexo_Label, Equipo) %>%
  summarise(
    n = n(),
    Porcentaje_Elite = mean(Elite, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  filter(n >= 20)

grafico_elite_edad_2 <- ggplot(
  df_plot_elite_edad_2,
  aes(x = RangoEdad, y = Porcentaje_Elite, fill = Sexo_Label)
) +
  geom_col(position = position_dodge(width = 0.9)) +
  facet_wrap(~ Equipo) +
  scale_fill_manual(
    name = "Sexo",
    values = c("Hombre" = "#63B8FF", "Mujer" = "#FF6EB4")
  ) +
  labs(
    title = "Porcentaje de atletas Elite por edad",
    subtitle = "Elite = 20% superior en DOTS | Sexo como predictor en el modelo",
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
# --------------------------------------------------------
# 8. GRAFICO: DENSIDAD DE PESO CORPORAL SEGUN NIVEL
# --------------------------------------------------------
# --------------------------------------------------------

grafico_peso_elite <- ggplot(
  df_modelo,
  aes(x = BodyweightKg, fill = Elite_texto)
) +
  geom_density(alpha = 0.6, color = "white") +
  facet_wrap(
    ~ Sex,
    labeller = as_labeller(c("F" = "Mujeres", "M" = "Hombres"))
  ) +
  scale_fill_manual(
    name = "Nivel del atleta",
    values = c("Elite" = "#FFC125", "No elite" = "#A2B5CD")
  ) +
  labs(
    title = "Distribución del peso corporal según nivel del atleta",
    subtitle = "Comparación entre atletas Elite y No Elite",
    x = "Peso corporal (kg)",
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


# --------------------------------------------------------
# --------------------------------------------------------
# 9. GRAFICO: EVOLUCION TEMPORAL DE ATLETAS ELITE
# --------------------------------------------------------
# --------------------------------------------------------

df_plot_year <- df_modelo %>%
  group_by(Year, Equipo) %>%
  summarise(
    n = n(),
    Porcentaje_Elite = mean(Elite, na.rm = TRUE) * 100,
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
  scale_x_continuous(
    breaks = seq(min(df_plot_year$Year), max(df_plot_year$Year), by = 2)
  ) +
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
# --------------------------------------------------------
# 10. GRAFICO: CONTROL ANTIDOPAJE
# --------------------------------------------------------
# --------------------------------------------------------

df_plot_tested <- df_modelo %>%
  mutate(
    Control_Dopaje = if_else(
      Tested_bin == 1,
      "Con testeo antidopaje",
      "Sin testeo o no reportado"
    )
  ) %>%
  group_by(Equipo, Control_Dopaje) %>%
  summarise(
    n = n(),
    Porcentaje_Elite = mean(Elite, na.rm = TRUE) * 100,
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
    name = "Estatus de la competencia",
    values = c(
      "Con testeo antidopaje" = "#5DADE2",
      "Sin testeo o no reportado" = "#E74C3C"
    )
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Proporción de atletas Elite según control antidopaje",
    subtitle = "Comparación entre competencias testeadas y no testeadas por equipamiento",
    x = "Tipo de equipamiento",
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
# 11. GRAFICO: DISTRIBUCION DE DOTS POR SEXO
# --------------------------------------------------------
# --------------------------------------------------------

grafico_dots_sexo <- ggplot(
  df_modelo,
  aes(x = Dots, fill = Sex)
) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    name = "Sexo",
    values = c("F" = "#FF6EB4", "M" = "#87CEFA")
  ) +
  labs(
    title = "Distribución del puntaje DOTS por sexo",
    x = "Puntaje DOTS",
    y = "Densidad"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

grafico_dots_sexo


# --------------------------------------------------------
# --------------------------------------------------------
# 12. GRAFICO: DOTS SEGUN SEXO
# --------------------------------------------------------
# --------------------------------------------------------

grafico_boxplot_dots_sexo <- ggplot(
  df_modelo,
  aes(x = Sex, y = Dots, fill = Sex)
) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(
    values = c("F" = "#FF6EB4", "M" = "#87CEFA")
  ) +
  labs(
    title = "Puntaje DOTS según sexo",
    x = "Sexo",
    y = "Puntaje DOTS"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  )

grafico_boxplot_dots_sexo


# --------------------------------------------------------
# --------------------------------------------------------
# 13. GRAFICO: RELACION ENTRE EDAD Y DOTS
# --------------------------------------------------------
# --------------------------------------------------------

grafico_edad_dots <- ggplot(
  df_modelo,
  aes(x = Age, y = Dots, color = Sex)
) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, method = "loess") +
  facet_wrap(
    ~ Sex,
    labeller = as_labeller(c("F" = "Mujeres", "M" = "Hombres"))
  ) +
  scale_color_manual(
    values = c("F" = "#FF6EB4", "M" = "#87CEFA")
  ) +
  labs(
    title = "Relación entre edad y rendimiento",
    subtitle = "El rendimiento se mide mediante el puntaje DOTS",
    x = "Edad",
    y = "Puntaje DOTS"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

grafico_edad_dots


# --------------------------------------------------------
# --------------------------------------------------------
# 14. GRAFICO: DOTS SEGUN EQUIPAMIENTO
# --------------------------------------------------------
# --------------------------------------------------------

grafico_dots_equipo <- ggplot(
  df_modelo,
  aes(x = Equipo, y = Dots, fill = Equipo)
) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(
    values = c("Raw" = "#4CAF50", "Equipado" = "#FF9800")
  ) +
  labs(
    title = "Puntaje DOTS según tipo de equipamiento",
    x = "Tipo de equipamiento",
    y = "Puntaje DOTS"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  )

grafico_dots_equipo