# Tourism ANOVA Analysis in Bolivia
# Cleaned portfolio version
# Note: the dataset is not included in this repository.
#######
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)

datos <- read_xls("data/tourism_bolivia.xls")
datos <- read_xls("Bases de datos turismo.xls")

datos_largos <- datos %>%
  pivot_longer(
    cols = -`Departamento/Mes`, 
    names_to = "Mes_Año",
    values_to = "Turistas"
  ) %>%
  separate(Mes_Año, into = c("Mes", "Año"), sep = "_") %>%
  mutate(
    Departamento = `Departamento/Mes`,
    Año = as.integer(Año),
    Mes = factor(Mes, levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                                 "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                 ordered = TRUE)
  ) %>%
  select(Departamento, Año, Mes, Turistas)


###ANALISIS ANOVA BIFACTORAL
anova_model <- aov(Turistas ~ Departamento * Mes, data = datos_largos)
summary(anova_model)

#####Boxplots comparativos 
ggplot(datos_largos, aes(x = Mes, y = Turistas, fill = Departamento)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribución mensual del turismo por departamento",
       y = "Número de turistas", x = "Mes")

######Grafico de efectos principales 
datos_largos$Turistas <- as.numeric(datos_largos$Turistas)

# Medias por Departamento
efecto_departamento <- datos_largos %>%
  group_by(Departamento) %>%
  summarise(Media = mean(Turistas, na.rm = TRUE))

# Medias por Mes
efecto_mes <- datos_largos %>%
  group_by(Mes) %>%
  summarise(Media = mean(Turistas, na.rm = TRUE))

# Efecto principal: Departamento
ggplot(efecto_departamento, aes(x = reorder(Departamento, -Media), y = Media)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Efecto principal del Departamento sobre el turismo interno",
       x = "Departamento", y = "Promedio mensual de turistas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Efecto principal: Mes
ggplot(efecto_mes, aes(x = Mes, y = Media)) +
  geom_line(group = 1, color = "red", size = 1.2) +
  geom_point(color = "darkred", size = 2) +
  theme_minimal() +
  labs(title = "Efecto principal del Mes sobre el turismo interno",
       x = "Mes", y = "Promedio mensual de turistas")

# Mapa de calor tradicional con cuadrados
ggplot(datos_largos, aes(x = Mes, y = fct_rev(Departamento), fill = Turistas)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "plasma", name = "Turistas", na.value = "grey90") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Mapa de calor del turismo por mes y departamento",
    subtitle = "Color más intenso indica mayor cantidad de turistas",
    x = "Mes",
    y = "Departamento"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

# Promedio por Mes y Departamento
interaccion <- datos_largos %>%
  group_by(Departamento, Mes) %>%
  summarise(Media = mean(Turistas, na.rm = TRUE), .groups = "drop")

# Gráfico de interacción
ggplot(interaccion, aes(x = Mes, y = Media, color = Departamento, group = Departamento)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Gráfico de interacción: Mes vs. Departamento",
       x = "Mes", y = "Promedio mensual de turistas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
