# NOVATEK Stock Volatility Analysis
# Cleaned portfolio version
# Note: the dataset is not included in this repository.
# Librerías 
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

# Cargamos las base de datos 
novatek <- read_excel("data/novatek.xlsx")
names(novatek) <- c("Fecha", "Cierre", "Cambio_Porc", "Volumen", "Max", "Min")

# Leemos SOLO las primeras 6 columnas
novatek <- read_excel("NOVATEK.xlsx") %>%
  select(1:6)

# Asignamos nombres correctos
colnames(novatek) <- c("Fecha", "Cierre", "Cambio_Porc", "Volumen", "Max", "Min")

# Eliminamos filas vacías
novatek <- novatek %>% filter(!is.na(Fecha))

# Aseguramos que Fecha sea Date
novatek <- novatek %>% mutate(Fecha = as.Date(Fecha))

# EXTRAEMOS AÑO
novatek <- novatek %>% mutate(Anho = year(Fecha))

### 1. Graficar la serie temporal del precio de cierre
ggplot(novatek, aes(x = Fecha, y = Cierre)) +
  geom_line(color = "steelblue") +
  labs(title = "Serie temporal del precio de cierre de NOVATEK",
       x = "Fecha", y = "Precio (USD)") +
  theme_minimal()

### 2.1 Precio promedio anual
precio_promedio_anual <- novatek %>%
  group_by(Anho) %>%
  summarise(Promedio_Cierre = mean(Cierre, na.rm = TRUE))

print(precio_promedio_anual)

### 2.2 Mayor subida y mayor bajada

mayor_subida <- novatek %>% filter(Cambio_Porc == max(Cambio_Porc, na.rm = TRUE))
mayor_bajada <- novatek %>% filter(Cambio_Porc == min(Cambio_Porc, na.rm = TRUE))

print("Semana con mayor subida porcentual:")
print(mayor_subida)

print("Semana con mayor bajada porcentual:")
print(mayor_bajada)

### 3. Rango semanal y gráfico de volatilidad
# Crear la variable 'Rango'
novatek <- novatek %>%
  mutate(Rango = Max - Min)

# Graficar el rango semanal
ggplot(novatek, aes(x = Fecha, y = Rango)) +
  geom_line(color = "darkorange") +
  labs(
    title = "Rango semanal de precios (High - Low)",
    x = "Fecha",
    y = "Rango (USD)"
  ) +
  theme_minimal()

### 4.Top 5 semanas con mayor volatilidad 
top_volatilidad <- novatek %>%
  arrange(desc(Rango)) %>%
  slice(1:5)

print("5 semanas con mayor rango (volatilidad):")
print(top_volatilidad)

### 5. Correlación entre precio de cierre y volumen negociado
# Calcular la correlación
correlacion_precio_volumen <- cor(novatek$Cierre, novatek$Volumen, use = "complete.obs")
print(paste("Correlación entre precio de cierre y volumen negociado:", round(correlacion_precio_volumen, 4)))

### ¿A mayor volumen, mayor cambio de precio? (absoluto)

ggplot(novatek, aes(x = Volumen, y = abs(Cambio_Porc))) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(
    title = "¿Mayor volumen implica mayor cambio porcentual?",
    x = "Volumen negociado",
    y = "|Cambio % semanal|"
  ) +
  theme_minimal()

### 6. Eventos económicos y su impacto en NOVATEK
# 1. Evento COVID-19 (marzo–abril 2020)
evento_covid <- novatek %>%
  filter(Fecha >= as.Date("2020-03-01") & Fecha <= as.Date("2020-04-30"))

# 2. Invasión a Ucrania (febrero–marzo 2022)
evento_guerra <- novatek %>%
  filter(Fecha >= as.Date("2022-02-01") & Fecha <= as.Date("2022-03-31"))

# 3. Crisis energética global (junio–agosto 2021)
evento_energia <- novatek %>%
  filter(Fecha >= as.Date("2021-06-01") & Fecha <= as.Date("2021-08-31"))

### 6.1 Precio de cierre en los eventos
ggplot() +
  geom_line(data = novatek, aes(x = Fecha, y = Cierre), color = "grey80") +
  geom_line(data = evento_covid, aes(x = Fecha, y = Cierre), color = "red", linewidth = 1.2) +
  geom_line(data = evento_guerra, aes(x = Fecha, y = Cierre), color = "blue", linewidth = 1.2) +
  geom_line(data = evento_energia, aes(x = Fecha, y = Cierre), color = "green", linewidth = 1.2) +
  labs(
    title = "Impacto de eventos en el PRECIO DE CIERRE de NOVATEK",
    x = "Fecha", y = "Precio de cierre (USD)"
  ) +
  theme_minimal()

### 6.2 Volumen negociado en los eventos
ggplot() +
  geom_line(data = novatek, aes(x = Fecha, y = Volumen), color = "grey80") +
  geom_line(data = evento_covid, aes(x = Fecha, y = Volumen), color = "red", linewidth = 1.2) +
  geom_line(data = evento_guerra, aes(x = Fecha, y = Volumen), color = "blue", linewidth = 1.2) +
  geom_line(data = evento_energia, aes(x = Fecha, y = Volumen), color = "green", linewidth = 1.2) +
  labs(
    title = "Impacto de eventos en el VOLUMEN NEGOCIADO de NOVATEK",
    x = "Fecha", y = "Volumen"
  ) +
  theme_minimal()
