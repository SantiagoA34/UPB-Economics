# CO2 Emissions MLE Analysis
# Cleaned portfolio version
# Note: the dataset is not included in this repository.

# 1. Instalar y cargar librerías
# install.packages(c("readxl","MASS","e1071"))
library(readxl)
library(dplyr)
library(MASS)
library(e1071)    # para skewness() y kurtosis()

# 2. Cargar datos
ruta <- "data/co2_emissions_2023.xlsx"
df <- read_excel(ruta)

# 3. Limpieza y transformación
df <- df %>%
  mutate(
    `CO2 t per capita` = as.numeric(gsub(",", ".", `CO2 t per capita`)),
    ln_co2_pc = log(`CO2 t per capita`)
  )

# 4. Cálculo de estadísticas descriptivas
mediana    <- median(df$ln_co2_pc, na.rm = TRUE)
desvest    <- sd(df$ln_co2_pc,    na.rm = TRUE)
sesgo      <- skewness(df$ln_co2_pc, na.rm = TRUE)
curtosis   <- kurtosis(df$ln_co2_pc, na.rm = TRUE)

cat("Estadísticas descriptivas de ln(CO2 per cápita):\n")
cat(sprintf("  Mediana           = %.4f\n", mediana))
cat(sprintf("  Desviación estándar = %.4f\n", desvest))
cat(sprintf("  Sesgo (skewness)    = %.4f\n", sesgo))
cat(sprintf("  Curtosis            = %.4f\n", curtosis))

# 5. Estimación MLE de la normal
fit_norm  <- fitdistr(df$ln_co2_pc, densfun="normal")
mu_hat    <- fit_norm$estimate["mean"]
sigma_hat <- fit_norm$estimate["sd"]
print(fit_norm)
print(mu_hat)
print(sigma_hat)

# 6. Histograma + curva normal
hist(df$ln_co2_pc,
     prob = TRUE,
     col  = "lightblue",
     border = "white",
     main  = "Histograma de ln(CO2 per cápita) + Curva Normal Ajustada",
     xlab  = "ln(CO2 per cápita)",
     ylab  = "Densidad")

xseq <- seq(min(df$ln_co2_pc, na.rm = TRUE),
            max(df$ln_co2_pc, na.rm = TRUE),
            length = 100)
lines(xseq,
      dnorm(xseq, mean = mu_hat, sd = sigma_hat),
      col = "red", lwd = 2)

