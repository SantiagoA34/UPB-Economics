# Econometrics II - Chile Labor Market and Macro Volatility
# Cleaned portfolio version
# Note: the datasets are not included in this repository.

#install.packages(c("R.oo","R.utils","R.cache","parallelly","future","globals","foreach","doParallel"),
#                 type = "binary",
#                 repos = "https://cran.rstudio.com/")

#install.packages("rugarch", dependencies = TRUE, type = "source", repos = "https://cran.rstudio.com/")


library(readxl)
library(tseries)
library(forecast)
library(vars)
library(lmtest)
library(urca)
library(quantmod)
library(rugarch)
library(xts)
library(FinTS)
library(zoo)

setwd("~/Documents/RStudio")

datos <- read_excel("BD METRICSIIFIN.xlsx", skip = 2)  
colnames(datos)
colnames(datos) <- c("Año", "TrimestreMovil", "TasaDesocupacion", "TasaOcupacion", 
                     "TasaParticipacion", "IMCE", "VarIPC","TC")

datoscovid <- read_excel("BD METRICSIICOVID.xlsx", skip = 2)  
colnames(datoscovid)
colnames(datoscovid) <- c("Año", "TrimestreMovil", "TasaDesocupacion", "TasaOcupacion", 
                          "TasaParticipacion", "IMCE", "VarIPC","TC")

#Estimación Univariada - Tasa de Participación

n_obs <- nrow(datos)
part_ts <- ts(datos$TasaParticipacion,
              start = c(2010, 1),
              frequency = 11)
plot(part_ts, main = "Tasa de Participación", ylab = "Tasa")

# Prueba de raíz unitaria sobre la serie original
adf_part1 <- ur.df(part_ts, type = "drift", lags = 12)
summary(adf_part1)

# Diferenciación
part_diff <- diff(part_ts)
plot(part_diff, main = "Tasa de Participación en primeras diferencias", ylab = "Δ Tasa")

# Prueba de raíz unitaria sobre la serie diferenciada
adf_part <- ur.df(part_diff, type = "drift", lags = 12)
summary(adf_part)

#Identificación del modelo
acf(part_diff, main = "ACF de la Tasa de Participación Diferenciada")
pacf(part_diff, main = "PACF de la Tasa de Participación Diferenciada")

part411 <- Arima(part_ts, order = c(4,1,1), include.drift = TRUE)
summary(part411)
#AIC=-49.48   AICc=-48.46   BIC=-30.08
ts.plot(part_ts)
lines(fitted(part411),col="red")

part511 <- Arima(part_ts, order = c(5,1,1), include.drift = TRUE)
summary(part511)
#AIC=-62.51   AICc=-61.19   BIC=-40.34
ts.plot(part_ts)
lines(fitted(part511),col="red")

part611 <- Arima(part_ts, order = c(6,1,1), include.drift = TRUE)
summary(part611)
#AIC=-62.22   AICc=-60.56   BIC=-37.29
ts.plot(part_ts)
lines(fitted(part611),col="red")

# Fuerte candidato: ARIMA (5,1,1) with drift
# Corroboración: Identificación automática del modelo ARIMA
arma_part <- auto.arima(part_ts)
summary(arma_part)

#Definimos: ARIMA(5,1,1) with drift 
res_part <- residuals(arma_part)

# Test de autocorrelación de residuos
lb_part <- Box.test(res_part, lag = 12, type = "Ljung-Box")
lb_part

# Revisión gráfica de residuos
checkresiduals(arma_part)
autoplot(arma_part)

# Test de heterocedasticidad (ARCH)
ArchTest(res_part, lags = 12)

# Forecast a 12 periodos
forecast_arma_part <- forecast(arma_part, h = 12)
forecast_arma_part

# Grafico forecast
plot(forecast_arma_part, main = "Forecast Tasa de Participación (ARIMA(5,1,1))",
     xlab = "Trimestres", ylab = "Tasa de Participación")

# Tasa de Participación COVID-19 - DATOS REALES
# Número total de observaciones
n_obs <- nrow(datoscovid)

desemp_ts <- ts(datoscovid$TasaParticipacion,
                start = c(2010, 1),
                frequency = 11)

# Recortar la serie hasta 2022
desemp_ts <- window(desemp_ts, end = c(2022, 11))

plot(desemp_ts,
     main = "Tasa de Participación (2010–2022)",
     ylab = "Tasa")


#Estimación ARCH/GARCH - TC

n_obs <- nrow(datos)
tc_ts <- ts(datos$TC,
             start = c(2010, 1),
             frequency = 11)
plot(tc_ts, main = "Tipo de cambio nominal", ylab = "TC")

# Prueba de raíz unitaria sobre la serie original
adf_tc1 <- ur.df(tc_ts, type = "drift", lags = 12)
summary(adf_tc1)

# Diferenciación
tc_diff <- diff(tc_ts)
plot(tc_diff, main = "Tipo de cambio en primeras diferencias", ylab = "Δ TC")

# Prueba de raíz unitaria sobre la serie diferenciada
adf_tc <- ur.df(tc_diff, type = "drift", lags = 12)
summary(adf_tc)

#Segunda diferencia
dd_tc <- diff(diff(tc_diff))  
plot(dd_tc, main = "Tipo de cambio en segundas diferencias", ylab = "Δ TC")

# Prueba de raíz unitaria sobre la serie de doble diferencia
adf_ddtc <- ur.df(dd_tc, type = "drift", lags = 12)
summary(adf_ddtc)

# Estimación de modelo
arma_tc <- auto.arima(tc_ts)
summary(arma_tc)

#Definimos: ARIMA(4,1,0)
res_tc <- residuals(arma_tc)

# Test de autocorrelación de residuos
lb_tc <- Box.test(res_tc, lag = 12, type = "Ljung-Box")
lb_tc

# Revisión gráfica de residuos
checkresiduals(arma_tc)
autoplot(arma_tc)

# Test de heterocedasticidad (ARCH)
ArchTest(res_tc, lags = 12)

#Test de correlograma
acf(res_tc^2, 
    main = "ACF de los residuos al cuadrado (TC)")

# Especificación GARCH(1,1)
spec_garch_diff <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "norm"
)

# Ajustar modelo
garch_fit_diff <- ugarchfit(spec = spec_garch_diff, data = tc_diff, out.of.sample = 20)
garch_fit_diff

#Predicción
garch11.boot = ugarchboot(garch_fit_diff, method="Partial", n.ahead=10, n.bootpred=2000)
print(garch11.boot)

# Análisis multivariado parsimonioso: Ocupación - IMCE

# Series individuales
ocup_ts  <- ts(datos$TasaOcupacion, start = c(2010,1), frequency = 11)
imce_ts  <- ts(datos$IMCE,           start = c(2010,1), frequency = 11)

# Base multivariada
data_var <- cbind(
  Ocupacion = ocup_ts,
  IMCE      = imce_ts
)

summary(data_var)
plot(data_var)

# ADF en niveles
adf_ocup  <- ur.df(ocup_ts, type = "drift", lags = 12)
adf_imce  <- ur.df(imce_ts, type = "drift", lags = 12)

summary(adf_ocup)
summary(adf_imce)

# Transformaciones para estacionariedad
d_ocup <- diff(ocup_ts)           # Primera diferencia
dd_imce <- diff(diff(imce_ts))    # Segunda diferencia

# ADF en diferencia
adf_docup  <- ur.df(d_ocup, type = "drift", lags = 12)
adf_ddimce  <- ur.df(dd_imce, type = "drift", lags = 12)

summary(adf_docup)
summary(adf_ddimce)


# Crear data frame para VAR
var_data <- cbind(d_ocup, dd_imce)

# Limpieza de NAs y alineación
min_len <- min(length(d_ocup), length(dd_imce))
d_ocupalin <- tail(d_ocup, min_len)
dd_imcealin <- tail(dd_imce, min_len)

var_data_clean <- cbind(d_ocupalin, dd_imcealin)

# Verificar filas y NAs
nrow(var_data_clean)
colSums(is.na(var_data_clean))

# Selección de rezagos óptimos
lag <- VARselect(var_data_clean, lag.max = 12, type = "const")
lag$criteria
lag$selection

# Estimación del VAR parsimonioso
var_model <- VAR(var_data_clean, p = 6, type = "const")
summary(var_model)

# Test de causalidad de Granger
caus_ocup <- causality(var_model, cause = "d_ocupalin")
caus_ocup$Granger

caus_imce <- causality(var_model, cause = "dd_imcealin")
caus_imce$Granger

# Impulso-respuesta ante un shock en la Tasa de Ocupación
irf_ocup <- irf(var_model, impulse = "d_ocupalin", response = c("d_ocupalin", "dd_imcealin"),
                n.ahead = 5, boot = TRUE, ci = 0.95)
plot(irf_ocup, main = "Respuesta ante shock en Tasa de Ocupación")


# Impulso-respuesta ante un shock en IMCE
irf_imce <- irf(var_model, impulse = "dd_imcealin", response = c("d_ocupalin", "dd_imcealin"),
                n.ahead = 12, boot = TRUE, ci = 0.95)
plot(irf_imce, main = "Respuesta ante shock en IMCE")

# Descomposición de la varianza
fevd_ocup <- fevd(var_model, n.ahead = 5)  
fevd_imce <- fevd(var_model, n.ahead = 5)

# Resultados numéricos
fevd_ocup
fevd_imce
