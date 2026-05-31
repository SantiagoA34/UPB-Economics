# Econometrics II - ARMA Practice
# Cleaned portfolio version
# Note: the dataset is not included in this repository.

# Simple ARMA model
#install.packages("forecast") 
#install.packages("lmtest")

library(readxl)
library(forecast)
library(lmtest)

#Establecemos directorio
setwd("~/Documents/RStudio")

apple <- read_excel("apple_6months_2025.xlsx")
View(apple)

#1. 
price <- ts(apple$Close, start = c(2025,1), frequency = 365) 
ts.plot(price) 
# Se puede apreciar una tendencia hacia el alza, cabe resaltar que a la mitad del
# periodo ocurre un decrecimiento de precios significativo, pero posteriormente 
# sigue subiendo hasta superar el máximo previo. 

#2
#Modelo lineal:
mlin <- lm(price~time(price)) 
summary(mlin)

# El modelo lineal no es adecuado porque asume que el precio sigue una tendencia 
# fija en el tiempo. Sin embargo, los precios financieros cambian de forma irregular 
# y dependen de shocks aleatorios. Por eso, aun después de ajustar la recta, los 
# residuos podrían conservar estructura temporal y no se comportarse como ruido blanco.

#3. 
resmlin <- residuals(mlin)
ts.plot(resmlin) 
# Mediante análisis visual, se puede observar que ya no existe tendencia, tambien, 
# el comportamiento se ve aleatorio e impredecible. Así que mediante la observación, 
# podemos decir que se trata de ruido blanco.

#4.   
Acf(resmlin)

# Podemos observar que en el ACF no existen cortes abruptos, sino que decaen lentamente. 
# El efecto es estadísticamente significativo hasta el lag 9, posteriormente deja de ser significativo. 

Pacf(resmlin)

# Existe un pico en el primer lag, después el efecto se corta abruptamente. 

# En base tanto al ACF como PACF, podemos sugerir los siguientes modelos:

# 1. ARMA (1,0) o AR (1): En el ACF no hay cortes abruptos, mientras que en el PACF hay un corte
# posterior al lag 1. Podemos pensar desde este momento que se trata de el modelo más simple y parsimonioso.

# 2. ARMA (1.1): Además del análisis del PACF donde el componente MA (1) es evidente, podemos considerar que
# en el ACF, el pico del lag 1 es más pronunciado que el resto, probablemente se trate de un componente MA (1)

# 3. ARMA (1,2): A lo mejor incluso podemos interpretar que los dos primeros lags en en ACF son los picos "más altos", 
# por lo que puede tratarse de un componente MA (2). 

# 4. ARMA (1,9): Podemos considerar que los 9 lags en el ACF son relevantes, y posteriormente se corta abruptamente
# hasta 0 (dejan de ser significativos). 

# 5. Estimación:

ARMA10 <- Arima(price,c(1,0,0),include.drift=TRUE)
summary(ARMA10)
ts.plot(price)
lines(fitted(ARMA10),col="red")

#AIC=599.55   AICc=599.87   BIC=610.99

ARMA11 <- Arima(price,c(1,0,1),include.drift=TRUE)
summary(ARMA11)
ts.plot(price)
lines(fitted(ARMA11),col="red")

#AIC=601.54   AICc=602.03   BIC=615.84

ARMA12 <- Arima(price,c(1,0,2),include.drift=TRUE)
summary(ARMA12)
ts.plot(price)
lines(fitted(ARMA12),col="red")

#AIC=602.78   AICc=603.47   BIC=619.94

ARMA19 <- Arima(price,c(1,0,9),include.drift=TRUE)
summary(ARMA19)
ts.plot(price)
lines(fitted(ARMA19),col="red")

#AIC=609.31   AICc=612.48   BIC=646.49

#6. Ya tenemos los valores de AIC Y SBC, mediante los cuales sabemos que el primer
# modelo: ARMA (1,0) tiene los valores más bajos, ahora veamos los errores:

autoplot(ARMA10)
checkresiduals(ARMA10)
Box.test(resid(ARMA10),type="Ljung",lag=20)

#Podemos ver que en ARMA (1,0) los errores son ruido blanco, no tienen tendencia 
# ni estructura temporal, ya que se encuentran dentro de los intérvalos de confianza, 
# además cabe resaltar que se aproxima a una distribución normal. 

autoplot(ARMA11)
checkresiduals(ARMA11)
Box.test(resid(ARMA11),type="Ljung",lag=20)


autoplot(ARMA12)
checkresiduals(ARMA12)
Box.test(resid(ARMA12),type="Ljung",lag=20)


autoplot(ARMA19)
checkresiduals(ARMA19)
Box.test(resid(ARMA19),type="Ljung",lag=20)

# Tanto para ARMA (1,1), ARMA (1,2) y ARMA (1,9), las conclusiones son las mismas, los errores son 
# "White Noise", todos los modelos logran capturar el efecto el su totalidad, sin dejar que el error
# se vea afectado por el tiempo. 


#7. 

#Se pudo observar que los errores son ruido blanco en todos los modelos, lo que nos dice que todos
# son válidos. Aunque cabe resaltar que . Sin embargo, para determinar el mejor modelo, es necesario analizar el AIC y SBC. 
# De todos los valores proporcionados, aquel con los valores más bajos es el modelo ARMA (1,0). A pesar
# de que todos los modelos tienen ruido blanco, también buscamos el modelo que cumpla con la parsimonía, 
# por lo que el modelo ARMA (1,0) llega a ser el mejor modelo para este análsis, logrando un balance entre
# parsimonía y ajuste. 

#8. Pronóstico

price.f <- forecast(ARMA10)
print(price.f)
autoplot(forecast(ARMA10))

# Mediante forecasting, se puede visualizar la predicción de los datos mediante AR (1).
# Se observa una tendencia creciente desde los valores actuales, el modelo espera que los precios continúen 
# aumentando con el tiempo. Los intérvalos de confianza no llegan a ser demasiado grandes. 
# Se asume cierta estabilidad, la cual puede no cumplirse por la naturaleza de los datos, al ser acciones, 
# son vulnerables a cambios repentinos y significativos. Ampliar el tamaño de la muestra puede mejorar el análisis. 
