# Econometrics I - Basic OLS Practice
# Cleaned portfolio version
# Note: the dataset is not included in this repository.
#install.packages("foreign") 
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(foreign)

#Establecemos directorio
setwd("~/Documents/RStudio")

#Exportamos la base de datos
EH2023_Persona <- read_sav("/Users/santiago/Documents/RStudio/EH2023_Persona.sav")

#Creamos el dataframe
data <- read.spss("/Users/santiago/Documents/RStudio/EH2023_Persona.sav", to.data.frame = TRUE)

#inciso a)
lm(ylab ~ aestudio, data = data)
reg = lm(ylab ~ aestudio, data = data)
summary(reg)
nobs(reg)
# Interpretación: La regresión es ylab = 1094.476 + 186.815aestudio
# El intercepto es 1094.436, si la variable "años de estudio" es 0, el ingreso laboral 
# será de 1094.436.

# El coeficiente beta1 es 186.815, significa que si el estudio incrementa en un año,
# el salario incrementa en 186.815 Bs/mes. 

# El R^2 es 0.03999, significa que el modelo explica el 3.99% de la variabilidad del 
# ingreso laboral

#inciso b)
data$exper <- data$s01a_03 - data$aestudio - 6
head(data$exper)

lm(ylab ~ exper, data = data)
reg = lm(ylab ~ exper, data = data)
summary(reg)
nobs(reg)
# Interpretación: La regresión es ylab = 3532.137 - 14.523exper
# El intercepto es 3532.137, si la variable "exper" es 0, el ingreso laboral 
# será de 3532.137.

# El coeficiente beta1 es -14.523, significa que si la experiencia incrementa en un año,
# el salario decrementa en 14.523 Bs/mes. 

# El R^2 es 0.002735, significa que el modelo explica el 0.3% de la variabilidad del 
# ingreso laboral

#inciso c)
lm(ylab ~ s01a_03, data = data)
reg = lm(ylab ~ s01a_03, data = data)
summary(reg)
nobs(reg)
# Interpretación: La regresión es ylab = 3055.115 + 2.716edad
# El intercepto es 3055.115, si la variable "edad" es 0, el ingreso laboral 
# será de 3055.115.

# El coeficiente beta1 es 2.716, significa que si la edad incrementa en un año,
# el salario incrementa en 2.716 Bs/mes. 

# El R^2 es 6.981e-05, significa que el modelo explica el 0.007 % de la variabilidad del 
# ingreso laboral

#inciso d)
lm(ylab ~ aestudio+s01a_03, data = data)
reg = lm(ylab ~ aestudio+s01a_03, data = data)
summary(reg)
nobs(reg)
# Interpretación: La regresión es ylab = -504.205 + 217.589aestudio + 29.866edad
# El intercepto es -504.205, si las demás variables son 0, el ingreso laboral 
# será de -504.205 (al no haber ingreso negativo, entendemos que será 0)

# El coeficiente beta1 es 217.589, significa que si los años de estudio incrementan en un año,
# el salario incrementa en 217.589 Bs/mes. Ceteris paribus las otras variables.

# El coeficiente beta2 es 29.866, significa que si la edad incrementa en un año,
# el salario incrementa en 29.866 Bs/mes. Ceteris paribus las otras variables.

# El R^2 ajustado es 0.04723, significa que el modelo explica el 4.7 % de la variabilidad del 
# ingreso laboral

#inciso e)
lm(ylab ~ aestudio+exper, data = data)
reg = lm(ylab ~ aestudio+exper, data = data)
summary(reg)
nobs(reg)
# Interpretación: La regresión es ylab = -325.010 + 247.454aestudio + 29.866exper
# El intercepto es -325.010, si las demás variables son 0, el ingreso laboral 
# será de -325.010 (al no haber ingreso negativo, entendemos que será 0)

# El coeficiente beta1 es 247.454, significa que si los años de estudio incrementan en un año,
# el salario incrementa en 247.454 Bs/mes. Ceteris paribus las otras variables.

# El coeficiente beta2 es 29.866, significa que si la experiencia incrementa en un año,
# el salario incrementa en 29.866 Bs/mes. Ceteris paribus las otras variables.

# El R^2 ajustado es 0.04723, significa que el modelo explica el 4.7 % de la variabilidad del 
# ingreso laboral

#inciso f)
lm(ylab ~ aestudio+s01a_03+exper, data = data)
reg = lm(ylab ~ aestudio+s01a_03+exper, data = data)
summary(reg)
nobs(reg)
# Interpretación: La regresión es ylab = -504.205 + 217.589 aestudio + 29.866 edad
# El intercepto es -504.205, si las demás variables son 0, el ingreso laboral 
# será de -504.205 (al no haber ingreso negativo, entendemos que será 0)

# El coeficiente beta1 es 217.589, significa que si los años de estudio incrementan en un año,
# el salario incrementa en 217.589 Bs/mes. Ceteris paribus las otras variables.

# El coeficiente beta2 es 29.866, significa que si la edad incrementa en un año,
# el salario incrementa en 29.866 Bs/mes. Ceteris paribus las otras variables.

# La razón por la cual la experiencia aparece con valores nulos, es por un caso de 
# colinealidad perfecta, la variable exper, está perfectamente correlacionada con edad,
# la regresión la elimina ya que no aporta nada al modelo. 

# El R^2 es 0.04723, significa que el modelo explica el 4.7 % de la variabilidad del 
# ingreso laboral

#inciso g) 
lm(log(ylab) ~ aestudio+exper, data = data)
reg = lm(log(ylab) ~ aestudio+exper, data = data)
summary(reg)
nobs(reg)
# Interpretación: La regresión es log(ylab) = 6.7082200 + 0.0874168 aestudio + 0.0012589 exper
# El intercepto es 6.7082200, si las demás variables son 0, el ingreso laboral 
# será de e^6.7082200.

# El coeficiente beta1 es 0.0874168, significa que si los años de estudio incrementan en un año,
# el salario incrementa en un 8.74%. Ceteris paribus las otras variables.

# El coeficiente beta2 es 0.0012589, significa que si la experiencia incrementa en un año,
# el salario incrementa en un 0.12%. Ceteris paribus las otras variables.

# El R^2 ajustado es 0.2097, significa que el modelo explica el 20.98 % de la variabilidad del 
# ingreso laboral

#inciso h)
lm(log(ylab) ~ log(aestudio)+exper, data = data)
reg = lm(log(ylab) ~ log(aestudio)+exper, data = data)
summary(reg)
nobs(reg)
#Observación: La regresión no puede realizarse ya que dentro de "aestudio" hay variables con 
# valor de 0. Procedemos a excluir observaciones con valores 0, ya que estamos analizando
# el efecto de los años de estudio sobre el ingreso laboral.

data_filtrada <- subset(data, aestudio > 0)
lm(log(ylab) ~ log(aestudio)+exper, data = data_filtrada)
reg <- lm(log(ylab) ~ log(aestudio) + exper, data = data_filtrada)
summary(reg)
nobs(reg)
# Interpretación: La regresión es log(ylab) = 6.2296990 + 0.6290126 log(aestudio) + 0.0029142 exper
# El intercepto es 6.2296990, si las demás variables son 0, el ingreso laboral 
# será de e^6.2296990

# El coeficiente beta1 es 0.6290126, significa que si los años de estudio incrementan en un 1%,
# el salario incrementa en un 0.6290126%. Ceteris paribus las otras variables.

# El coeficiente beta2 es 0.0029142, significa que si la experiencia incrementa en un año,
# el salario incrementa en un 0.29%. Ceteris paribus las otras variables.

# El R^2 ajustado es 0.1507, significa que el modelo explica el 15.07 % de la variabilidad del 
# ingreso laboral

#inciso i)
# Observación: Nuevamente filtramos la base de datos, ahora la experiencia también tomará en cuenta
# valores positivos únicamente. 

data_filtrada2 <- subset(data, aestudio > 0 & exper > 0)
lm(log(ylab) ~ log(aestudio)+log(exper), data = data_filtrada2)
reg <- lm(log(ylab) ~ log(aestudio) + log(exper), data = data_filtrada2)
summary(reg)
nobs(reg)

# Interpretación: La regresión es log(ylab) = 5.860569 + 0.668554 log(aestudio) + 0.123168 log(exper)
# El intercepto es 5.860569, si las demás variables son 0, el ingreso laboral 
# será de e^5.860569

# El coeficiente beta1 es 0.668554, significa que si los años de estudio incrementan en un 1%,
# el salario incrementa en un 0.668554%. Ceteris paribus las otras variables.

# El coeficiente beta2 es 0.123168, significa que si la experiencia incrementa en un 1%,
# el salario incrementa en un 0.123168%. Ceteris paribus las otras variables.

# El R^2 ajustado es 0.1644, significa que el modelo explica el 16.44 % de la variabilidad del 
# ingreso laboral

#inciso j)
lm(ylab ~ log(aestudio)+log(exper), data = data_filtrada2)
reg <- lm(ylab ~ log(aestudio) + log(exper), data = data_filtrada2)
summary(reg)
nobs(reg)

# Interpretación: La regresión es ylab = -2493.35 + 1713.22 log(aestudio) + 618.00 log(exper)
# El intercepto es -2493.35, si las demás variables son 0, el ingreso laboral 
# será de -2493.35 (al no haber ingreso negativo, entendemos que será 0)

# El coeficiente beta1 es 1713.22, significa que si los años de estudio incrementan en un 1%,
# el salario incrementa en 17.13 Bs/mes. Ceteris paribus las otras variables.

# El coeficiente beta2 es 618.00, significa que si la experiencia incrementa en un 1%,
# el salario incrementa en un 6.18 Bs/mes.  Ceteris paribus las otras variables.

# El R^2 ajustado es 0.03644, significa que el modelo explica el 3.64% de la variabilidad del 
# ingreso laboral

#2. 
#Verificar las varianzas de variables importantes (para justificar el log)
var(data$aestudio, na.rm = TRUE)
var(data$ylab, na.rm = TRUE)
var(data$exper, na.rm = TRUE)

#Si tuviésemos que elegir alguna regresión, sería la del inciso g) y la del inciso h)
#Inciso g)
# log(ylab) = 6.7082200 + 0.0874168 aestudio + 0.0012589 exper
# Consideramos que, al ser una variable tan volátil, añadir logaritmo es 
# importante (incluso necesario) para mejorar el análisis y corregir la heterocedasticidad. 

# Esta regresión posee una interpretación práctica y más directa sobre los efectos de 
# los años de estudio y la experiencia sobre el ingreso laboral.

#Al no filtrar "aestudio" mantenemos mayor parte de la muestra original. 

#Inciso h) 
## log(ylab) = 6.2296990 + 0.6290126 log(aestudio) + 0.0029142 exper

# A diferencia del inciso g), acá si aplicamos logaritmo a los años de estudio

# A pesar de perder algunos datos (porque filtramos la base de datos) podemos ganar mayor 
# interpretación económica, ya que podemos calcular la elasticidad entre cambios en los 
# años de estudio y el ingreso laboral.

# También podemos corregir cierta heterocedasticidad de la variable "años de estudio" 

#3. Sesgo de "edu" 
cov(data$aestudio, data$exper)
var(data$aestudio)

sesgo = (cov(data$aestudio, data$exper)/var(data$aestudio))
print(sesgo)

# El valor es -1.895515. Es el sesgo por omitir la variable "exper" en la regresión
# Es decir, por cada año adicional de educación formal, 
# la experiencia laboral disminuye en promedio 1.895515
