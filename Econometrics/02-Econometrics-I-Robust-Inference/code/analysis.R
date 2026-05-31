# Econometrics I - Robust Inference Practice
# Cleaned portfolio version
# Note: the dataset is not included in this repository.
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(foreign)
library(haven)
library(lmtest)
library(sandwich)
library(nlme)

#Establecemos directorio
setwd("~/Documents/RStudio")


#Exportamos la base de datos
EH2023_Persona <- read_sav("/Users/santiago/Documents/RStudio/EH2023_Persona.sav")

#Creamos el dataframe
data <- read.spss("/Users/santiago/Documents/RStudio/EH2023_Persona.sav", to.data.frame = TRUE)

#Inciso a) 
data$exper <- data$s01a_03 - data$aestudio - 6
head(data$exper)
lm(log(ylab) ~ aestudio+exper, data = data)
rega = lm(log(ylab) ~ aestudio+exper, data = data)
summary(rega)
nobs(rega)

white_testa <- bptest(rega, ~ aestudio + exper + I(aestudio^2) + I(exper^2) + I(aestudio*exper), data = data)

print(white_testa)

# Interpretación: La regresión es log(ylab) = 6.7082200 + 0.0874168 aestudio + 0.0012589 exper
# El intercepto es 6.7082200, si las demás variables son 0, el ingreso laboral 
# será de e^6.7082200.

# El coeficiente beta1 es 0.0874168, significa que si los años de estudio incrementan en un año,
# el salario incrementa en un 8.74%. Ceteris paribus las otras variables.

# El coeficiente beta2 es 0.0012589, significa que si la experiencia incrementa en un año,
# el salario incrementa en un 0.12%. Ceteris paribus las otras variables.

# El R^2 es 0.2098, significa que el modelo explica el 20.98 % de la variabilidad del 
# ingreso laboral

# El R^2 ajustado es 0.2097, significa que el modelo explica el 20.97 % de la variabilidad del 
# ingreso laboral

#Mediante el test de Breusch - Pagan, el p-value sale muchísimo menor a 0.05, por lo que rechazamos
#la hipótesis nula que dice que la varianza del error es constante. por lo tanto, existe heterocedasticidad.

#Inciso b) 
#variable exper^2
data$exper2 <- data$exper^2
head(data$exper2)

lm(log(ylab) ~ aestudio+exper+exper2, data = data)
regb = lm(log(ylab) ~ aestudio+exper+exper2, data = data)
summary(regb)
nobs(regb)

white_testb <- bptest(regb, 
                       ~ aestudio + exper + exper2 + 
                         I(aestudio^2) + I(exper^2) + I(exper2^2) +
                         I(aestudio*exper) + I(aestudio*exper2) + I(exper*exper2),
                       data = data)

print(white_testb)

#Inciso c) 
#variable dummy: 1 = mujer, 0 = hombre
data$female <- ifelse(data$s01a_02 == "2. Mujer", 1, 0)
table(data$female)
head(data$female)

lm(log(ylab) ~ aestudio+exper+exper2+female, data = data)
regc = lm(log(ylab) ~ aestudio+exper+exper2+female, data = data)
summary(regc)
nobs(regc)

white_testc <- bptest(regc,
                       ~ aestudio + exper + exper2 + female +
                         I(aestudio^2) + I(exper^2) + I(exper2^2) + I(female^2) +
                         I(aestudio*exper) + I(aestudio*exper2) + I(aestudio*female) +
                         I(exper*exper2) + I(exper*female) + I(exper2*female),
                       data = data)

print(white_testc)

#inciso d) 
unique(data$s01a_02)
data$urbano <- ifelse(data$area == "Urbana", 1, 0)
table(data$urbano)
head(data$urbano)

lm(log(ylab) ~ aestudio+exper+exper2+female+urbano, data = data)
regd = lm(log(ylab) ~ aestudio+exper+exper2+female+urbano, data = data)
summary(regd)
nobs(regd)

white_testd <- bptest(regd,
                       ~ aestudio + exper + exper2 + female + urbano +
                         I(aestudio^2) + I(exper^2) + I(exper2^2) + I(female^2) + I(urbano^2) +
                         I(aestudio*exper) + I(aestudio*exper2) + I(aestudio*female) + I(aestudio*urbano) +
                         I(exper*exper2) + I(exper*female) + I(exper*urbano) +
                         I(exper2*female) + I(exper2*urbano) + I(female*urbano),
                       data = data)

print(white_testd)



# EJERCICIO 3 
##########################################################################
# a) Selección del modelo base
# Se elige el modelo (d):
# ln(ylab) = β0 + β1edu + β2exper + β3exper^2 + β4female + β5urbano + ui
# Este modelo incluye factores de capital humano (educación, experiencia)
# y variables socioeconómicas (género y localización), lo que lo hace más completo.

##########################################################################
# b) Nueva variable de investigación: interacción entre female y urbano
##########################################################################
# Justificación teórica:
# Se busca analizar si el efecto del género sobre el ingreso laboral
# depende del tipo de zona donde vive la persona. Es decir,
# si la brecha salarial de género es diferente en áreas urbanas y rurales.

# Crear variable de interacción
data$female_urbano <- data$female * data$urbano

##########################################################################
# c) Especificación del modelo ampliado
##########################################################################
# ln(ylab) = β0 + β1edu + β2exper + β3exper^2 + β4female + β5urbano + β6(female*urbano) + ui
# Este modelo nos permite observar si el efecto conjunto de ser mujer y vivir en zona urbana
# amplifica o reduce el ingreso laboral esperado.

# Estimamos la regresión
reg_inter <- lm(log(ylab) ~ aestudio + exper + I(exper^2) + female + urbano + female_urbano, data = data)
summary(reg_inter)


# El coeficiente 0.012 es positivo pero no significativo (p = 0.683). 
# Esto indica que no existe evidencia estadística de que el efecto del género 
# sobre los ingresos varíe entre áreas urbanas y rurales.
# En otras palabras, la brecha salarial de género persiste tanto en zonas urbanas 
# como rurales, sin diferencias apreciables en magnitud.

# El efecto del género sobre el ingreso laboral no depende del área de residencia.
# Las mujeres, en promedio, perciben ingresos significativamente menores que los hombres, 
# tanto en zonas rurales como urbanas, incluso tras controlar por educación y experiencia.

