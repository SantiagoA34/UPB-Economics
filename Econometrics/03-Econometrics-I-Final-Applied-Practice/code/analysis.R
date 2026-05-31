# Econometrics I - Final Applied Practice
# Cleaned portfolio version
# Note: the dataset is not included in this repository.
setwd("/Users/USUARIO/Documents/R Studio canchero")
getwd()
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
install.packages("AER")
library(AER)
#Establecemos directorio
setwd("/Users/USUARIO/Documents/R Studio canchero")

#Exportamos la base de datos
EH2023_Persona <- read_sav("/Users/USUARIO/Documents/R Studio canchero/EH2023_Persona.sav")

#Creamos el dataframe
data <- read.spss("/Users/USUARIO/Documents/R Studio canchero/EH2023_Persona.sav", to.data.frame = TRUE)

#Variable exper
data$exper <- data$s01a_03 - data$aestudio - 6
head(data$exper)

#Variable female (dummy): 1 = mujer, 0 = hombre
data$female <- ifelse(data$s01a_02 == "2. Mujer", 1, 0)
table(data$female)
head(data$female)

#Variable urbano 
data$urbano <- ifelse(data$area == "Urbana", 1, 0)
table(data$urbano)
head(data$urbano)

#Cambio de nombres
names(data)[names(data) == "aestudio"] <- "edu"
names(data)[names(data) == "s01a_03"]   <- "age"

#Inciso a)
rega = lm(log(ylab) ~ edu+exper+age, data = data)
summary(rega)
nobs(rega)
confint(rega)
bptest(rega)

# El coeficiente de edu (=0.0874168) indica que un año adicional de educación
# incrementa el salario en aproximadamente 8.74% en promedio, manteniendo fija la
# experiencia. El error estándar (0.0016019) es reducido, lo que sugiere alta precisión.
# El valor t (=54.572) es extremadamente elevado y el p-valor (<0.001) descarta
# de forma contundente la hipótesis nula de que el coeficiente sea cero. El intervalo
# de confianza [0.08428, 0.09056] muestra que el efecto estimado es robusto.
#
# El coeficiente de exper (=0.0012589) implica que cada año de experiencia incrementa
# el salario en torno a 0.126%. El error estándar (0.0004761) es pequeño, el valor t
# (=2.644) indica que la señal estadística es moderada pero suficiente, y el p-valor
# (=0.0082) muestra significancia estadística. El intervalo [0.000326, 0.002192] confirma
# un efecto positivo, aunque cuantitativamente modesto.
#
# La variable age presenta NA en coeficiente y error estándar debido a colinealidad
# perfecta con la variable "exper", lo que impide su identificación dentro del modelo.
#
# El R2 (=0.2098) indica que el 21% de la variación del logaritmo del salario es
# explicada por educación y experiencia. El R2 ajustado (=0.2097) apenas difiere,
# lo que refleja estabilidad del ajuste.
#
# El estadístico F (=2211, p<0.001) rechaza la hipótesis conjunta de irrelevancia
# de los coeficientes, validando la utilidad global del modelo.
#
# El test Breusch–Pagan (BP=1175.5, p<0.001) revela heterocedasticidad marcada,
# lo que implica que la varianza de los residuos no es constante.

#Inciso b)
regb = lm(log(ylab) ~ edu+exper+age+female, data = data)
summary(regb)
nobs(regb)
confint(regb)
bptest(regb)

# El coeficiente de edu (=0.0875031) indica que cada año adicional de educación
# eleva el salario en un 8.75%. Su error estándar (0.0015823) es reducido, el valor t
# (=55.302) es muy elevado y el p-valor (<0.001) descarta de forma categórica la
# ausencia de efecto. El intervalo [0.08440, 0.09060] confirma estabilidad y precisión.
#
# El coeficiente de exper (=0.0013137) muestra que un año adicional de experiencia
# incrementa el salario en 0.131%. El error estándar (0.0004703) es pequeño, el valor t
# (=2.793) indica señal estadística clara y el p-valor (=0.0052) establece significancia.
# El intervalo [0.000392, 0.002236] mantiene el efecto dentro de un rango acotado y positivo.
#
# El coeficiente de female (=−0.2610187) señala que, manteniendo constantes educación
# y experiencia, las mujeres perciben salarios aproximadamente 26.1% menores que los
# hombres. El error estándar (0.0128015) es muy bajo, el valor t (=−20.390) es de gran
# magnitud y el p-valor (<0.001) indica alta significancia. El intervalo [−0.28611, −0.23593]
# descarta cualquier posibilidad de efecto cercano a cero.
#
# age continúa no identificada por colinealidad exacta, por lo que el modelo la excluye.
#
# El R2 (=0.2291) muestra que el 22.9% de la variación del salario es explicado por
# educación, experiencia y género. El R2 ajustado (=0.2289) confirma el incremento de ajuste.
#
# El estadístico F (=1649, p<0.001) establece que el conjunto de coeficientes es
# significativamente distinto de cero.
#
# El test Breusch–Pagan (BP=1234.8, p<0.001) indica persistente heterocedasticidad.


#Inciso c) 
regc = lm(log(ylab) ~ edu+exper+age+female+urbano, data = data)
summary(regc)
nobs(regc)
confint(regc)
bptest(regc)

# El coeficiente de edu (=0.0716531) implica que un año adicional de educación
# incrementa el salario en 7.17%. El error estándar (0.0015972) es bajo, el valor t
# (=44.861) muestra gran precisión y el p-valor (<0.001) respalda la significancia
# estadística. El intervalo [0.06852, 0.07478] indica un efecto estable y acotado.
#
# El coeficiente de exper (=0.0018361) sugiere que un año de experiencia incrementa
# el salario en 0.184%. El error estándar (0.0004548) es pequeño, el valor t (=4.038)
# es consistente con una señal estadística sólida y el p-valor (<0.001) confirma
# significancia. El intervalo [0.000945, 0.002727] indica un efecto positivo seguro.
#
# El coeficiente de female (=−0.3056174) indica que el salario de las mujeres es,
# en promedio, 30.6% menor que el de los hombres, manteniendo constantes todas las
# demás variables. Su error estándar (0.0124391) es reducido, el valor t (−24.569)
# es de magnitud muy elevada y el p-valor (<0.001) señala significancia absoluta.
# El intervalo [−0.33000, −0.28124] corrobora robustez del efecto.
#
# El coeficiente de urbano (=0.5445379) muestra que residir en área urbana se asocia
# con un salario 54.45% mayor que vivir en área rural, manteniendo constantes educación,
# experiencia y género. El error estándar (0.0158553) es pequeño, el valor t (=34.344)
# es muy alto y el p-valor (<0.001) indica un efecto estadísticamente incuestionable.
# El intervalo [0.51346, 0.57562] exhibe notable precisión.
#
# age permanece excluida por colinealidad exacta.
#
# El R2 (=0.2801) indica que el modelo explica el 28% de la variación del salario,
# un incremento notable respecto a los modelos previos. El R2 ajustado (=0.2799)
# confirma este avance.
#
# El estadístico F (=1619, p<0.001) establece significancia del conjunto de regresores.
#
# El test Breusch–Pagan (BP=1380.9, p<0.001) muestra heterocedasticidad clara,
# sugiriendo que los residuos no presentan varianza constante.




# 3.2 (a) Instrumento para edu: primera etapa

grep("padre|madre|jefe|educ|estudio", names(data),
     ignore.case = TRUE, value = TRUE)


# Construimos la base con las variables necesarias
vars_iv <- c("ylab", "edu", "exper", "female", "urbano", "educ_prev")
data_iv <- na.omit(data[ , vars_iv])

# Primera etapa: edu en función del instrumento (educ_prev) + controles
first_stage <- lm(edu ~ educ_prev + exper + female + urbano, data = data_iv)
summary(first_stage)
#El coeficiente edu_prev tiene un estadístico t ≈ 30.6 y p-valor < 0.001. Además, el F global de la regresión es 250.2.  
#Esto muestra que `educ_prev` está fuertemente correlacionada con los años de educación del individuo y no es un instrumento débil. 
#Bajo el supuesto de que `educ_prev` no afecta directamente el salario, sino solo a través de `edu`, podemos considerarla un instrumento relevante y  válido para corregir la endogeneidad de la educación.

# 3.2 (b) MCO vs IV con educ_prev como instrumento


# Modelo MCO (OLS) base
ols <- lm(log(ylab) ~ edu + exper + female + urbano, data = data_iv)
summary(ols)

# Modelo IV/2SLS: edu instrumentada con educ_prev
iv <- ivreg(log(ylab) ~ edu + exper + female + urbano |
              educ_prev + exper + female + urbano,
            data = data_iv)

summary(iv, diagnostics = TRUE)

#El test de Hausman  rechaza la exogeneidad de `edu`, indicando que el estimador MCO es inconsistente por endogeneidad.  

#En consecuencia, se prefiere la estimación IV, y el retorno de la educación de alrededor de **5% por año** es el que se interpreta como efecto causal corregido.

# 3.3 a) Si el error de medida está en la variable dependiente `ylab` (por ejemplo, el ingreso está mal medido u oscila mucho por outliers), el error se incorpora al término de error de la regresión. 
#Bajo el supuesto de error clásico , los estimadores MCO siguen siendo insesgados y consistentes, pero menos precisos .

#En la práctica, la corrección parar mejorar la medición de `ylab, puede hacerse mediante el incremento del tamano de la muestra, una mejora en los disenos de las encuestas y wl uso de datos verificaods o autenticos.
#3.3 b) Error de medida en edu

# 1) Base con las variables necesarias 
vars_iv <- c("ylab", "edu", "exper", "female", "urbano", "educ_prev")
data_iv <- na.omit(data[ , vars_iv])

# 2) Modelo original MCO 
ols_3_3 <- lm(log(ylab) ~ edu + exper + female + urbano, data = data_iv)
summary(ols_3_3)

# 3) Primera etapa de 2SLS: edu ~ educ_prev + controles
first_stage_3_3 <- lm(edu ~ educ_prev + exper + female + urbano, data = data_iv)
summary(first_stage_3_3)   # aquí ves relevancia del instrumento

# 4) Segunda etapa "manual"
data_iv$edu_hat <- fitted(first_stage_3_3)

second_stage_3_3 <- lm(log(ylab) ~ edu_hat + exper + female + urbano, data = data_iv)
summary(second_stage_3_3)

# 5) 2SLS en un solo paso con ivreg
iv_3_3 <- ivreg(log(ylab) ~ edu + exper + female + urbano |
                  educ_prev + exper + female + urbano,
                data = data_iv)

summary(iv_3_3, diagnostics = TRUE)  # incluye Wu–Hausman y weak instruments

#el único cambio fuerte es que el efecto de la educación se reduce bastante al corregir el modelo, el resto de parámetros y estadísticos solo cambia moderadamente.

# 3.4. Discusión sobre los Resultados 
#
# a) ¿Cómo cambian los coeficientes y significancias después de corregir
# los problemas de endogeneidad y errores de medida?
#
# Al comparar el modelo MCO con las estimaciones
# con variables instrumentales, el cambio más importante
# se da en el coeficiente de edu. En MCO, el retorno a la educación se
# sitúa alrededor de 7%–9% por año de estudio,
# mientras que, al instrumentar edu con `educ_prev`, el coeficiente cae a
# aproximadamente 5% por año. Es decir, una vez corregida la posible
# endogeneidad y el error de medida en edu, el efecto estimado de la
# educación sobre el ingreso laboral sigue siendo positivo y significativo,
# pero de menor magnitud.
#
# Además, el error estándar de edu aumenta en el modelo IV respecto al MCO,
# lo que reduce el valor t, aunque el coeficiente continúa siendo
# estadísticamente significativo. Esto es consistente con la teoría
# que dice que los estimadores IV son menos precisos que los MCO, pero
# más confiables cuando hay endogeneidad o error de medida en la variable
# explicativa.
#
# En cambio, los coeficientes de exper, female y urbano cambian poco entre
# MCO e IV. Los signos se mantienen, las magnitudes varían solo de manera
# moderada y la significancia estadística se conserva en niveles
# similares. Por lo tanto, la principal corrección recae sobre el retorno
# de la educación, mientras que la experiencia, la brecha de género y la
# prima urbana resultan bastante robustas.
#
# En el caso de un error de medida clásico en la variable dependiente
# (ylab), el efecto teórico es distinto. Los coeficientes MCO siguen siendo
# insesgados y consistentes, pero sus errores estándar aumentan y la
# significancia puede reducirse. Es decir, la corrección de este tipo de
# error afecta principalmente la precisión de las estimaciones, no tanto el
# valor medio de los coeficientes.
#
# (b) ¿Qué conclusiones puede extraer sobre la relación entre educación,
# experiencia y el ingreso laboral, una vez que corrigió los problemas de
# endogeneidad y error de medida?
#
# Una vez corregida la posible endogeneidad y el error de medida en edu
# mediante variables instrumentales, podemos interpretar el coeficiente de
# edu como un efecto más cercano al causal. Cada año adicional de educación
# eleva el ingreso laboral en torno a 5% en promedio. Esto implica que los
# modelos MCO tienden a sobreestimar el retorno educativo (7%–9%), y que
# parte de ese retorno reflejaba habilidades no observadas, entorno
# familiar u otros factores correlacionados tanto con la educación como con
# el salario.
#
# La experiencia laboral mantiene un efecto positivo pero cuantitativamente
# pequeño, lo que sugiere que, en esta base de datos, la acumulación de 
# experiencia tiene rendimientos crecientes muy modestos
# frente a los de la educación formal. Al mismo tiempo, la variable
# female muestra una brecha salarial persistente y estadísticamente
# muy significativa, las mujeres ganan entre 25% y 30% menos que los
# hombres con características observables similares. Finalmente, urbano
# presenta una prima salarial elevada, lo que indica
# fuertes diferencias de ingresos entre áreas urbanas y rurales.
#
# En conjunto, tras corregir endogeneidad y errores de medida, se refuerza
# la conclusión de que la educación tiene un efecto positivo y relevante
# sobre el salario, pero menor al que sugerían las estimaciones MCO, a la
# vez, se confirma la importancia de la localización (urbano) y la
# existencia de una brecha de género sustancial y robusta. Estas relaciones
# pueden interpretarse como evidencia de retornos educativos significativos
# y de fuertes desigualdades estructurales en el mercado laboral.

# 3.5. Selección del Modelo y Propuesta de Extensión 
#
# Modelo seleccionado:
# Para la investigación se selecciona el modelo IV corregido por
# endogeneidad y error de medida en la educación:
#
#   iv_3_3 <- ivreg(log(ylab) ~ edu + exper + female + urbano 
#                     educ_prev + exper + female + urbano, data = data_iv)
#
# Este modelo es preferido porque:
#  - El test de Hausman rechaza la exogeneidad de edu, por lo que el MCO
#    es inconsistente y el IV es más adecuado para estimar el efecto causal.
#  - Corrige simultáneamente la posible endogeneidad y el error de medida
#    en edu usando `educ_prev` como instrumento relevante y válido.
#  - Mantiene controles clave (exper, female, urbano), con signos y
#    magnitudes robustas, lo que lo hace un buen resumen de la estructura
#    salarial en la muestra.
#
# Propuesta de extensión: incluir una nueva variable
# Supongamos que en la base existe una variable que identifica si la
# persona tiene empleo formal (por ejemplo, cotiza a la seguridad social
# o cuenta con contrato escrito). Denominamos a esta variable `formal`,
# donde:
#   formal = 1 si el trabajador está en el sector formal,
#   formal = 0 si está en el sector informal.
#
# El modelo extendido sería:
#
#   log(ylab) = β0 + β1 edu + β2 exper + β3 female + β4 urbano
#               + β5 formal + u
#
# instrumentando edu con `educ_prev` 
#
# a) Hipótesis de investigación relacionada con la nueva variable
# H1: Los trabajadores en el sector formal perciben, en promedio, un
#     ingreso laboral significativamente mayor que los trabajadores en el
#     sector informal, controlando por educación, experiencia, género y
#     área de residencia.
#
# Es decir, se espera que β5 > 0 y estadísticamente significativo.
#
# b) Pregunta de investigación
# “¿Tener un empleo formal está asociado con mayores ingresos laborales,
# una vez que se controla por educación, experiencia, género y área
# urbana/rural, y se corrige la endogeneidad de la educación?”
#
# c) Relevancia de incorporar la nueva variable
# Incluir `formal` es relevante porque:
#  - Captura la segmentación del mercado laboral entre sector formal e
#    informal, que es especialmente importante en economías como la
#    boliviana, donde la informalidad es alta.
#  - Permite estimar una “prima salarial formal”, es decir, cuánto más
#    ganan los trabajadores formales respecto a los informales con
#    características observables similares.
#  - Aporta información útil para el diseño de políticas públicas
#    relacionadas con formalización, protección social y regulación del
#    mercado laboral.
#  - Ayuda a separar el efecto de la educación del efecto institucional
#    de estar en un empleo formal, evitando atribuirle a la educación
#    diferencias salariales que podrían responder en realidad a la
#    formalidad.
#
# d) Posibles fuentes de endogeneidad o error de medida en la nueva variable
# 1) Endogeneidad por selección:
#    - Trabajadores con mayor habilidad no observada, motivación, redes
#      sociales o mejor salud tienen más probabilidad de acceder a empleos
#      formales y, al mismo tiempo, mayores salarios. Si estas variables
#      no observadas no se controlan explícitamente, `formal` estará
#      correlacionada con el término de error y su coeficiente β5 será
#      potencialmente sesgado.
#
# 2) Endogeneidad por simultaneidad:
#    - En algunos casos, una mayor estabilidad y nivel salarial podría
#      facilitar la formalización (por ejemplo, empresas formalizan a los
#      trabajadores cuando crecen), generando una relación bidireccional
#      entre ingreso y formalidad.
#
# 3) Error de medida:
#    - Los encuestados pueden no tener claro si su empleo es “formal” o
#      “informal” (por ejemplo, si tienen contratos verbales, si la
#      empresa está registrada pero no aporta a la seguridad social, etc.).
#      Esto puede generar clasificación errónea (misclassification) de la
#      variable `formal`.
#    - Un error de medida no clásico (correlacionado con el ingreso) puede
#      introducir sesgo adicional en β5.
#
# En resumen, la variable `formal` es conceptualmente muy importante para
# explicar el ingreso laboral, pero también puede ser endógena. En una
# extensión futura del trabajo sería deseable buscar instrumentos
# plausibles para formalidad (por ejemplo, características del mercado
# laboral local o de la regulación) para estimar de manera más convincente
# la prima salarial asociada al empleo formal.
