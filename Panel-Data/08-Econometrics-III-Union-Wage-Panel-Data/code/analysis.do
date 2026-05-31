*******************************************************
* Econometrics III - Union Wage Panel Data Final Project
* Cleaned portfolio version
* Note: the dataset is not included in this repository.
*******************************************************

*******************************************************
* 1. CARGA DE BASE DE DATOS
*******************************************************

use wagepan.dta, clear

describe
summarize

*******************************************************
* 2. DECLARACION DEL PANEL
*******************************************************

xtset nr year

xtdescribe

*******************************************************
* 3. ESTADISTICA DESCRIPTIVA
*******************************************************

* Estadisticas descriptivas generales
summarize lwage union exper expersq married hours poorhlth

* Tablas de frecuencia
tab union
tab married
tab poorhlth

* Histograma
histogram lwage, normal color(pink) fcolor(pink%40) lcolor(maroon) title("Distribución del logaritmo del salario") xtitle("Log(wage)") ytitle("Frecuencia") graphregion(color(white))

* Boxplot
graph box lwage, over(union) box(1, fcolor(pink)) medtype(line) title("Distribución salarial según sindicalización") ytitle("Log(wage)")

* Grafico de barras
graph bar (mean) union, over (year) bar(1, color(pink)) blabel(bar, color(maroon)) title ("Proporción promedio de sindicalización por ano") ytitle("Promedio de union") graphregion(color(white))

* Descomposicion within y between
xtsum lwage union exper expersq married hours poorhlth

*******************************************************
* 4. MODELO POOLED OLS
*******************************************************

reg lwage union exper expersq married hours poorhlth occ1 occ2 occ3 occ4 occ5 occ6 occ7 occ8 agric bus construc ent fin manuf min nrthcen nrtheast south i.year, vce(cluster nr)

*******************************************************
* 5. MODELO DE EFECTOS FIJOS (FE)
*******************************************************

xtreg lwage union exper expersq married hours poorhlth occ1 occ2 occ3 occ4 occ5 occ6 occ7 occ8 agric bus construc ent fin manuf min nrthcen nrtheast south i.year, fe vce(cluster nr)

*******************************************************
* 6. MODELO DE EFECTOS ALEATORIOS (RE)
*******************************************************

xtreg lwage union exper expersq married hours poorhlth occ1 occ2 occ3 occ4 occ5 occ6 occ7 occ8 agric bus construc ent fin manuf min nrthcen nrtheast south i.year, re theta

*******************************************************
* 7. PRUEBA DE HAUSMAN
*******************************************************

xtreg lwage union exper expersq married hours poorhlth occ1 occ2 occ3 occ4 occ5 occ6 occ7 occ8 agric bus construc ent fin manuf min nrthcen nrtheast south i.year, fe
estimates store FE

xtreg lwage union exper expersq married hours poorhlth occ1 occ2 occ3 occ4 occ5 occ6 occ7 occ8 agric bus construc ent fin manuf min nrthcen nrtheast south i.year, re
estimates store RE

hausman FE RE

*******************************************************
* 8. PRUEBA LM BREUSCH-PAGAN
*******************************************************

xtreg lwage union exper expersq married hours poorhlth occ1 occ2 occ3 occ4 occ5 occ6 occ7 occ8 agric bus construc ent fin manuf min nrthcen nrtheast south i.year, re
xttest0

*******************************************************
* 9. PRUEBA DE HETEROCEDASTICIDAD
*******************************************************

ssc install xttest3
xtreg lwage union exper expersq married hours poorhlth occ1 occ2 occ3 occ4 occ5 occ6 occ7 occ8 agric bus construc ent fin manuf min nrthcen nrtheast south i.year, fe
xttest3

ssc install estout

*******************************************************
* 10. TABLA COMPARATIVA DE RESULTADOS
*******************************************************

* Pooled OLS
reg lwage union exper expersq married hours poorhlth occ1 occ2 occ3 occ4 occ5 occ6 occ7 occ8 agric bus construc ent fin manuf min nrthcen nrtheast south i.year, vce(cluster nr)
eststo pooled

* Efectos fijos
xtreg lwage union exper expersq married hours poorhlth occ1 occ2 occ3 occ4 occ5 occ6 occ7 occ8 agric bus construc ent fin manuf min nrthcen nrtheast south i.year, fe vce(cluster nr)
eststo fe

* Efectos aleatorios
xtreg lwage union exper expersq married hours poorhlth occ1 occ2 occ3 occ4 occ5 occ6 occ7 occ8 agric bus construc ent fin manuf min nrthcen nrtheast south i.year, re vce(cluster nr)
eststo re

* Tabla comparativa
esttab pooled fe re, se star(* 0.10 ** 0.05 *** 0.01) r2 ar2
