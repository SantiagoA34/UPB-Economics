*******************************************************
* Econometrics III - Applied Stata Practice
* Cleaned portfolio version
* Note: the dataset is not included in this repository.
*******************************************************
use "EH2024_Persona"

* Asignar 1 si la persona está afiliada a algún seguro
replace seguro = 1 if s02a_01a != 5

* Asignar 0 si la persona no está afiliada
replace seguro = 0 if s02a_01a == 5

* Etiquetar la variable
label define seguro_lbl 0 "No afiliado" 1 "Afiliado"
label values seguro seguro_lbl


*EDAD s01a_03
*ESTADO CIVIL s01a_10
*USA SOLUCIONES CASERAS s02a_02f
*AUTOMEDICACION s02a_02g
*ACUDIO A UN CENTRO EN LOS ULTIMOS 12 MESES s02a_02he
*CUANTOS HIJOS VIVOS NACIDOS TIENE s02b_09a
*NIVEL DE EDUCACION s03a_02a
*INGRESO LABORAL PRINCIPAL EN BS s04d_22a
*TRABAJO EN LA ULTIMA SEMANA s04a_03 (CREO)
*AFILIADO A LA GESTORA O NO s04f_35
*RECIBE JUBILACION s05a_01a
*SE PUEDE USAR LA PEA CREO
*YLAB
*YHOG
*cobersalud
*niv_ed_g
*estrato
