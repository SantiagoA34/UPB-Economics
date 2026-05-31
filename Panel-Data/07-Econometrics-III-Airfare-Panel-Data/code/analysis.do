/*
********************************************************
* Econometrics III - Airfare Panel Data Analysis
* Cleaned portfolio version
* Note: the dataset is not included in this repository.
*******************************************************

	OBJETIVO:
	Estimar el efecto de la concentración de mercado (concen) sobre las tarifas
	aéreas (fare), controlando por distancia, utilizando datos de panel de rutas
	aéreas en EE.UU. (1997-2000). Se comparará estimaciones por MCO agrupado,
	efectos aleatorios y efectos fijos.

	MODELO BASE:
	log(fare_it) = η_t + β1*concen_it + β2*log(dist_i) + β3*[log(dist_i)]^2
	               + α_i + u_it

	ESTRUCTURA DEL DO FILE:
		1. Configuración inicial
		2. Carga y exploración de datos
		3. Ejercicio i)   - Pooled OLS con dummies de año
		4. Ejercicio ii)  - Intervalos de confianza (estandar vs robusto)
		5. Ejercicio iii) - Término cuadrático en log(dist)
		6. Ejercicio iv)  - Efectos aleatorios (RE)
		7. Ejercicio v)   - Efectos fijos (FE)
		8. Ejercicio vi)  - Características capturadas por α_i
		9. Ejercicio vii) - Conclusión sobre concentración y tarifas

*/

*===============================================================================
* 1. CONFIGURACIÓN INICIAL
*===============================================================================

	clear all
	macro drop _all
	set more off

	cd "C:\Users\LENOVO\OneDrive\Documentos\STATA"


*===============================================================================
* 2. CARGA Y EXPLORACIÓN DE DATOS
*===============================================================================

	use "AIRFARE.dta", clear
	describe
	summarize

	* --- Estructura del panel ---
	* id identifica cada ruta (1,149 rutas) y year los períodos (1997-2000)
	xtset id year
	xtdescribe
	/*
	Panel balanceado: 1,149 rutas observadas en 4 años = 4,596 observaciones.
	Variables clave:
	  - fare: tarifa promedio de solo ida en dólares.
	  - concen: concentración de mercado (fracción de mercado de la aerolínea más
	    grande en la ruta). Varía entre 0.16 y 1.
	  - dist: distancia en millas entre los dos aeropuertos de la ruta.
	  - lfare, ldist, ldistsq: transformaciones logarítmicas ya incluidas.
	  - y98, y99, y00: dummies de año (1997 es el año base).
	*/

	* Estadísticos descriptivos de las variables del modelo
	sum lfare concen ldist ldistsq y98 y99 y00

	* Variación within vs between de las variables clave
	xtsum concen lfare
	/*
	Es importante distinguir la variación between (entre rutas) de la variación
	within (dentro de una misma ruta a lo largo del tiempo), donde este ultimo lo tomaremos como los "individuos". Para concen, la
	variación between (0.189) es mucho mayor que la within (0.054), lo que
	anticipa que los estimadores FE y RE podrían diferir. Para lfare, el patrón es
	similar: between (0.425) >> within (0.099).
	*/


*===============================================================================
* 3. EJERCICIO i) - POOLED OLS CON DUMMIES DE AÑO
*===============================================================================

	/*
	Estimamos el modelo por MCO agrupado (pooled OLS), ignorando la estructura
	de panel (tratando las 4,596 observaciones como independientes). Incluimos
	dummies de año para capturar tendencias temporales comunes a todas las rutas
	(η_t). La categoría base es 1997.
	*/

	reg lfare concen ldist ldistsq y98 y99 y00
	estimates store pooled_ols

	/*
	RESULTADOS POOLED OLS:

	  β1 (concen) = 0.360, significativo al 1% (t = 11.98, p < 0.001).
	  R² = 0.406: el modelo explica el 40.6% de la variación en log(fare).

	  Interpretación: Si Δconcen = 0.10, el incremento porcentual estimado en
	  fare es aproximadamente:
	    %Δfare ≈ 100 * β1 * Δconcen = 100 * 0.360 * 0.10 ≈ 3.60%

	  Es decir, un aumento de 0.10 en la concentración de mercado se asocia con
	  un incremento de aproximadamente 3.6% en la tarifa aérea. Este resultado se puede ver en la realidad, cuando hay mayor poder de mercado, los precios son mayores.
	*/


*===============================================================================
* 4. EJERCICIO ii) - INTERVALOS DE CONFIANZA (ESTANDAR VS ROBUSTO)
*===============================================================================

	/*
	El IC al 95% estandar de MCO asume errores homocedásticos e independientes.
	Con datos de panel, esto probablemente no se cumple: (1) las observaciones
	de una misma ruta están correlacionadas en el tiempo (correlación serial),
	y (2) la varianza del error puede diferir entre rutas (heterocedasticidad).
	Ambos problemas invalidan los errores estándar usuales, haciendo que el IC
	sea demasiado estrecho (subestimación).
	*/

	* --- IC estándar (errores estándar no robustos) ---
	qui reg lfare concen ldist ldistsq y98 y99 y00
	display "IC 95% usual para β1:"
	display "  Límite inferior: " _b[concen] - invttail(e(df_r), 0.025)*_se[concen]
	display "  Límite superior: " _b[concen] + invttail(e(df_r), 0.025)*_se[concen]

	* --- IC robusto (errores estándar clusterizados por ruta) ---
	/*
	Usamos errores estándar clusterizados por id (ruta) en lugar de simplemente
	robustos (vce(robust)). El clustering por ruta corrige tanto la
	heterocedasticidad como la correlación serial dentro de cada ruta, que es
	exactamente el problema que tenemos con datos de panel.
	*/
	reg lfare concen ldist ldistsq y98 y99 y00, vce(cluster id)
	estimates store pooled_robust

	display ""
	display "IC 95% robusto (cluster por ruta) para β1:"
	display "  Límite inferior: " _b[concen] - invttail(e(df_r), 0.025)*_se[concen]
	display "  Límite superior: " _b[concen] + invttail(e(df_r), 0.025)*_se[concen]

	/*
	COMPARACIÓN DE INTERVALOS:
	  IC estándar:   [0.301, 0.419]  → ancho = 0.118
	  IC robusto: [0.245, 0.475]  → ancho = 0.230

	El IC robusto es casi el doble de ancho que el estándar. El error estándar
	pasa de 0.030 (usual) a 0.059 (cluster), casi duplicándose. Esto confirma
	que los errores estándar usuales subestiman la verdadera incertidumbre.

	Podemos atribuir esto a que la correlación serial positiva dentro de cada ruta infla
	artificialmente la precisión aparente de las estimaciones. Los errores
	clusterizados corrigen este sesgo, dando un IC más transparente.

	β1 sigue siendo significativo con errores robustos (t = 6.15, p < 0.001),
	pero la inferencia basada en errores estándar usuales no es confiable
	para datos de panel, com desarrollamos a lo largo del 3er término de la clase.
	*/


*===============================================================================
* 5. EJERCICIO iii) - TÉRMINO CUADRÁTICO EN log(dist)
*===============================================================================

	/*
	Nuestro modelo incluye tanto ldist como ldistsq = [log(dist)]^2. Esto permite
	una relación no lineal (en forma de U) entre log(fare) y log(dist).

	La derivada parcial de log(fare) respecto a log(dist) es:
	  ∂log(fare)/∂log(dist) = β2 + 2*β3*log(dist)

	Esta derivada es cero en el punto de giro:
	  log(dist*) = -β2 / (2*β3)
	  dist* = exp(-β2 / (2*β3))
	*/

	qui reg lfare concen ldist ldistsq y98 y99 y00
	local b_ldist = _b[ldist]
	local b_ldistsq = _b[ldistsq]

	display "β2 (ldist)   = " `b_ldist'
	display "β3 (ldistsq) = " `b_ldistsq'

	local turning_log = -`b_ldist' / (2*`b_ldistsq')
	local turning_dist = exp(`turning_log')

	display ""
	display "Punto de giro en log(dist): " `turning_log'
	display "Punto de giro en dist (millas): " `turning_dist'

	* --- ¿Está dentro del rango de los datos? ---
	sum dist
	display ""
	display "Rango de dist en los datos: [" r(min) ", " r(max) "]"

	/*
	INTERPRETACIÓN DEL TÉRMINO CUADRÁTICO:
	  β2 = -0.902, β3 = 0.103

	  Punto de giro: log(dist*) = -(-0.902)/(2*0.103) = 4.376
	                 dist* = exp(4.376) ≈ 79.5 millas

	Dado que β2 < 0 y β3 > 0, la relación entre log(fare) y log(dist) tiene
	forma de U: para distancias muy cortas (< 80 millas), la tarifa decrece
	con la distancia. A partir del punto de giro (~80 millas), la relación
	se vuelve positiva: más distancia → mayor tarifa.

	El punto de giro (79.5 millas) está FUERA del rango de los datos por
	debajo: la distancia mínima observada es 95 millas. Esto significa que,
	en la práctica, para TODAS las rutas de nuestra muestra la relación entre
	log(fare) y dist es positiva y creciente. El tramo decreciente (< 80
	millas) es un proceso creado por el modelo, no algo que observamos en los
	datos.
	*/


*===============================================================================
* 6. EJERCICIO iv) - EFECTOS ALEATORIOS (RE)
*===============================================================================

	/*
	El estimador de efectos aleatorios (RE) asume que el efecto inobservable
	de ruta α_i no está correlacionado con las variables explicativas. Bajo
	este supuesto, RE es más eficiente que FE porque utiliza tanto la variación
	within como la between.

	RE estima el modelo con cuasi-media:
	  (y_it - θ*ȳ_i) = (1-θ)*β0 + β1*(x_it - θ*x̄_i) + ... + (v_it - θ*v̄_i)

	donde θ ∈ [0,1]. Si θ=1, RE = FE (within). Si θ=0, RE = OLS.
	*/

	xtreg lfare concen ldist ldistsq y98 y99 y00, re
	estimates store re_model

	* Cálculo explícito de θ̂
	local sigma_u = e(sigma_u)
	local sigma_e = e(sigma_e)
	local T = 4
	local theta = 1 - `sigma_e' / sqrt(`T'*`sigma_u'^2 + `sigma_e'^2)
	display "θ̂ (theta) = " `theta'

	/*
	RESULTADO RE:
	  β1 (concen) = 0.209, significativo al 1% (z = 7.88, p < 0.001).

	  El coeficiente cae de 0.360 (pooled OLS) a 0.209 (RE). Esto ocurre
	  porque RE pondera la variación within, o sea, dentro de cada ruta en el tiempo,
	  con más peso que pooled OLS, y la variación within produce una estimación
	  menor.

	  σ_α = 0.319, σ_u = 0.107 → rho = 0.900, es decir, el 90% de la varianza
	  del error compuesto se debe a heterogeneidad entre rutas.

	  θ̂ se puede calcular como: θ = 1 - σ_u / sqrt(T*σ²_α + σ²_u).
	  Con T=4, σ_α=0.319, σ_u=0.107: θ̂ ≈ 0.834. Un θ̂ alto (cercano a 1)
	  implica que RE se aproxima a FE, lo que explica por qué β1 de RE (0.209)
	  está más cerca de FE que de pooled OLS.
	*/


*===============================================================================
* 7. EJERCICIO v) - EFECTOS FIJOS (FE)
*===============================================================================

	/*
	El estimador de efectos fijos (FE / within) elimina α_i al restar la media
	temporal de cada ruta. No requiere que α_i sea independiente de los
	regresores, por lo que es consistente incluso si hay variables omitidas
	correlacionadas con concen.

	*/

	xtreg lfare concen ldist ldistsq y98 y99 y00, fe
	estimates store fe_model

	/*
	RESULTADO FE:
	  β1 (concen) = 0.169, significativo al 1% (t = 5.74, p < 0.001).

	  Las variables ldist y ldistsq son automáticamente eliminadas (omitted)
	  porque la distancia de una ruta no cambia en el tiempo: al restar la
	  media temporal, estas variables constantes desaparecen.

	  R² within = 0.135: la variación temporal en concen (y las dummies de año)
	  explica el 13.5% de la variación within de log(fare).

	  ¿Por qué FE (0.169) y RE (0.209) dan estimaciones relativamente similares?
	  La clave está en θ̂ ≈ 0.834 de la estimación RE. Como θ̂ es cercano a 1,
	  la transformación cuasi-media de RE es prácticamente igual a la
	  transformación within de FE. Esto ocurre porque σ²_α >> σ²_u: la
	  heterogeneidad entre rutas (σ_α = 0.434) domina ampliamente al error
	  clusterizado (σ_u = 0.107), lo que hace que RE dé poco peso a la
	  variación between y se acerque al estimador within (FE).
	*/

	* --- Comparación de estimaciones ---
	estimates table pooled_ols re_model fe_model, b se stats(N r2 r2_a)


*===============================================================================
* 8. EJERCICIO vi) - CARACTERÍSTICAS CAPTURADAS POR α_i
*===============================================================================

	/*
	El efecto fijo de ruta α_i captura todas las características de la ruta
	que no varían en el tiempo y que afectan la tarifa. Dos ejemplos:

	(1) TAMAÑO Y ACTIVIDAD ECONÓMICA DE LAS CIUDADES CONECTADAS:
	    Rutas que conectan grandes centros económicos (En nuestro país podemos verlo con la ruta La Paz - Santa Cruz) tienen mayor demanda empresarial, más viajeros de negocios
	    dispuestos a pagar tarifas altas, y mayor infraestructura aeroportuaria (en la teoría, fuera de Bolivia si se puede ver un poco más desarrollado esto).
	    Estas características son estables en el tiempo y afectan tanto el nivel
	    de tarifas como potencialmente la concentración (ciudades grandes pueden
	    atraer más aerolíneas o, al contrario, tener slots limitados que
	    favorecen la concentración).

	(2) PRESENCIA DE AEROLÍNEAS DE BAJO COSTO (LOW-COST CARRIERS):
	    Algunas rutas son atendidas por aerolíneas de bajo costo,
	    lo cual reduce tanto la tarifa promedio como la concentración de mercado
	    (al agregar un competidor fuerte). Si una ruta es atractiva para low-cost
	    carriers depende de factores geográficos y regulatorios relativamente
	    fijos en el corto plazo.

	Ambas características podrían estar correlacionadas con concen_it: rutas
	entre grandes ciudades o con presencia de low-cost carriers tienden a tener
	menor concentración. Si no se controla por α_i (como en pooled OLS), la
	estimación de β1 estaría sesgada porque concen captura parcialmente el
	efecto de estas características omitidas.
	*/


*===============================================================================
* 9. EJERCICIO vii) - CONCLUSIÓN SOBRE CONCENTRACIÓN Y TARIFAS
*===============================================================================

	/*
	¿Estamos convencidos de que mayor concentración incrementa las tarifas?

	La evidencia apunta en esa dirección, pero con matices:

	(a) En los tres modelos (pooled OLS, RE, FE), β1 es positivo y
	    estadísticamente significativo: mayor concentración se asocia con
	    mayores tarifas.

	(b) La estimación de FE es la más confiable porque controla por todas las
	    características fijas de la ruta que podrían sesgar el resultado. Es
	    nuestra MEJOR ESTIMACIÓN: bajo FE, un aumento de 0.10 en concen
	    incrementa fare en aproximadamente 100*β1_FE*0.10 %.

	(c) La diferencia entre pooled OLS y FE sugiere que sí había sesgo por
	    variables omitidas: las características fijas de la ruta (tamaño de
	    ciudades, presencia de low-cost) afectaban tanto concen como fare.

	(d) Limitación: incluso FE solo controla por heterogeneidad fija. Si hay
	    shocks temporales específicos de la ruta que afectan simultáneamente
	    la concentración y las tarifas ( como las fusiones de aerolíneas), la estimación FE podría seguir sesgada. Para una
	    identificación causal más fuerte se necesitaría un instrumento para
	    concen.

	RESUMEN CUANTITATIVO:
	*/
	qui xtreg lfare concen ldist ldistsq y98 y99 y00, fe
	display "=== MEJOR ESTIMACIÓN (Efectos Fijos) ==="
	display "β1 (concen)    = " _b[concen]
	display "Si Δconcen = 0.10, %Δfare ≈ " _b[concen]*0.10*100 "%"
	display ""
	display "Conclusión: Sí, mayor concentración incrementa las tarifas aéreas."
	display "La estimación FE es preferible porque controla por la heterogeneidad"
	display "inobservable fija de cada ruta."


*===============================================================================
* FIN DEL DO FILE
*===============================================================================
