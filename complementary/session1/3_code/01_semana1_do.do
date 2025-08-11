/*==============================================================================
 Proyecto:     Econometría Avanzada - Semana 1
 Do Autor:     Clase Complementaria
 Descripción:  Semana 1 - Introducción a Stata y la replicación
 Fecha:        08-08-2025
==============================================================================*/

clear all
cap log close
set more off, perm
cls

/* Contenido
 0: Organización según Gentzkow y Shapiro (2014)
 1: Configuración general (paths, log, etc.)
 2: ¿Cómo funciona Stata?
 3: Carga y limpieza de datos
 4: Creación de variables 
 5: Condicionales
 6: Macros y Loops
*/

* -------------------------------------------------------------------------------
* 0. Organización según Gentzkow y Shapiro (2014):
* -------------------------------------------------------------------------------

/* Antes de comenzar a utilizar Stata es importante organizar la carpeta sobre la 
	cual vamos a hacer el análisis estadístico. Al trabajar en Stata tipicamente:

	Si lee la guía de Gentzkow y Shapiro (2014) para desarrollar su trabajo de 
	una forma que sea posible replicar van a ver que recomiendan una estrucutra 
	como la siguiente:
			
			Proyecto /
			│
			├── 0_raw/                # Datos originales sin procesar
			├── 1_clean/              # Datos limpios/listos para análisis
			├── 2_output/             # Resultados (tablas, figuras, logs)
			├── 3_code/               # Archivos .do organizados por módulos
			│   ├── 00_master.do
			│   ├── 01_clean.do
			│   ├── 02_analysis.do
			│   └── 03_figures.do
			
	No es necesario que hagan diferentes dofiles para los talleres, realmente va 
	a depender del tipo de puntos, pero una estrucutra así podría ser útil para 
	su trabajo final. La recomendación de los autores es tener dofiles cortos 
	con un objetivo claro.
	
	*/
	
	
		
* -------------------------------------------------------------------------------	
*  1: Set up carpeta de trabajo y directorios
* -------------------------------------------------------------------------------

// Esto ya lo inluimos arriba e idealmente debe ir al comeinzo del dofile
clear all
cap log close
set more off, perm
cls

*) Directorio dinámico para que se pueda correr el dofile
		
	if "`c(username)'"=="danielavlasak" {
	global path "/Users/danielavlasak/Library/CloudStorage/OneDrive-UniversidaddelosAndes/ANDES/compl_econ_avz/semana1_stata"
		}
	if "`c(username)'"=="TU_USUARIO" {
	global path "_______________RUTA__________/semana1_stata"
		}

	cd "$path"

	* En qué directorio estamos trabajando	
	pwd
		
	* Archivos en el directorio
	dir
	
	
* -------------------------------------------------------------------------------	
* 2. ¿Cómo funciona Stata?
* -------------------------------------------------------------------------------

*) Cómo abrir Stata y el do-file de la clase 
	
	*) Ventanas en Stata
			
		* Ventana de comandos
		* Ventana de resultados
		* Ventana de variables
		* Ventana de propiedades
		* Ventana de revisión
		* Ventana de visualizar datos
		
	*) Dos tipos de archivos: do-files y log-files
		* Do-file
		* Log-files
		
	
	log using "2_output/Econometría_Avz_`c(current_date)'.log", replace

	display "Clase 1- Econometría Avanzada"
	di 2+2
	sysuse dir /* Bases de datos guardadas										*/

	log close 
	translate "2_output/Econometría_Avz_`c(current_date)'.log" 				    ///
		"2_output/Econometría_Avz_`c(current_date)'.pdf", replace 

*) ¿Cómo buscar ayuda? 
	
	* Helpfiles 
	help tabulate oneway
	search tabulate	/* Busca todos los help/otros files que mencionan 'tabulate'*/

		* 1ro: Nombre del comando.
		* 2do: Los argumentos obligatorios.
		* 3ro: Entre llaves "[]" los argumentos opcionales.
		* 4to: Después de la coma (,) las opciones (también en []).
		* 5to: Descripción de todas las opciones.
	
	* Internet: Googlear la pregunta (en inglés)
	* 		i.e. Stata list, stackoveflow
	*		ChatGPT puede ser útil, aunque comete muchos errores en Stata
	
	* Revisar en los links de ayuda de stata o en do de Outputs (Bloque Neón).
	
	* Preguntarle a cualquier miembro del equipo
	
*) Búsqueda e instalación de paquetes
ssc install hprescott /* Official: Instalación de paquetes 						*/
ssc describe hprescott 
findit hodrick prescott /* User written: Busqueda de paquetes 					*/

* -------------------------------------------------------------------------------
*  3: Carga y limpieza de datos
* -------------------------------------------------------------------------------
		
*) Excel
import excel "0_raw/CasosDengue.xlsx", firstrow clear 
	/* "firstrow" la primela fila del excel son los nombres de las variables	*/
	
	
* Similarmente:	

	* Para .csv usar el comando "import delimited"
	* Para bases de Stata (.dta) usar el comando "use"
	
	
*) compress+save: guardar base de datos
import excel "0_raw/CasosDengue.xlsx", firstrow clear
	
	* Guardar base optimizando espacio
	compress

	* opción "replace" para sobreescribir
	save "0_raw/base.dta", replace // vamos a guardar esta base preliminarmente en formato dta
		
*) browse and desribe: Exploración de los datos
		
	* Descripción del contenido
	describe
	
	* Ver de la base de datos
	br
	

*) Organización de datos	
use "0_raw/base.dta", clear // abrir bases en el formato nativo de stata
	
	* Renombrar variables
	rename area zona
	ren (population week) (poblacion semana)
		
	* Poner etiquetas a variables, datos y valores
	label var semana "Semana Epidemiologica"
	label data "Bases de datos sobre casos de Dengue en 2011"
	label define unoatres 1 "Uno" 2 "Dos" 3 "Tres", modify
	label values semana unoatres
	tab semana

	* Ordenar VARIABLES en la base.
	order semana departamento casos	  
	des
	
	* Ordenar OBSERVACIONES. Menor a Mayor
	sort semana					  
	list semana in 1/4
	
	* Ordenar OBSERVACIONES. Mayor a Menor
	gsort - semana 	

	
	list semana in 1/4
	sort departamento zona semana
	
* -------------------------------------------------------------------------------
* 4. Creación de variables
* -------------------------------------------------------------------------------

*) Creación básica
gen incidencia = casos/poblacion * 100000 /* por cada 100,000 habitantes 		*/
gen casos2 = casos^2
gen log_incidencia = log(incidencia)
	
*) Variables con condiciones
gen casos_brote = (casos >= 50 & casos != .)
tab casos_brote
cap drop casos_brote
gen casos_brote = cond(casos >= 50 & casos != .,1,cond(casos == .,.,0))

gen id_sem=cond(semana<=10,1,0)

*) Reemplazos y recodificaciones
*replace casos_brote = 0 if casos_brote == .
recode casos_brote (. = 0)

*) egen 
egen promedio_casos = mean(casos)		

bysort departamento: egen prome_dpto_casos = mean(casos)

*) Cuardar versión limpia del dta en la carpeta clean
save "1_clean/base.dta", replace 


* -------------------------------------------------------------------------------
* 5. Condicionales
* -------------------------------------------------------------------------------

*) if 
if 2==2 {
	di "Hola"
}

*) else if y else
scalar a=3
if a==1 {
	di "a es 1"	
}
else if a==2 {
	di "a es 2"
}
else {
	di "a no es 1 ni 2"
}

* -------------------------------------------------------------------------------
* 6. Macros y Loops
* -------------------------------------------------------------------------------


*) global
global prueba "Antioquia Santander Meta Tolima Vichada" 	
di "$prueba"

macro list

*) local
local controles casos incidencia 
di "`controles'"

* Se pueden llamar las variables en globals para luego utilizarlas (practica muy común)
global Y "casos"
global X "poblacion casos"

* así las llamamos posteriormente :) 
sum $X

macro list

*) Loop

*En Stata hay tres tipos de Loops:

	*forvalues: recorre elementos de una lista de números enteros que siguen
	*un patrón definido.
	
	*foreach: recorre elementos de una lista arbitraria
	
	*while: ejecuta un comando hasta que se cumpla una condición lógica


	* Ejemplo 1
	foreach persona in Manuel JuanFelipe Rafael Danilo Daniela {
		di "`persona'"
	}

	* Ejemplo 2
	local n=0
	foreach persona in $prueba {
		local ++n
		di "`n'. `persona'"
	}
	
	* Ejemplo 3
	foreach dep of global prueba{
		di "*********`dep'*********"
		forvalues j = 1(1)5{
			dis "`j'"
		}
	}
	
	
	*Ejemplo 4
	local j=0
	forvalues i=1(1)5 {
		local j=`j'+`i'
		di `j'
		
		
		
	}
	
	
	*Ejemplo 5
	local i=0
	while `i'<5 {
		local ++i
		di `i'
	}
	
	
	


* 7. Figuras y Tablas
* -------------------------------------------------------------------------------
* Este tema lo iremos aprendiendo sobre la marcha.
* Sin embargo, les dejamos un do-file tutuorial de cómo hacer figuras y gráficas
* en Bloque Neón:

	*Links y documentos de ayuda -> Outputs de Stata -> DoFile Outputs.

*********************************************************************************

**  Ejercicio: haga un loop que muestre en la ventana de resultados cada uno   **
** 	de los números del 1 al 10 seguido del texto "es par"/"es impar", según    **
** 	corresponda. Pista: Explore los condicionales if y else. 	               **
			   
*********************************************************************************
