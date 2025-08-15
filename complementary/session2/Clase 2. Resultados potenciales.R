"Econometría Avanzada, 2025-2
Elaborado por: Danilo Aristizabal, Universidad de los Andes
Creado: 20250814

En este script vamos a simular el Doctor Perfecto y calcular el ATE, ATT, ATU. También vamos a estimar (MCO) el
efecto de ser hospitalizado sobre la salud usando datos simulados y proponemos un ejercicio para mostrar la consistencia 
del estimador de MCO incrementando el tamaño de la muestra."

# Limpiando memoria
cat("\f")
rm(list = ls())  

# Cargamos los paquetes a usar. No siempre vamos a necesitar todos estos paquetes... 
packageList<-c('haven','dplyr','tidyverse', 'sandwich','lmtest', 'writexl')
sapply(packageList,require,character.only=TRUE)

# Establecemos el directorio de trabajo
directorio <- "C:/Users/DANILO/Dropbox (Uniandes)/" # Danilo
setwd(directorio)
getwd()

# Establecemos una semilla para poder replicar luego la simulacion
set.seed(31416)

# Parámetros
alpha <- 0.3
delta <- 0.07
beta <- 0.25
N <- 1000000

# Simulación
x <- rchisq(N, df = 3)
u0 <- rnorm(N)
u1 <- rnorm(N)

"Para D = 0"
Y0 <- alpha + beta * x + u0
"Para D = 1"
Y1 <- alpha + delta + beta * x + u1

D <- as.integer(Y1 > Y0)

Y <- Y1 * D + Y0 * (1 - D)
Ti <- Y1 - Y0

"Incluyamos estas variables simuladas en un data frame"
data <- data.frame(Y1, Y0, D, Y)

# Instalamos el paquete de ggplot si no lo tenemos instalado
install.packages("ggplot2")

# Cargamos el paquete de ggplot
library(ggplot2)

# Graficar
densidad <- ggplot(data, aes(x = data$Y)) + 
  geom_histogram(aes(y = ..density..), bins = 50, fill = "gray", alpha = 0.5, color = NA) + 
  geom_density(color = "black", size = 1) +
  theme_minimal(base_size = 14) + theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid = element_blank()) +
  labs(
    title = "Distribución de Y",
    x = "Y",
    y = "Densidad"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  guides(fill = guide_legend(title = NULL))

ggsave(densidad, file ="./Densidad.pdf",width = 8,height = 10)

# Estimaciones
ATE <- mean(Ti)
ATT <- mean(Ti[D == 1])
ATU <- mean(Ti[D == 0])

# Regresiones
modelo1 <- lm(Y ~ D)

"Otra forma de hacer exactamente lo mismo, pero usando el dataframe"
modelo2 <- lm(Y ~ D, data)

" ¿Donde guarda R los coeficientes y los residuales del modelo?
coef(modelo): Coeficientes estimados.
residuals(modelo): Residuos del modelo.
fitted(modelo): Valores ajustados.
summary(modelo): Estadísticas completas del modelo (errores estándar, valor t, valor p, R cuadrado, etc.).
"
summary(modelo1)

"Guardemos el parametro de interes de minimo cuadrados ordinarios"
MCO <- coef(lm(Y ~ D))[2]

"¿Que pasa si incluimos controles?"
modelo3 <- lm(Y ~ D + x)
MCO_X <- coef(lm(Y ~ D + x))[2]

summary(modelo3)
"El mismo modelo Usando erroes estandar robustos"
coeftest(modelo3, vcov = vcovHC(modelo3, type = "HC1"))

# Tratamiento aleatorio
D_r <- rbinom(N, 1, 0.5)
Y_r <- Y1 * D_r + Y0 * (1 - D_r)

MCO_r <- coef(lm(Y_r ~ D_r))[2]
MCO_r_X <- coef(lm(Y_r ~ D_r + x))[2]

# Resultados
resultados <- data.frame(
  Efecto = c("T_i", "ATE", "ATT", "ATU", "MCO", "MCO|X", "MCO (D random)", "MCO (D random)|X"),
  Coeficiente = c(Ti[1], ATE, ATT, ATU, MCO, MCO_X, MCO_r, MCO_r_X)
)
print(resultados)


write_xlsx(resultados, "./resultados.xlsx")


"Ejercicio: 									

Presente en una gráfica cómo a medida que aumenta el tamaño de  			   
muestra, el estimador del ATE cuando el tratamiento es aleatorio se acerca  
cada vez más al valor poblacional de este. 							       
                                                                             
Para esto:                                                                  
1. Creen una matriz vacía con 1000 filas y dos columnas. En la primera 	   
  columna van a guardar el tamaño de muestra de cada iteración y en la     
  segunda el ATE estimado.                                                 
2. Dentro de un loop simulen 1000 veces la muestra de Y1, Y0, Y y D_r tal 
  como fue presentado anteriormente en el este script. En cada iteración, el   
  tamaño de muestra debe incrementar en 100. Es decir, en la primera 	   
  iteración N = 100, en la segunda N = 200, en la tercera N = 300 y así   
  sucesivamente hasta completar las mil iteraciones. Asegúrense de 		   
  guardar el tamaño de muestra de cada iteración en la matriz. Es decir,  
  si están en la iteración 2, el número 200 debe ser guardaro en la        
  posición 2,1 de la matriz.
3. Dentro de cada iteración hagan una regresión de Y contra D_r y x. 	   
  guarden el coeficiente estimado en la fila correspondiente de la segunda 
  columna de la matriz. Es decir, si están en la iteración 10, el 		   
  coeficiente estimado debe ser guardado en la posición 10,2 de la matriz.
4. Limpien su espacio de trabajo con el comando cat(\f).                    
5. Hagan un gráfico de linea (? ggplot) del parámetro estimado (eje y)  
  contra el tamaño de muestra de la estimación (eje x). Agreguen una línea 
  horizontal ubicada en el eje y en la posición 0.071 (el ATE poblacional).
                                                                             
Para revisar el comportamiento de un estimador inconsistente, pueden        
repetir este ejercicio cuando D es asignado según Y1>Y0.					   
"


