#-------------------------------------------------------------------------------
# Proyecto:     Econometría Avanzada - Semana 1
# Do Autor:     Clase Complementaria
# Descripción:  Semana 1 - Introuducción a Stata y la replicación
# Fecha:        08-08-2025
#-------------------------------------------------------------------------------

# Contenido
  # 0: Organización según Gentzkow y Shapiro (2014)
  # 1: Configuración general (paths, log, etc.)
  # 2: ¿Cómo funciona Stata?
  # 3: Carga y limpieza de datos
  # 4: Creación de variables 
  # 5: Condicionales
  # 6: Macros y Loops


#-------------------------------------------------------------------------------
# 0. Organización según Gentzkow y Shapiro (2014)
#-------------------------------------------------------------------------------

# Estructura sugerida del proyecto:
# Proyecto/
# ├── 0_raw/         # Datos originales
# ├── 1_clean/       # Datos limpios
# ├── 2_output/      # Resultados (gráficos, tablas, logs)
# ├── 3_code/        # Scripts R organizados por módulo

#-------------------------------------------------------------------------------
# 1. Configuración General
#-------------------------------------------------------------------------------

# Limpiar ambiente
rm(list = ls())

# Paquetes necesarios
packages <- c("readxl", "dplyr", "ggplot2", "haven")
lapply(packages, require, character.only = TRUE)

# Establecer path dinámico (ajustar según el usuario)
user <- Sys.info()[["user"]]
if (user == "danielavlasak") {
  path <- "~/Library/CloudStorage/OneDrive-UniversidaddelosAndes/ANDES/compl_econ_avz/semana1_stata"
} else {
  path <- "_______________RUTA__________/semana1_R"
}
setwd(path)
getwd()
list.files()

#-------------------------------------------------------------------------------
# 2. ¿Cómo funciona R?
#-------------------------------------------------------------------------------

# Script vs consola (esto es un script .R)
# Log de resultados (puedes usar sink para guardar output en txt)

sink("2_output/Econometría_Avz_log.txt")
cat("Clase 1 - Econometría Avanzada\n")
2 + 2
sink()

# Ayuda
help(mean)

# o usar RDocumentation online
# https://www.rdocumentation.org/

# Instalar paquetes
# install.packages("nombre")
# Ejemplo:
# install.packages("mFilter")  # para Hodrick-Prescott
library(mFilter)
library(dplyr)

#-------------------------------------------------------------------------------
# 3. Carga y limpieza de datos
#-------------------------------------------------------------------------------

# Leer Excel
library(readxl)
df <- read_excel("0_raw/CasosDengue.xlsx")


# Inspección de datos
str(df) # Ver el tipo de variables
head(df) # Visualizar en la consola unos datos
View(df) # Visualizar datos, equivalente a browse en Stata

# Renombrar variables
df <- df %>%
  rename(
    area = zona,
    population = poblacion,
    week = semana
  )

# Etiquetas de variables (en R es más común usar comments o attr)
attr(df$week, "label") <- "Semana Epidemiológica"
attr(df, "label") <- "Casos de dengue 2011"

# Ordenar columnas
df <- df %>% select(week, departamento, casos, everything())

# Ordenar observaciones
df <- df %>% arrange(week)
df %>% slice(1:4)

df <- df %>% arrange(desc(week))
df %>% slice(1:4)

df <- df %>% arrange(departamento, zona, week)

#-------------------------------------------------------------------------------
# 4. Creación de variables
#-------------------------------------------------------------------------------

df <- df %>%
  mutate(
    incidencia = casos / population * 100000,
    casos2 = casos^2,
    log_incidencia = log(incidencia),
    casos_brote = ifelse(casos >= 50 & !is.na(casos), 1, ifelse(is.na(casos), NA, 0)),
    id_sem = ifelse(week <= 10, 1, 0)
  )

# Recode (más explícito)
df$casos_brote <- ifelse(is.na(df$casos_brote), 0, df$casos_brote)

# Medias agrupadas
df <- df %>%
  group_by(departamento) %>%
  mutate(
    prome_dpto_casos = mean(casos, na.rm = TRUE)
  ) %>%
  ungroup()

# Guardar base limpia
saveRDS(df, "1_clean/base.rds")

#-------------------------------------------------------------------------------
# 5. Condicionales
#-------------------------------------------------------------------------------

a <- 3
if (a == 1) {
  cat("a es 1\n")
} else if (a == 2) {
  cat("a es 2\n")
} else {
  cat("a no es 1 ni 2\n")
}

#-------------------------------------------------------------------------------
# 6. Macros y Loops
#-------------------------------------------------------------------------------

# En R usamos vectores en lugar de macros
prueba <- c("Antioquia", "Santander", "Meta", "Tolima", "Vichada")
print(prueba)

# Bucles con for
for (persona in c("Manuel", "Juan Felipe", "Danilo", "Rafael", "Daniela")) {
  print(persona)
}

n <- 0
for (persona in prueba) {
  n <- n + 1
  cat(n, ". ", persona, "\n")
}


j <- 0
for (i in 1:5) {
  j <- j + i
  print(j)
}

i <- 0
while (i < 5) {
  i <- i + 1
  print(i)
}

#-------------------------------------------------------------------------------
# 7. Figuras y Tablas (más adelante)
#-------------------------------------------------------------------------------

# Esto se cubre con ggplot y otros paquetes como stargazer o modelsummary

