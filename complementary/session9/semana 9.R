#-------------------------------------------------------------------------------
# Proyecto:     Econometria Avanzada - Semana 9
# Do Autor:     Clase Complementaria
# Descripcion:  Variables instrumentales
# Fecha:        10-10-2025
# Nota:         Este c?digo usa AER::ivreg() para estimaci?n IV, que incluye diagn?stico de primera etapa.
#-------------------------------------------------------------------------------

# I. PREPARAR EL ESPACIO DE TRABAJO--------------------------------------------

rm(list = ls())

# Cargamos paquetes 
suppressPackageStartupMessages({
  library(rio)     # Para leer archivos .dta
  library(dplyr)     # Manipulaci?n de datos
  library(ggplot2)   # Visualizaci?n
  library(AER)       # Para regresi?n IV
  library(skimr)     # Estad?sticas descriptivas
  library(boot)      # Para realizar bootstrap
  library(tidyr)
})

# Paths por usuario
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# CARGAMOS LA BASE DE DATOS

base <- import("./AeioTU.dta")
skim(base)
glimpse(base)

# Load the functions that we use in this script
#source("./00_functions_cleaning_df.R")

"Outcome de inter?s (Y): Habilidades cognitivas
 Variable indep. de inter?s (X): Asignaci?n a centro de cuidado
 ?Cu?l es el efecto de la asignaci?n de un cupo en un centro de cuidado sobre el desarrollo cognitivo? "

# 1. DEFINIMOS COVARIABLES

covs <- c("cognitivo_BL", "sexo", "raza", "riqueza_BL", "ninos_casa_BL",
         "centrocuidadp_BL", "edad_BL", "edad2_BL")
covs <- c(covs, grep("^assessor_FE", names(df), value = TRUE))

### Enfoque 1: MCO(OLS) Regresion ingenua

# Variable de uso
base <- base %>%
  mutate(to_use = !is.na(cognitivo_ajust) & !is.na(cognitivo_BL))

# Seleccionamos las columnas necesarias
modelo_df <- base %>%
  filter(to_use == TRUE) %>%
  select(cognitivo_ajust, asistencia, all_of(covs))

# Regresi?n OLS
ols_model <- lm(cognitivo_ajust ~ ., data = modelo_df)
summary(ols_model)

# 2. ESTADISTICAS DESCRIPTIVAS Y TASA DE CUMPLIMIENTO

# Tabla de cumplimiento
table(base$asistencia, base$D)


# Gr?fico de cumplimiento (Revisar en casa en detalle)
base %>%
  filter(to_use == TRUE) %>%
  group_by(D) %>%
  summarise(
    pct_asiste = mean(asistencia, na.rm = TRUE) * 100,
    pct_no_asiste = (1 - mean(asistencia, na.rm = TRUE)) * 100
  ) %>%
  pivot_longer(cols = starts_with("pct"), names_to = "tipo", values_to = "porcentaje") %>%
  ggplot(aes(x = factor(D, labels = c("Asignado a Control", "Asignado a Tratamiento")),
             y = porcentaje, fill = tipo)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(porcentaje, 1)), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 4) +
  scale_fill_manual(values = c("pct_asiste" = "black", "pct_no_asiste" = "gray"),
                    labels = c("% Asiste", "% No Asiste")) +
  labs(title = "Cumplimiento por grupo asignado", y = "%", x = "Grupo") +
  theme_minimal()


### Enfoque 2: Variables instrumentales

# Verificacion de relevancia
modelo_relevancia <- base %>%
  filter(to_use == TRUE) %>%
  select(asistencia, D, all_of(covs))

# Verificamos relevancia
relevancia <- lm(asistencia ~ ., data = modelo_relevancia)
summary(relevancia)

# Estimacion de IV 
modelo_iv <- base %>%
  filter(to_use == TRUE) %>%
  select(cognitivo_ajust, asistencia, D, all_of(covs))

# Ajustar el modelo IV
iv_model <- ivreg(cognitivo_ajust ~ asistencia + . - D | D + . - asistencia, data = modelo_iv)

# Mostrar resumen con diagnósticos
summary(iv_model, diagnostics = TRUE)

## Errores estandar: BOOTSTRAPING

# Funci?n para bootstrap
iv_boot_fn <- function(data, indices) {
  d <- data[indices, ]
  model <- ivreg(cognitivo_ajust ~ asistencia + . | D + ., data = d)
  return(coef(model)["asistencia"])
}

# Ejecutar bootstrap
set.seed(123)  # Para reproducibilidad
boot_iv <- boot(data = modelo_iv, statistic = iv_boot_fn, R = 1000)

# Resultados
print(boot_iv)
boot.ci(boot_iv, type = c("perc", "bca"))
