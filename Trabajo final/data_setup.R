#### Trabajo final de Econometría Avanzada
# Proyecto: Evaluación de aumento de acceso a educación parvularia
# Autor: Mahicol Ramírez
# Fecha: 5 de diciembre de 2025
#
# DESCRIPCIÓN GENERAL DEL SCRIPT
# ---------------------------------------------------------------
# Este script construye una base de datos a nivel de niño (folio)
# utilizando la Encuesta Longitudinal de Primera Infancia (ELPI)
# para las rondas 2010 y 2012. En particular:
#
# 1) Carga y depura la información de:
#    - Evaluaciones cognitivas (TVIP) 2010 y 2012.
#    - Módulos de Hogar / Cuidador principal 2010 y 2012.
#    - Módulos de Cuidado infantil 2010 y 2012.
#
# 2) Define un único registro por niño (folio), manteniendo:
#    - El puntaje de TVIP (tvip_pb) y la edad en meses de 2012,
#      restringiendo la muestra a niños con TVIP 2012 aplicada
#      entre 58 y 78 meses (≈ 4,8 a 6,5 años).
#
# 3) Construye la variable de tratamiento:
#    - treat_prek_4_5: indicador de haber asistido a algún
#      establecimiento de educación preescolar/parvularia (o
#      educacional, según 2012) entre los 4–5 años, usando:
#        * 2010: Cuidado_infantil_2010, tramo 8 (orden_tr == 8),
#                pregunta J10.
#        * 2012: Cuidado_infantil_2012, tramo 8 (tramo == 8),
#                pregunta E7.
#
# 4) Conserva información del cuidador principal en 2010 y 2012,
#    renombrando las variables con nombres más intuitivos
#    (educación, ocupación, ingresos).
#
# El resultado es la base `base_final`, con una observación por
# niño (folio), que se usará en etapas posteriores para análisis
# descriptivos y ejercicios de identificación causal.

rm(list = ls())  # Limpiamos el entorno

#### 1. Cargar librerías
library(haven)        # Lectura de .dta (Stata)
library(dplyr)        # Manipulación de datos
library(ggplot2)      # Gráficas (para etapas posteriores)
library(scales)
library(rstudioapi)   # Para fijar directorio usando la ruta del script
library(tidyr)
library(patchwork)
library(survey)

#### 2. Directorio de trabajo y rutas
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

root    <- getwd()
datos   <- file.path(root, "data")
results <- file.path(root, "results")
if (!dir.exists(results)) dir.create(results, recursive = TRUE)

#### 3. Definir subconjuntos de variables
# Evaluaciones (2010 y 2012)
vars_evaluaciones_2010 <- c("edad_meses", "tvip_pb")
vars_evaluaciones_2012 <- c("edad_meses", "tvip_pb")

# Hogar 2010 (cuidador principal)
vars_hogar_2010 <- c("b1", "b2c", "b2n",  # educación
                     "c1", "c2", "c3",    # trabajo
                     "d1m", "d2m", "d3m") # ingresos

# Hogar 2012 (cuidador principal)
vars_hogar_2012 <- c("j1", "j2c", "j2n",                 # educación
                     "k1", "k2", "k3",                   # trabajo
                     "l1_monto", "l2_monto", "l3_monto") # ingresos

#### 4. Cargar bases de datos
Entrevista_2010   <- read_dta(file.path(datos, "Entrevistada_2010.dta"))
Evaluaciones_2010 <- read_dta(file.path(datos, "Evaluaciones_2010.dta"))
Evaluaciones_2012 <- read_dta(file.path(datos, "Evaluaciones_2012.dta"))

Hogar_2010 <- read_dta(file.path(datos, "Hogar_2010.dta"))
Hogar_2012 <- read_dta(file.path(datos, "Hogar_2012.dta"))

Cuidado_2010 <- read_dta(file.path(datos, "Cuidado_infantil_2010.dta"))
Cuidado_2012 <- read_dta(file.path(datos, "Cuidado_infantil_2012.dta"))

############################################################
#### 5. Base Hogar 2010: cuidador principal + nombres claros
############################################################

hogar_2010_cp <- Hogar_2010 |>
  mutate(
    id_persona_num = as.numeric(id_persona),
    orden_persona  = id_persona_num %% 100
  ) |>
  filter(orden_persona == 1) |>  # cuidador principal en 2010
  transmute(
    folio,
    id_persona,
    # Módulo B – educación (2010)
    cp_edu_asiste_2010 = b1,   # ¿Asiste a establecimiento educacional?
    cp_edu_curso_2010  = b2c,  # Curso actual / último aprobado
    cp_edu_nivel_2010  = b2n,  # Nivel educacional (sala cuna, jardín, etc.)
    # Módulo C – situación laboral (2010)
    cp_lab_trabajo_semana_2010   = c1,  # Trabajó al menos una hora
    cp_lab_actividad_semana_2010 = c2,  # Realizó alguna actividad de trabajo
    cp_lab_tiene_empleo_2010     = c3,  # Tenía empleo del que estuvo ausente
    # Módulo D – ingresos (2010)
    cp_inc_sueldos_2010        = d1m,  # Sueldos y salarios
    cp_inc_en_especie_2010     = d2m,  # Ingresos en especie / regalías
    cp_inc_independiente_2010  = d3m   # Ingresos actividades independientes
  ) |>
  distinct(folio, .keep_all = TRUE)

############################################################
#### 6. Base Hogar 2012: cuidador principal + nombres claros
############################################################

hogar_2012_cp <- Hogar_2012 |>
  # En 2012, el cuidador principal es la persona con orden == 1
  filter(orden == 1) |>
  transmute(
    folio,
    # id_persona_2012 = id_persona,  # <-- ESTA LÍNEA CAUSA EL ERROR, LA QUITAMOS
    # Módulo J – educación (2012), análogo a B en 2010
    cp_edu_asiste_2012 = j1,     # ¿Asiste a establecimiento educacional?
    cp_edu_curso_2012  = j2c,    # Curso actual / último aprobado
    cp_edu_nivel_2012  = j2n,    # Nivel educacional
    # Módulo K – situación laboral (2012), análogo a C
    cp_lab_trabajo_semana_2012   = k1,  # Trabajó la semana pasada
    cp_lab_actividad_semana_2012 = k2,  # Realizó alguna actividad de trabajo
    cp_lab_tiene_empleo_2012     = k3,  # Tenía empleo del que estuvo ausente
    # Módulo L – ingresos (2012), análogo a D
    cp_inc_sueldos_2012       = l1_monto,
    cp_inc_en_especie_2012    = l2_monto,
    cp_inc_independiente_2012 = l3_monto
  ) |>
  distinct(folio, .keep_all = TRUE)

############################################################
#### 7. Base TVIP: usar siempre la medición 2012
############################################################

# 7.1. Selección de variables de Evaluaciones 2010 y 2012
eval_2010_sel <- Evaluaciones_2010 |>
  select(
    folio,
    edad_meses_2010 = edad_meses,
    tvip_pb_2010    = tvip_pb
  )

eval_2012_sel <- Evaluaciones_2012 |>
  select(
    folio,
    edad_meses_2012 = edad_meses,
    tvip_pb_2012    = tvip_pb
  )

# 7.2. Unir Evaluaciones 2010–2012 por folio
eval_10_12 <- eval_2010_sel |>
  full_join(eval_2012_sel, by = "folio")

# 7.3. Definir variables "finales" de TVIP usando SIEMPRE 2012
eval_tvip <- eval_10_12 |>
  mutate(
    tvip_score      = tvip_pb_2012,    # Puntaje bruto TVIP 2012
    edad_meses_tvip = edad_meses_2012  # Edad del niño al test 2012
  )

############################################################
#### 8. Filtrar por rango de edad y presencia de TVIP en 2012
############################################################

# Parámetros del diseño:
edad_min <- 58L  # meses (mínimo)
edad_max <- 78L  # meses (máximo: 6,5 años)

base_tvip <- eval_tvip |>
  filter(
    !is.na(tvip_score),
    !is.na(edad_meses_tvip),
    edad_meses_tvip >= edad_min,
    edad_meses_tvip <= edad_max
  )

# Filtro opcional por una edad exacta en meses (dejar en NA si no se usa)
edad_meses_target <- NA_integer_  # por ejemplo, 60L si quieres sólo niños de 5 años exactos

if (!is.na(edad_meses_target)) {
  base_tvip <- base_tvip |>
    filter(edad_meses_tvip == edad_meses_target)
}

############################################################
#### 9. Variable de tratamiento: asistencia a pre-kinder 4–5 años
####    basada en tramo 8 en Cuidado Infantil (2010/2012)
############################################################

## 9.1. 2010 – Cuidado Infantil
## orden_tr = 8 → tramo 4–5 años
## J10: "¿Envió al(a la) niño(a) a algún establecimiento de educación
##       preescolar o parvulario entre los (….)?"
##       1–3 = asistió, 4 = no asistió

trat_2010 <- Cuidado_2010 |>
  filter(orden_tr == 8) |>
  mutate(
    prek_4_5_2010 = case_when(
      j10 %in% 1:3 ~ 1L,  # asistió a algún establecimiento preescolar/parvulario
      j10 == 4    ~ 0L,   # no asistió
      TRUE        ~ NA_integer_
    )
  ) |>
  select(folio, prek_4_5_2010) |>
  distinct(folio, .keep_all = TRUE)

## 9.2. 2012 – Cuidado Infantil
## tramo = 8 → 4–5 años
## E7: "¿Envió al(a la) niño(a) seleccionado(a) a algún establecimiento
##      educacional entre los (….)?"
##      1 = Sí, sala cuna
##      2 = Sí, jardín infantil
##      3 = Sí, escuela/colegio
##      4 = No

trat_2012 <- Cuidado_2012 |>
  filter(tramo == 8) |>
  mutate(
    prek_4_5_2012 = case_when(
      e7 %in% 1:3 ~ 1L,  # asistió a algún establecimiento educacional
      e7 == 4     ~ 0L,  # no asistió
      TRUE        ~ NA_integer_
    )
  ) |>
  select(folio, prek_4_5_2012) |>
  distinct(folio, .keep_all = TRUE)

## 9.3. Combinar 2010 y 2012 en una sola variable de tratamiento
##      Regla: usamos primero la info 2010; si falta, usamos 2012.

tratamiento_prek <- trat_2010 |>
  full_join(trat_2012, by = "folio") |>
  mutate(
    treat_prek_4_5 = dplyr::coalesce(prek_4_5_2010, prek_4_5_2012)
  ) |>
  select(folio, treat_prek_4_5, prek_4_5_2010, prek_4_5_2012)

############################################################
#### 10. Base final: 1 registro por folio
####     TVIP 2012 + cuidador 2010/2012 + tratamiento pre-kinder
############################################################

base_final <- base_tvip |>
  left_join(hogar_2010_cp,      by = "folio") |>
  left_join(hogar_2012_cp,      by = "folio") |>
  left_join(tratamiento_prek,   by = "folio")

# Chequeos básicos
nrow(base_final)
dplyr::n_distinct(base_final$folio)

# Guardar base consolidada
saveRDS(
  base_final,
  file = file.path(datos, "base_final.rds")
)

############################################################
#### 11. Estructura de la base base_final (resumen de variables)
############################################################
# La base base_final tiene una observación por niño (folio) y
# se compone, en esencia, de los siguientes grupos de variables:
#
# 1) Identificación y resultado (outcome)
#    - folio             : Identificador único del niño/hogar.
#    - edad_meses_2010   : Edad en meses al momento de la evaluación 2010 (si existe).
#    - edad_meses_2012   : Edad en meses al momento de la evaluación 2012.
#    - tvip_pb_2010      : Puntaje bruto TVIP en 2010 (si existe).
#    - tvip_pb_2012      : Puntaje bruto TVIP en 2012.
#    - tvip_score        : Puntaje TVIP utilizado en el análisis (TVIP 2012).
#    - edad_meses_tvip   : Edad en meses utilizada en el análisis (edad_meses_2012),
#                          restringida al rango [58, 78].
#
# 2) Cuidador principal 2010 (hogar_2010_cp)
#    - id_persona                     : Identificador de la persona en 2010.
#    - cp_edu_asiste_2010            : Asistencia a establecimiento educacional (B1).
#    - cp_edu_curso_2010             : Curso actual / último aprobado (B2C).
#    - cp_edu_nivel_2010             : Nivel educacional (B2N).
#    - cp_lab_trabajo_semana_2010    : Trabajó al menos una hora la semana pasada (C1).
#    - cp_lab_actividad_semana_2010  : Realizó alguna actividad de trabajo (C2).
#    - cp_lab_tiene_empleo_2010      : Tenía empleo del que estuvo ausente (C3).
#    - cp_inc_sueldos_2010           : Monto sueldos y salarios (D1m).
#    - cp_inc_en_especie_2010        : Monto ingresos en especie/regalías (D2m).
#    - cp_inc_independiente_2010     : Monto ingresos actividades independientes (D3m).
#
# 3) Cuidador principal 2012 (hogar_2012_cp)
#    - id_persona_2012               : Identificador de la persona en 2012.
#    - cp_edu_asiste_2012            : Asistencia a establecimiento educacional (J1).
#    - cp_edu_curso_2012             : Curso actual / último aprobado (J2C).
#    - cp_edu_nivel_2012             : Nivel educacional (J2N).
#    - cp_lab_trabajo_semana_2012    : Trabajó la semana pasada (K1).
#    - cp_lab_actividad_semana_2012  : Realizó alguna actividad de trabajo (K2).
#    - cp_lab_tiene_empleo_2012      : Tenía empleo del que estuvo ausente (K3).
#    - cp_inc_sueldos_2012           : Monto sueldos y salarios (L1_monto).
#    - cp_inc_en_especie_2012        : Monto ingresos en especie/regalías (L2_monto).
#    - cp_inc_independiente_2012     : Monto ingresos actividades independientes (L3_monto).
#
# 4) Tratamiento: asistencia a pre-kinder entre 4–5 años (tramo 8)
#    - prek_4_5_2010 : Indicador construido a partir de Cuidado_infantil_2010,
#                      tramo 8 (orden_tr == 8) y pregunta J10:
#                      * 1 si asistió a algún establecimiento de educación
#                        preescolar/parvulario, 0 si no, NA si no informado.
#    - prek_4_5_2012 : Indicador construido a partir de Cuidado_infantil_2012,
#                      tramo 8 (tramo == 8) y pregunta E7:
#                      * 1 si asistió a algún establecimiento educacional
#                        (sala cuna, jardín infantil, escuela/colegio),
#                        0 si no, NA si no informado.
#    - treat_prek_4_5: Variable de tratamiento principal, definida como:
#                      * treat_prek_4_5 = prek_4_5_2010 si no es NA;
#                      * en su defecto, treat_prek_4_5 = prek_4_5_2012;
#                      * NA si no se dispone de información en ambos años.
#
# Esta estructura deja la base lista para:
# - Comparar resultados de TVIP (tvip_score) por condición de tratamiento
#   (treat_prek_4_5) en un rango homogéneo de edad.
# - Incorporar controles por características del cuidador y del hogar
#   en 2010 y 2012 en modelos econométricos posteriores.
