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
#    - Módulos de Cuidado infantil 2010 y 2012 (tramos de edad).
#    - Módulos de Embarazo de la madre / cuidador (G en 2010, B en 2012).
#
# 2) Define un único registro por niño (folio), manteniendo:
#    - El puntaje de TVIP (tvip_pb) y la edad en meses de 2012,
#      restringiendo la muestra a niños con TVIP 2012 aplicada
#      entre 58 y 78 meses (≈ 4,8 a 6,5 años).
#
# 3) Construye la variable de tratamiento:
#    - treat_prek_4_5: indicador de haber asistido a algún
#      establecimiento de educación preescolar/parvulario (o
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
# 5) Agrega controles de embarazo de la madre/cuidador:
#    - 2010 (módulo G, Entrevistada_2010): estado nutricional,
#      tabaquismo, consumo de alcohol, medicamentos y drogas.
#    - 2012 (módulo B, Cuidador Principal 2012): análogos de estado
#      nutricional, tabaquismo, alcohol, medicamentos y drogas.
#
# 6) Incluye un puntaje TVIP estandarizado (tvip_z) para interpretar
#    efectos en desviaciones estándar.
#
# El resultado es la base `base_final`, con una observación por
# niño (folio), que se usará en etapas posteriores para análisis
# descriptivos y ejercicios de identificación causal (p.ej. PSM).

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

# Embarazo 2010 (módulo G, Entrevistada_2010)
vars_emb_2010 <- c("g6a", "g7a", "g7b", "g9", "g11a", "g11b")

# Embarazo 2012 (módulo B, Cuidador Principal 2012)
vars_emb_2012 <- c("b6", "b8", "b9", "b12", "b14", "b16")

#### 4. Cargar bases de datos
Entrevista_2010   <- read_dta(file.path(datos, "Entrevistada_2010.dta"))
Entrevista_2012   <- read_dta(file.path(datos, "Entrevistada_2012.dta"))

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
    cp_edu_asiste_2010 = b1,   # B1: asiste a establecimiento educacional
    cp_edu_curso_2010  = b2c,  # B2c: curso actual / último aprobado
    cp_edu_nivel_2010  = b2n,  # B2n: nivel educacional (básica, media, etc.)
    # Módulo C – situación laboral (2010)
    cp_lab_trabajo_semana_2010   = c1,  # C1: trabajó >= 1 hora la semana pasada
    cp_lab_actividad_semana_2010 = c2,  # C2: realizó alguna actividad de trabajo
    cp_lab_tiene_empleo_2010     = c3,  # C3: tenía empleo del que estuvo ausente
    # Módulo D – ingresos (2010)
    cp_inc_sueldos_2010        = d1m,  # D1m: sueldos y salarios (monto)
    cp_inc_en_especie_2010     = d2m,  # D2m: ingresos en especie/regalías (monto)
    cp_inc_independiente_2010  = d3m   # D3m: ingresos actividades independientes (monto)
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
    # Módulo J – educación (2012), análogo a B en 2010
    cp_edu_asiste_2012 = j1,     # J1: asiste a establecimiento educacional
    cp_edu_curso_2012  = j2c,    # J2c: curso actual / último aprobado
    cp_edu_nivel_2012  = j2n,    # J2n: nivel educacional
    # Módulo K – situación laboral (2012), análogo a C
    cp_lab_trabajo_semana_2012   = k1,  # K1: trabajó la semana pasada
    cp_lab_actividad_semana_2012 = k2,  # K2: realizó alguna actividad de trabajo
    cp_lab_tiene_empleo_2012     = k3,  # K3: tenía empleo del que estuvo ausente
    # Módulo L – ingresos (2012), análogo a D
    cp_inc_sueldos_2012       = l1_monto, # L1_monto: sueldos y salarios
    cp_inc_en_especie_2012    = l2_monto, # L2_monto: ingresos en especie/regalías
    cp_inc_independiente_2012 = l3_monto  # L3_monto: ingresos actividades independientes
  ) |>
  distinct(folio, .keep_all = TRUE)

############################################################
#### 7. Embarazo 2010: módulo G (Entrevistada_2010)
############################################################
# Según el cuestionario 2010:
# - G6a: estado nutricional durante el embarazo (bajo peso, normal, sobrepeso, obesidad). :contentReference[oaicite:2]{index=2}
# - G7a: fumó cigarrillos durante el embarazo (sí/no). :contentReference[oaicite:3]{index=3}
# - G7b: número de cigarrillos fumados al mes durante el embarazo (cantidad mensual). :contentReference[oaicite:4]{index=4}
# - G9: consumo de bebidas alcohólicas durante el embarazo (nunca, esporádicamente, regularmente). :contentReference[oaicite:5]{index=5}
# - G11a: consumo de medicamentos durante el embarazo (frecuencia). :contentReference[oaicite:6]{index=6}
# - G11b: consumo de drogas durante el embarazo (frecuencia). :contentReference[oaicite:7]{index=7}

embarazo_2010 <- Entrevista_2010 |>
  transmute(
    folio,
    emb_nutr_estado_2010   = g6a,  # Estado nutricional durante el embarazo (categorías 1–4, 8, 9)
    emb_fuma_2010          = g7a,  # Indicador categórico: fumó cigarrillos durante el embarazo
    emb_cigs_mes_2010      = g7b,  # Nº de cigarrillos fumados al mes en el embarazo
    emb_alcohol_2010       = g9,   # Frecuencia consumo de alcohol en el embarazo
    emb_medicamentos_2010  = g11a, # Frecuencia consumo de medicamentos en el embarazo
    emb_drogas_2010        = g11b  # Frecuencia consumo de drogas en el embarazo
  ) |>
  distinct(folio, .keep_all = TRUE)

############################################################
#### 8. Embarazo 2012: módulo B (Cuidador Principal 2012)
############################################################
# Según el cuestionario 2012:
# - B6: estado nutricional durante el embarazo (bajo peso, normal, sobrepeso, obesidad). :contentReference[oaicite:8]{index=8}
# - B8: fumó cigarrillos durante el embarazo (sí/no). :contentReference[oaicite:9]{index=9}
# - B9: cantidad de cigarrillos fumados al mes durante el embarazo. :contentReference[oaicite:10]{index=10}
# - B12: consumo de bebidas alcohólicas durante el embarazo (nunca, esporádicamente, regularmente). :contentReference[oaicite:11]{index=11}
# - B14: consumo de medicamentos durante el embarazo (frecuencia). :contentReference[oaicite:12]{index=12}
# - B16: consumo de drogas durante el embarazo (frecuencia). :contentReference[oaicite:13]{index=13}

embarazo_2012 <- Entrevista_2012 |>
  transmute(
    folio,
    emb_nutr_estado_2012   = b6,   # Estado nutricional durante el embarazo (categorías 1–4, 8, 9)
    emb_fuma_2012          = b8,   # Indicador categórico: fumó cigarrillos durante el embarazo
    emb_cigs_mes_2012      = b9,   # Nº de cigarrillos al mes durante el embarazo
    emb_alcohol_2012       = b12,  # Frecuencia consumo de alcohol en el embarazo
    emb_medicamentos_2012  = b14,  # Frecuencia consumo de medicamentos en el embarazo
    emb_drogas_2012        = b16   # Frecuencia consumo de drogas en el embarazo
  ) |>
  distinct(folio, .keep_all = TRUE)

############################################################
#### 9. Base TVIP: usar siempre la medición 2012
############################################################

# 9.1. Selección de variables de Evaluaciones 2010 y 2012
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

# 9.2. Unir Evaluaciones 2010–2012 por folio
eval_10_12 <- eval_2010_sel |>
  full_join(eval_2012_sel, by = "folio")

# 9.3. Definir variables "finales" de TVIP usando SIEMPRE 2012
eval_tvip <- eval_10_12 |>
  mutate(
    tvip_score      = tvip_pb_2012,    # Puntaje bruto TVIP 2012
    edad_meses_tvip = edad_meses_2012  # Edad del niño al test 2012
  )

############################################################
#### 10. Filtrar por rango de edad y presencia de TVIP en 2012
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
edad_meses_target <- NA_integer_  # por ejemplo, 60L si quieres sólo niños de 60 meses exactos

if (!is.na(edad_meses_target)) {
  base_tvip <- base_tvip |>
    filter(edad_meses_tvip == edad_meses_target)
}

############################################################
#### 11. Variable de tratamiento: asistencia a pre-kinder 4–5 años
####     basada en tramo 8 en Cuidado Infantil (2010/2012)
############################################################

## 11.1. 2010 – Cuidado Infantil
## orden_tr = 8 → tramo 4–5 años
## J10: “¿Envió al(a la) niño(a) a algún establecimiento de educación
##       preescolar o parvulario entre los (….)?” (1–3 sí, 4 no)

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

## 11.2. 2012 – Cuidado Infantil
## tramo = 8 → 4–5 años
## E7: “¿Envió al(a la) niño(a) seleccionado(a) a algún establecimiento
##      educacional entre los (….)?” (1–3 sí, 4 no)

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

## 11.3. Combinar 2010 y 2012 en una sola variable de tratamiento
##      Regla: usamos primero la info 2010; si falta, usamos 2012.

tratamiento_prek <- trat_2010 |>
  full_join(trat_2012, by = "folio") |>
  mutate(
    treat_prek_4_5 = dplyr::coalesce(prek_4_5_2010, prek_4_5_2012)
  ) |>
  select(folio, treat_prek_4_5, prek_4_5_2010, prek_4_5_2012)

############################################################
#### 12. Base final: 1 registro por folio
####     TVIP 2012 + cuidador 2010/2012 + embarazo + tratamiento
############################################################

base_final <- base_tvip |>
  left_join(hogar_2010_cp,    by = "folio") |>
  left_join(hogar_2012_cp,    by = "folio") |>
  left_join(tratamiento_prek, by = "folio") |>
  left_join(embarazo_2010,    by = "folio") |>
  left_join(embarazo_2012,    by = "folio") |>
  mutate(
    # TVIP estandarizado: media 0, desviación estándar 1
    tvip_z = as.numeric(scale(tvip_score))
  )

# Chequeos básicos
nrow(base_final)
dplyr::n_distinct(base_final$folio)

# Guardar base consolidada
saveRDS(
  base_final,
  file = file.path(datos, "base_final.rds")
)

############################################################
#### 13. Estructura de la base base_final (resumen de variables)
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
#    - tvip_z            : Puntaje TVIP estandarizado (media 0, desviación estándar 1)
#                          calculado sobre la muestra filtrada.
#
# 2) Cuidador principal 2010 (hogar_2010_cp)
#    - id_persona                     : Identificador de la persona en 2010.
#    - cp_edu_asiste_2010            : Asiste a establecimiento educacional (B1).
#    - cp_edu_curso_2010             : Curso actual / último aprobado (B2c).
#    - cp_edu_nivel_2010             : Nivel educacional (B2n).
#    - cp_lab_trabajo_semana_2010    : Trabajó ≥1 hora la semana pasada (C1).
#    - cp_lab_actividad_semana_2010  : Realizó alguna actividad de trabajo (C2).
#    - cp_lab_tiene_empleo_2010      : Tenía empleo del que estuvo ausente (C3).
#    - cp_inc_sueldos_2010           : Monto de sueldos y salarios (D1m, pesos).
#    - cp_inc_en_especie_2010        : Monto ingresos en especie/regalías (D2m, pesos).
#    - cp_inc_independiente_2010     : Monto ingresos independientes (D3m, pesos).
#
# 3) Cuidador principal 2012 (hogar_2012_cp)
#    - cp_edu_asiste_2012            : Asiste a establecimiento educacional (J1).
#    - cp_edu_curso_2012             : Curso actual / último aprobado (J2c).
#    - cp_edu_nivel_2012             : Nivel educacional (J2n).
#    - cp_lab_trabajo_semana_2012    : Trabajó la semana pasada (K1).
#    - cp_lab_actividad_semana_2012  : Realizó alguna actividad de trabajo (K2).
#    - cp_lab_tiene_empleo_2012      : Tenía empleo del que estuvo ausente (K3).
#    - cp_inc_sueldos_2012           : Monto sueldos y salarios (L1_monto, pesos).
#    - cp_inc_en_especie_2012        : Monto ingresos en especie/regalías (L2_monto, pesos).
#    - cp_inc_independiente_2012     : Monto ingresos independientes (L3_monto, pesos).
#
# 4) Tratamiento: asistencia a pre-kinder entre 4–5 años (tramo 8)
#    - prek_4_5_2010 : Indicador (0/1) construido a partir de Cuidado_infantil_2010,
#                      tramo 8 (orden_tr == 8) y pregunta J10:
#                      * 1 si asistió a algún establecimiento de educación
#                        preescolar/parvulario entre 4–5 años,
#                        0 si no, NA si no informado.
#    - prek_4_5_2012 : Indicador (0/1) construido a partir de Cuidado_infantil_2012,
#                      tramo 8 (tramo == 8) y pregunta E7:
#                      * 1 si asistió a algún establecimiento educacional
#                        (sala cuna, jardín, escuela/colegio) entre 4–5 años,
#                        0 si no, NA si no informado.
#    - treat_prek_4_5: Variable de tratamiento principal, definida como:
#                      * treat_prek_4_5 = prek_4_5_2010 si no es NA;
#                      * en su defecto, treat_prek_4_5 = prek_4_5_2012;
#                      * NA si no se dispone de información en ambos años.
#
# 5) Embarazo de la madre/cuidador 2010 (embarazo_2010, módulo G)
#    - emb_nutr_estado_2010  : Estado nutricional durante el embarazo
#                              (categorías: bajo peso, normal, sobrepeso, obesidad;
#                              códigos 1–4, 8=“no responde”, 9=“no sabe”).
#    - emb_fuma_2010         : Indicador categórico de si fumó cigarrillos
#                              durante el embarazo (1=Sí, 2=No, 8=No responde).
#    - emb_cigs_mes_2010     : Nº de cigarrillos fumados en promedio mensual
#                              durante el embarazo (cantidad continua).
#    - emb_alcohol_2010      : Frecuencia de consumo de bebidas alcohólicas
#                              durante el embarazo (1=nunca, 2=esporádicamente,
#                              3=regularmente, 8=No responde).
#    - emb_medicamentos_2010 : Frecuencia consumo de medicamentos durante
#                              el embarazo (escala tipo Likert análoga a alcohol).
#    - emb_drogas_2010       : Frecuencia consumo de drogas durante el
#                              embarazo (escala tipo Likert análoga).
#
# 6) Embarazo de la madre/cuidador 2012 (embarazo_2012, módulo B)
#    - emb_nutr_estado_2012  : Estado nutricional durante el embarazo
#                              (bajo peso, normal, sobrepeso, obesidad; códigos 1–4,
#                               8=No responde, 9=No sabe).
#    - emb_fuma_2012         : Indicador categórico de si fumó cigarrillos
#                              durante el embarazo (1=Sí, 2=No, 8=No responde).
#    - emb_cigs_mes_2012     : Nº de cigarrillos fumados en promedio mensual
#                              durante el embarazo.
#    - emb_alcohol_2012      : Frecuencia de consumo de bebidas alcohólicas
#                              durante el embarazo (1=nunca, 2=esporádicamente,
#                              3=regularmente, 8=No responde).
#    - emb_medicamentos_2012 : Frecuencia consumo de medicamentos en el
#                              embarazo (escala 1–3 + 8=No responde).
#    - emb_drogas_2012       : Frecuencia consumo de drogas en el embarazo
#                              (escala 1–3 + 8=No responde).
#
# Con esta estructura, la base está lista para:
# - Descriptivos por tratamiento (treat_prek_4_5).
# - Ajuste de modelos de PSM usando como covariables características
#   del niño, del cuidador/hogar y del embarazo.
# - Interpretar efectos en unidades de desviación estándar de TVIP (tvip_z).
