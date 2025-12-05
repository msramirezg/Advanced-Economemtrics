############################################################
#### Trabajo final de Econometría Avanzada
#### Proyecto: Evaluación de aumento de acceso a educación parvularia
#### Autor: Mahicol Ramírez
#### Fecha: 5 de diciembre de 2025
############################################################
#
# Este script:
#  1) Carga y depura ELPI 2010–2012 para construir base_final (1 obs por folio).
#  2) A partir de base_final construye una base limpia para PSM:
#     - Resultado: tvip_score, tvip_z, edad_meses_tvip.
#     - Tratamiento: treat_prek_4_5 y treat (0/1).
#     - Covariables: educación (homogenizada 2012→2010 + 3 categorías),
#                    ingresos (ln_cp_inc_total),
#                    desocupación,
#                    gestación (peso_normal_emb, fuma_emb, alcohol_emb + brutas).
#  3) Guarda base_psm.rds en la carpeta data/.
############################################################

rm(list = ls())

#### 1. Cargar librerías
library(haven)        # Lectura de .dta (Stata)
library(dplyr)        # Manipulación de datos
library(ggplot2)      # (para chequeos si los quieres)
library(rstudioapi)   # Para fijar directorio usando la ruta del script
library(tidyr)

#### 2. Directorio de trabajo y rutas
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

root    <- getwd()
datos   <- file.path(root, "data")
results <- file.path(root, "results")
if (!dir.exists(results)) dir.create(results, recursive = TRUE)

#### 3. Cargar bases de datos crudas
Entrevista_2010   <- read_dta(file.path(datos, "Entrevistada_2010.dta"))
Entrevista_2012   <- read_dta(file.path(datos, "Entrevistada_2012.dta"))

Evaluaciones_2010 <- read_dta(file.path(datos, "Evaluaciones_2010.dta"))
Evaluaciones_2012 <- read_dta(file.path(datos, "Evaluaciones_2012.dta"))

Hogar_2010 <- read_dta(file.path(datos, "Hogar_2010.dta"))
Hogar_2012 <- read_dta(file.path(datos, "Hogar_2012.dta"))

Cuidado_2010 <- read_dta(file.path(datos, "Cuidado_infantil_2010.dta"))
Cuidado_2012 <- read_dta(file.path(datos, "Cuidado_infantil_2012.dta"))

############################################################
#### 4. Base Hogar 2010: cuidador principal + nombres claros
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
    cp_edu_nivel_2010  = b2n,  # Nivel educacional (básica, media, etc.)
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
#### 5. Base Hogar 2012: cuidador principal + nombres claros
############################################################

hogar_2012_cp <- Hogar_2012 |>
  filter(orden == 1) |>
  transmute(
    folio,
    # Módulo J – educación (2012)
    cp_edu_asiste_2012 = j1,
    cp_edu_curso_2012  = j2c,
    cp_edu_nivel_2012  = j2n,
    # Módulo K – trabajo (2012)
    cp_lab_trabajo_semana_2012   = k1,
    cp_lab_actividad_semana_2012 = k2,
    cp_lab_tiene_empleo_2012     = k3,
    # Módulo L – ingresos (2012)
    cp_inc_sueldos_2012       = l1_monto,
    cp_inc_en_especie_2012    = l2_monto,
    cp_inc_independiente_2012 = l3_monto
  ) |>
  distinct(folio, .keep_all = TRUE)

############################################################
#### 6. Gestación 2010 y 2012
############################################################

embarazo_2010 <- Entrevista_2010 |>
  transmute(
    folio,
    emb_nutr_estado_2010   = g6a,
    emb_fuma_2010          = g7a,
    emb_cigs_mes_2010      = g7b,
    emb_alcohol_2010       = g9,
    emb_medicamentos_2010  = g11a,
    emb_drogas_2010        = g11b
  ) |>
  distinct(folio, .keep_all = TRUE)

embarazo_2012 <- Entrevista_2012 |>
  transmute(
    folio,
    emb_nutr_estado_2012   = b6,
    emb_fuma_2012          = b8,
    emb_cigs_mes_2012      = b9,
    emb_alcohol_2012       = b12,
    emb_medicamentos_2012  = b14,
    emb_drogas_2012        = b16
  ) |>
  distinct(folio, .keep_all = TRUE)

############################################################
#### 7. TVIP 2010–2012 y selección de cohorte
############################################################

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

eval_10_12 <- eval_2010_sel |>
  full_join(eval_2012_sel, by = "folio")

eval_tvip <- eval_10_12 |>
  mutate(
    tvip_score      = tvip_pb_2012,
    edad_meses_tvip = edad_meses_2012
  )

edad_min <- 58L
edad_max <- 78L

base_tvip <- eval_tvip |>
  filter(
    !is.na(tvip_score),
    !is.na(edad_meses_tvip),
    edad_meses_tvip >= edad_min,
    edad_meses_tvip <= edad_max
  )

############################################################
#### 8. Tratamiento: asistencia a pre-kinder 4–5 años
############################################################

# 2010 – Cuidado Infantil
trat_2010 <- Cuidado_2010 |>
  filter(orden_tr == 8) |>
  mutate(
    prek_4_5_2010 = case_when(
      j10 %in% 1:3 ~ 1L,
      j10 == 4    ~ 0L,
      TRUE        ~ NA_integer_
    )
  ) |>
  select(folio, prek_4_5_2010) |>
  distinct(folio, .keep_all = TRUE)

# 2012 – Cuidado Infantil
trat_2012 <- Cuidado_2012 |>
  filter(tramo == 8) |>
  mutate(
    prek_4_5_2012 = case_when(
      e7 %in% 1:3 ~ 1L,
      e7 == 4     ~ 0L,
      TRUE        ~ NA_integer_
    )
  ) |>
  select(folio, prek_4_5_2012) |>
  distinct(folio, .keep_all = TRUE)

tratamiento_prek <- trat_2010 |>
  full_join(trat_2012, by = "folio") |>
  mutate(
    treat_prek_4_5 = dplyr::coalesce(prek_4_5_2010, prek_4_5_2012)
  ) |>
  select(folio, treat_prek_4_5, prek_4_5_2010, prek_4_5_2012)

############################################################
#### 9. Base final: merge + consolidación gestación (coalesce 2012/2010)
############################################################

base_final <- base_tvip |>
  left_join(hogar_2010_cp,    by = "folio") |>
  left_join(hogar_2012_cp,    by = "folio") |>
  left_join(tratamiento_prek, by = "folio") |>
  left_join(embarazo_2010,    by = "folio") |>
  left_join(embarazo_2012,    by = "folio") |>
  mutate(
    across(
      c(
        emb_nutr_estado_2010, emb_fuma_2010, emb_cigs_mes_2010,
        emb_alcohol_2010, emb_medicamentos_2010, emb_drogas_2010,
        emb_nutr_estado_2012, emb_fuma_2012, emb_cigs_mes_2012,
        emb_alcohol_2012, emb_medicamentos_2012, emb_drogas_2012
      ),
      ~ haven::zap_labels(.)
    ),
    emb_cigs_mes_2010 = ifelse(emb_cigs_mes_2010 == 999, NA, emb_cigs_mes_2010),
    emb_cigs_mes_2012 = ifelse(emb_cigs_mes_2012 == 999, NA, emb_cigs_mes_2012),
    emb_nutr_estado = dplyr::coalesce(emb_nutr_estado_2012, emb_nutr_estado_2010),
    emb_fuma        = dplyr::coalesce(emb_fuma_2012,        emb_fuma_2010),
    emb_cigs_mes    = dplyr::coalesce(emb_cigs_mes_2012,    emb_cigs_mes_2010),
    emb_alcohol     = dplyr::coalesce(emb_alcohol_2012,     emb_alcohol_2010),
    emb_medicamentos= dplyr::coalesce(emb_medicamentos_2012,emb_medicamentos_2010),
    emb_drogas      = dplyr::coalesce(emb_drogas_2012,      emb_drogas_2010)
  ) |>
  select(
    -emb_nutr_estado_2010, -emb_fuma_2010, -emb_cigs_mes_2010,
    -emb_alcohol_2010, -emb_medicamentos_2010, -emb_drogas_2010,
    -emb_nutr_estado_2012, -emb_fuma_2012, -emb_cigs_mes_2012,
    -emb_alcohol_2012, -emb_medicamentos_2012, -emb_drogas_2012
  )

# (Opcional) guardar base_final por fuera del PSM
saveRDS(
  base_final,
  file = file.path(datos, "base_final.rds")
)

############################################################
#### 10. Construir base_psm: sólo variables necesarias para PSM
############################################################

base_psm <- base_final |>
  mutate(
    #--------------------------------------------------------
    # 1. Resultado: TVIP estandarizado
    #--------------------------------------------------------
    tvip_z = as.numeric(scale(tvip_score)),
    
    #--------------------------------------------------------
    # 2. Educación del cuidador – homogenizar 2012 → escala 2010
    #--------------------------------------------------------
    cp_edu_nivel_2010_raw = as.numeric(cp_edu_nivel_2010),
    cp_edu_nivel_2012_raw = as.numeric(cp_edu_nivel_2012),
    
    # Mapear escala 2012 → 2010
    cp_edu_nivel_2012_2010 = case_when(
      cp_edu_nivel_2012_raw %in% 1:4   ~ 2L,   # Sala cuna / jardín / PreK / Kínder → básica
      cp_edu_nivel_2012_raw == 5      ~ 1L,   # Preparatoria
      cp_edu_nivel_2012_raw == 6      ~ 2L,   # Básica
      cp_edu_nivel_2012_raw == 7      ~ 3L,   # Diferencial
      cp_edu_nivel_2012_raw == 8      ~ 4L,   # Humanidades
      cp_edu_nivel_2012_raw == 9      ~ 5L,   # Media científico-humanista
      cp_edu_nivel_2012_raw == 10     ~ 6L,   # Técnica/Comercial/Industrial/Normalista
      cp_edu_nivel_2012_raw == 11     ~ 7L,   # Media técnico profesional
      cp_edu_nivel_2012_raw == 12     ~ 8L,   # CFT incompleta
      cp_edu_nivel_2012_raw == 13     ~ 9L,   # CFT completa
      cp_edu_nivel_2012_raw == 14     ~ 10L,  # IP incompleta
      cp_edu_nivel_2012_raw == 15     ~ 11L,  # IP completa
      cp_edu_nivel_2012_raw == 16     ~ 12L,  # Univ incompleta
      cp_edu_nivel_2012_raw == 17     ~ 13L,  # Univ completa
      cp_edu_nivel_2012_raw == 18     ~ 14L,  # Postgrado
      cp_edu_nivel_2012_raw == 19     ~ 19L,  # Ninguno
      cp_edu_nivel_2012_raw == 88     ~ 88L,  # No responde
      cp_edu_nivel_2012_raw == 99     ~ 99L,  # No sabe
      TRUE                             ~ NA_integer_
    ),
    
    # Consolidar: primero 2012 homogenizado, luego 2010
    cp_edu_nivel = dplyr::coalesce(cp_edu_nivel_2012_2010, cp_edu_nivel_2010_raw),
    
    # Colapsar en 3 categorías
    cp_edu_nivel_cat = case_when(
      cp_edu_nivel %in% c(19, 1:4) ~ "baja",
      cp_edu_nivel %in% 5:7        ~ "media",
      cp_edu_nivel %in% 8:14       ~ "alta",
      TRUE                         ~ NA_character_
    ),
    cp_edu_nivel_cat = factor(cp_edu_nivel_cat,
                              levels = c("baja", "media", "alta")),
    
    #--------------------------------------------------------
    # 3. Ingresos del cuidador: consolidar 2012/2010
    #--------------------------------------------------------
    cp_inc_sueldos_2010_num       = as.numeric(cp_inc_sueldos_2010),
    cp_inc_en_especie_2010_num    = as.numeric(cp_inc_en_especie_2010),
    cp_inc_independiente_2010_num = as.numeric(cp_inc_independiente_2010),
    cp_inc_sueldos_2012_num       = as.numeric(cp_inc_sueldos_2012),
    cp_inc_en_especie_2012_num    = as.numeric(cp_inc_en_especie_2012),
    cp_inc_independiente_2012_num = as.numeric(cp_inc_independiente_2012),
    
    cp_inc_sueldos       = dplyr::coalesce(cp_inc_sueldos_2012_num,       cp_inc_sueldos_2010_num),
    cp_inc_en_especie    = dplyr::coalesce(cp_inc_en_especie_2012_num,    cp_inc_en_especie_2010_num),
    cp_inc_independiente = dplyr::coalesce(cp_inc_independiente_2012_num, cp_inc_independiente_2010_num),
    
    cp_inc_total = cp_inc_sueldos + cp_inc_en_especie + cp_inc_independiente,
    cp_inc_total = if_else(cp_inc_total <= 0 | is.na(cp_inc_total),
                           NA_real_, cp_inc_total),
    ln_cp_inc_total = log(cp_inc_total),
    
    #--------------------------------------------------------
    # 4. Desocupado (consolidado)
    #--------------------------------------------------------
    cp_desocupado_2012 = if_else(
      !is.na(cp_lab_trabajo_semana_2012) & !is.na(cp_lab_tiene_empleo_2012),
      as.integer(cp_lab_trabajo_semana_2012 == 0 & cp_lab_tiene_empleo_2012 == 0),
      NA_integer_
    ),
    cp_desocupado_2010 = if_else(
      !is.na(cp_lab_trabajo_semana_2010) & !is.na(cp_lab_tiene_empleo_2010),
      as.integer(cp_lab_trabajo_semana_2010 == 0 & cp_lab_tiene_empleo_2010 == 0),
      NA_integer_
    ),
    cp_desocupado = dplyr::coalesce(cp_desocupado_2012, cp_desocupado_2010),
    
    #--------------------------------------------------------
    # 5. Gestación: dummies simplificadas
    #--------------------------------------------------------
    emb_nutr_estado_clean = dplyr::na_if(emb_nutr_estado, 8),
    emb_nutr_estado_clean = dplyr::na_if(emb_nutr_estado_clean, 9),
    
    peso_normal_emb = case_when(
      emb_nutr_estado_clean == 2            ~ 1L,
      emb_nutr_estado_clean %in% c(1, 3, 4) ~ 0L,
      TRUE                                  ~ NA_integer_
    ),
    
    fuma_emb = case_when(
      emb_fuma == 1 ~ 1L,
      emb_fuma == 2 ~ 0L,
      TRUE          ~ NA_integer_
    ),
    
    alcohol_emb = case_when(
      emb_alcohol %in% c(2, 3) ~ 1L,
      emb_alcohol == 1         ~ 0L,
      TRUE                     ~ NA_integer_
    ),
    
    #--------------------------------------------------------
    # 6. Tratamiento limpio 0/1
    #--------------------------------------------------------
    treat = as.integer(treat_prek_4_5 == 1)
  ) |>
  # Sólo niños con info clave
  filter(
    !is.na(treat_prek_4_5),
    !is.na(tvip_score),
    !is.na(edad_meses_tvip)
  ) |>
  # Seleccionar sólo variables necesarias para PSM
  select(
    folio,
    # outcome
    tvip_score,
    tvip_z,
    edad_meses_tvip,
    # tratamiento
    treat_prek_4_5,
    treat,
    # educación cuidador (homogenizada)
    cp_edu_nivel,
    cp_edu_nivel_cat,
    # ingresos
    cp_inc_total,
    ln_cp_inc_total,
    # trabajo
    cp_desocupado,
    # gestación bruta y dummies
    emb_nutr_estado,
    emb_fuma,
    emb_alcohol,
    emb_medicamentos,
    emb_drogas,
    emb_cigs_mes,
    peso_normal_emb,
    fuma_emb,
    alcohol_emb
  )

# Chequeo rápido
nrow(base_psm)
dplyr::n_distinct(base_psm$folio)

# Guardar base limpia para PSM
saveRDS(
  base_psm,
  file = file.path(datos, "base_psm.rds")
)

############################################################
#### 11. Comentarios metodológicos sobre la selección de covariables
############################################################
# (1) ¿Por qué acotar el conjunto de covariables?
# 
#   - El objetivo del PSM es aproximar la independencia condicional:
#       (Y(0), Y(1)) ⟂ D | X
#     usando un conjunto X de covariables pretratamiento que capturen la
#     selección al tratamiento. En teoría, "más controles" podría ayudar,
#     pero en la práctica:
#       * Aumenta el riesgo de p(X) ≈ 0 o 1 (probabilidades ajustadas 0/1),
#         lo que rompe el soporte común y genera pesos extremos en IPW.
#       * Incrementa la varianza de los estimadores (malas coincidencias en
#         el matching, pseudo-tratados con pocos controles comparables, etc.).
#       * Hace más opaco el diseño: se vuelve difícil justificar por qué cada
#         covariable entra al PS y qué papel juega.
#
#   - Las covariables seleccionadas aquí son:
#       * Características del niño al momento del test:
#           - edad_meses_tvip.
#       * Capital humano y recursos del cuidador:
#           - cp_edu_nivel (homogeneizado 2012→2010).
#           - cp_edu_nivel_cat (baja / media / alta).
#           - ln_cp_inc_total (ingreso laboral del hogar/cuidador).
#           - cp_desocupado (aproxima fragilidad en el mercado laboral).
#       * Condiciones de gestación:
#           - emb_nutr_estado, emb_fuma, emb_alcohol,
#             emb_medicamentos, emb_drogas, emb_cigs_mes,
#             y dummies simplificadas: peso_normal_emb, fuma_emb, alcohol_emb.
#
#   - Todas estas covariables tienen dos propiedades clave:
#       (i) Son pretratamiento (definidas antes o durante el embarazo / primera infancia).
#       (ii) Tienen un canal plausible tanto hacia la probabilidad de asistir
#            a pre-kínder como hacia el desarrollo cognitivo (TVIP).
#
# (2) ¿Qué se está dejando por fuera?
#
#   - La ELPI permite incluir muchas más variables:
#       * Otras características del hogar (composición, infraestructura, etc.).
#       * Más detalles laborales del cuidador y otros miembros.
#       * Variables del niño que pueden estar ya afectadas por el tratamiento
#         (por ejemplo, indicadores de estimulación o asistencia a otros
#         establecimientos más cercanos en tiempo a la medición).
#
#   - No se incluyen por varias razones:
#       * Riesgo de "kitchen sink PS": meter todo lo disponible sin una lógica
#         clara aumenta la dimensionalidad de X, complica el soporte común y
#         empeora el matching sin necesariamente mejorar la calidad causal.
#       * Algunas variables pueden estar ya afectadas por el propio tratamiento
#         o por decisiones educativas intermedias, lo que las convertiría en
#         mediadoras o colliders en lugar de verdaderos predeterminados.
#       * El tamaño muestral efectivo (en controles) no es tan grande; si se
#         saturara el logit con demasiadas dummies, aparecería con frecuencia
#         el problema de "fitted probabilities numerically 0 or 1".
#
# (3) Riesgo metodológico de este recorte
#
#   - La decisión de acotar las covariables NO garantiza que se haya logrado
#     el conjunto "mínimo suficiente". Quedan varios riesgos:
#       * Omisión de variables relevantes (por ejemplo, medidas previas de
#         habilidades del niño o indicadores más finos de entorno familiar)
#         que podrían seguir sesgando el ATT, incluso después de hacer PSM.
#       * Posibles variables de confusión no observadas (preferencias de los
#         padres por educación, redes de información, etc.) que ningún PSM
#         puede corregir.
#
#       * El PSM descansa en el supuesto de independencia condicional dado X.
#       * X se construye a partir de teoría (economía de la educación, desarrollo
#         infantil) y de restricciones prácticas (tamaño muestral, soporte común).
#       * No se reclama identificación "perfecta", sino una mejora sustantiva
#         respecto al estimador ingenuo de diferencias de medias.
#
# (4) Enfoques alternativos que se podrían haber considerado
#
#   - Especificaciones más ricas del PS:
#       * Incluir términos cuadráticos o interacciones (por ejemplo,
#         educación × ingreso) si el tamaño muestral lo permite.
#       * Incluir medidas previas de desempeño cognitivo (TVIP 2010), cuando
#         existan, como indicador directo de habilidades iniciales.
#
#   - Enfoques más estructurados de selección de covariables:
#       * Uso de algoritmos de selección (p.ej. lasso para el PS) con
#         posterior chequeo de balance en lugar de confiar solo en juicio
#         manual.
#       * Diseños alternativos (DiD, control sintético, etc.) si se contara
#         con más rondas y variación de política en el tiempo.
#
# En síntesis:
#   - El conjunto de covariables aquí definido busca un compromiso entre
#     (i) relevancia económica, (ii) temporalidad pretratamiento y
#     (iii) viabilidad estadística (evitar p(X) ≈ 0/1 y pérdida severa
#          de soporte común).
#   - El ejercicio es defendible, pero no único: otras especificaciones
#     del PS son posibles y deben tratarse como ejercicios de robustez,
#     no como meras variaciones arbitrarias.
############################################################