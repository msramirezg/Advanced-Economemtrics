############################################################
#### Trabajo final Econometría Avanzada – Módulo PSM
#### Proyecto: Acceso a educación parvularia y TVIP
#### Autor: Mahicol Ramírez
#### Fecha: 5 de diciembre de 2025
############################################################

# Pregunta: ¿Asistir a educación preescolar no obligatoria (Pre-Kínder)
#           entre 4–5 años mejora el puntaje TVIP al ingreso a Kínder?
# Resultado: Y_i = TVIP (estandarizado).
# Tratamiento: D_i = 1 si asistió a educación preescolar/parvularia
#               entre 4–5 años (treat_prek_4_5 == 1), 0 si no.
# Objetivo: Estimar el ATT usando PSM e IPW, comparándolo con el estimador ingenuo.

rm(list = ls())

#### 1. Paquetes
library(dplyr)
library(ggplot2)
library(rstudioapi)
library(MatchIt)    # Propensity Score Matching
library(cobalt)     # Balance y love plots
library(broom)      # Resultados en formato tidy
library(sandwich)   # Varianzas robustas
library(lmtest)     # coeftest

#### 2. Directorio y carga de base PSM
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

root    <- getwd()
datos   <- file.path(root, "data")
results <- file.path(root, "results")
if (!dir.exists(results)) dir.create(results, recursive = TRUE)

# La base base_psm.rds ya consolida:
# - Resultado (tvip_score, edad_meses_tvip)
# - Tratamiento (treat_prek_4_5, treat)
# - Covariables pretratamiento del cuidador y gestación.
data_psm <- readRDS(file.path(datos, "base_psm.rds"))

############################################################
#### 3. Preparación mínima de datos para PSM
############################################################
# Covariables de interés para PSM (todas medidas antes del resultado):
#   - edad_meses_tvip: edad al test (madurez cognitiva).
#   - cp_edu_nivel_cat: capital humano del cuidador (baja/media/alta).
#   - ln_cp_inc_total: log ingreso laboral del hogar/cuidador.
#   - peso_normal_emb: indicador de estado nutricional normal en gestación.
#   - fuma_emb: indicador de tabaquismo en gestación.
#   - alcohol_emb: indicador de consumo de alcohol en gestación.
covariables <- c(
  "edad_meses_tvip",
  "cp_edu_nivel",
  "cp_edu_nivel_cat",
  "ln_cp_inc_total",
  "peso_normal_emb",
  "fuma_emb",
  "alcohol_emb"
)

############################################################
#### 4. Estadísticas descriptivas iniciales
############################################################

# Descriptivos por grupo de tratamiento
desc_por_grupo <- data_psm |>
  group_by(treat) |>
  summarise(
    N                = n(),
    mean_tvip_z      = mean(tvip_z, na.rm = TRUE),
    sd_tvip_z        = sd(tvip_z,   na.rm = TRUE),
    mean_edad_meses  = mean(edad_meses_tvip, na.rm = TRUE),
    mean_ln_ingreso  = mean(ln_cp_inc_total,  na.rm = TRUE),
    prop_peso_normal = mean(peso_normal_emb,  na.rm = TRUE),
    prop_fuma        = mean(fuma_emb,         na.rm = TRUE),
    prop_alcohol     = mean(alcohol_emb,      na.rm = TRUE),
    .groups          = "drop"
  )

print(desc_por_grupo)

# Histograma de TVIP bruto por tratamiento
p_hist_tvip <- ggplot(data_psm, aes(x = tvip_score, fill = factor(treat))) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30, color = "white") +
  scale_fill_manual(
    name   = "Tratamiento",
    values = c("0" = "#1b9e77", "1" = "#d95f02"),
    labels = c("0" = "Controles", "1" = "Tratados")
  ) +
  labs(
    x = "Puntaje TVIP (bruto)",
    y = "Frecuencia",
    #title = "Distribución del puntaje TVIP por asistencia a Pre-Kínder"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

print(p_hist_tvip)

ggsave(
  filename = file.path(results, "fig_hist_tvip_prekinder.pdf"),
  plot     = p_hist_tvip,
  width    = 7, height = 5
)

############################################################
#### 5. Estimador ingenuo: diferencia de medias y prueba de hipótesis
############################################################

# Modelo: Y_i = α + τ_naive * D_i + u_i
# τ_naive es la diferencia de medias entre tratados y controles sin ajustar.

# Prueba t clásica
diff_means_test <- t.test(tvip_z ~ treat, data = data_psm)
print(diff_means_test)

# Regresión lineal con errores robustos (HC1)
naive_lm <- lm(tvip_z ~ treat, data = data_psm)
naive_res <- coeftest(naive_lm, vcov = vcovHC(naive_lm, type = "HC1"))
print(naive_res)

# Guardar τ̂_naive en formato tidy
naive_tidy <- broom::tidy(naive_lm, conf.int = TRUE) |>
  filter(term == "treat") |>
  mutate(
    estimador = "Naive (OLS)",
    tipo      = "Antes de PSM"
  )

############################################################
#### 6. Tabla de balance de covariables (antes de PSM)
############################################################

# Función para construir tabla de balance simple
make_balance_table <- function(df, treat_var, vars) {
  out <- lapply(vars, function(v) {
    x_t <- df[df[[treat_var]] == 1, v][[1]]
    x_c <- df[df[[treat_var]] == 0, v][[1]]
    
    x_t_num <- as.numeric(x_t)
    x_c_num <- as.numeric(x_c)
    
    mt <- mean(x_t_num, na.rm = TRUE)
    mc <- mean(x_c_num, na.rm = TRUE)
    st <- sd(x_t_num,   na.rm = TRUE)
    sc <- sd(x_c_num,   na.rm = TRUE)
    
    s_pooled <- sqrt((st^2 + sc^2) / 2)
    std_diff <- (mt - mc) / s_pooled
    
    data.frame(
      variable     = v,
      mean_treated = mt,
      mean_control = mc,
      sd_treated   = st,
      sd_control   = sc,
      std_diff     = std_diff
    )
  })
  
  do.call(rbind, out)
}

balance_before <- make_balance_table(
  df        = data_psm,
  treat_var = "treat",
  vars      = c("edad_meses_tvip", "ln_cp_inc_total",
                "peso_normal_emb", "fuma_emb", "alcohol_emb")
)

print(balance_before)

write.csv(
  balance_before,
  file.path(results, "tabla_balance_before_psm.csv"),
  row.names = FALSE
)

############################################################
#### 7. Supuesto de independencia condicional y selección de X
############################################################

# Supuesto de independencia condicional:
#   (Y_i(0), Y_i(1)) ⟂ D_i | X_i
#
# En este contexto, X_i incluye:
#  - Edad al test (edad_meses_tvip),
#  - Capital humano del cuidador (cp_edu_nivel_cat),
#  - Log ingreso del hogar (ln_cp_inc_total),
#  - Condiciones de gestación (peso_normal_emb, fuma_emb, alcohol_emb).
#
# La idea es capturar el proceso de selección al tratamiento mediante
# covariables pretratamiento que estén correlacionadas tanto con D_i
# como con el resultado potencial Y_i(d). Este supuesto no es testeable
# directamente, pero el balance de X después del emparejamiento da
# evidencia indirecta sobre su plausibilidad.

############################################################
#### 8. Paso 1: Estimar el propensity score p̂_i
############################################################

# Variables que entran al modelo de PS
vars_ps <- c(
  "treat",
  "edad_meses_tvip",
  "cp_edu_nivel_cat",
  "ln_cp_inc_total",
  "peso_normal_emb",
  "fuma_emb",
  "alcohol_emb"
)

# Restricción práctica: eliminar observaciones con NA en cualquiera de estas X
# Esto reduce la muestra efectiva y es una de las limitaciones empíricas.
data_psm_ps <- data_psm |>
  filter(
    if_all(all_of(vars_ps), ~ !is.na(.))
  )

# Modelo logit para el PS
ps_model <- glm(
  treat ~ edad_meses_tvip + cp_edu_nivel_cat + ln_cp_inc_total +
    peso_normal_emb + fuma_emb + alcohol_emb,
  data   = data_psm_ps,
  family = binomial(link = "logit")
)

summary(ps_model)

data_psm_ps <- data_psm_ps |>
  mutate(
    pscore   = predict(ps_model, type = "response"),
    logit_ps = predict(ps_model, type = "link")
  )

# Distribución de p̂_i por tratamiento (antes de restringir soporte común)
p_pscore <- ggplot(data_psm_ps, aes(x = pscore, fill = factor(treat))) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30, color = "white") +
  scale_fill_manual(
    name   = "Tratamiento",
    values = c("0" = "#1b9e77", "1" = "#d95f02"),
    labels = c("0" = "Controles", "1" = "Tratados")
  ) +
  labs(
    x = "Propensity score estimado",
    y = "Frecuencia",
#    title = "Distribución del propensity score por tratamiento (antes de soporte común)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

print(p_pscore)

ggsave(
  filename = file.path(results, "fig_pscore_before_matching.pdf"),
  plot     = p_pscore,
  width    = 7, height = 5
)

############################################################
#### 9. Paso 2: Región de soporte común
############################################################

# Cálculo de la intersección de rangos de p̂_i para tratados y controles
ps_range <- data_psm_ps |>
  group_by(treat) |>
  summarise(
    min_ps = min(pscore, na.rm = TRUE),
    max_ps = max(pscore, na.rm = TRUE),
    .groups = "drop"
  )

print(ps_range)

ps_min <- max(ps_range$min_ps)
ps_max <- min(ps_range$max_ps)

# Restricción explícita al soporte común S = [ps_min, ps_max]
data_psm_cs <- data_psm_ps |>
  filter(pscore >= ps_min, pscore <= ps_max)

# Esta restricción elimina observaciones con p̂_i muy extremos
# (tratados sin contrafactual creíble o controles sin análogos tratados).

############################################################
#### 10. Paso 3: Algoritmo de emparejamiento
############################################################

# Algoritmo: nearest neighbor matching sobre el logit del PS
# - Estimando ATT.
# - Con reemplazo: un control puede servir como clon de varios tratados.
# - Caliper en la escala del logit para evitar emparejamientos muy lejanos.

match_formula <- treat ~ edad_meses_tvip + cp_edu_nivel_cat + ln_cp_inc_total +
  peso_normal_emb + fuma_emb + alcohol_emb

m_nn <- matchit(
  formula  = match_formula,
  data     = data_psm_cs,
  method   = "nearest",
  distance = "logit",
  replace  = TRUE,
  caliper  = 0.2,
  estimand = "ATT"
)

summary(m_nn)

# Datos emparejados con pesos de matching
matched_data <- match.data(m_nn)

# En matched_data, los pesos definen el "clon" sintético:
# cada tratado se compara con uno o varios controles que comparten
# valores similares de p̂_i, ponderados por weights.

############################################################
#### 11. Balance después del matching
############################################################

bal_after <- bal.tab(
  m_nn,
  un          = TRUE,   # mostrar también balance sin ajustar
  m.threshold = 0.1     # umbral de |SMD| para considerar buen balance
)

print(bal_after)

p_love <- love.plot(
  m_nn,
  stats     = "mean.diffs",
  abs       = TRUE,
  threshold = 0.1
) +
  theme_minimal(base_size = 12)

print(p_love)

ggsave(
  filename = file.path(results, "fig_loveplot_psm.pdf"),
  plot     = p_love,
  width    = 7, height = 5
)

############################################################
#### 12. Paso 4: Estimación del ATT en la muestra emparejada
############################################################

# Modelo en muestra emparejada:
#   Y_i = α + τ_ATT * D_i + u_i
# estimado por MCO ponderado usando los pesos de matching.

att_match_lm <- lm(
  tvip_z ~ treat,
  data    = matched_data,
  weights = weights
)

att_match_res <- coeftest(att_match_lm,
                          vcov = vcovHC(att_match_lm, type = "HC1"))
print(att_match_res)

att_psm_tidy <- broom::tidy(att_match_lm, conf.int = TRUE) |>
  filter(term == "treat") |>
  mutate(
    estimador = "ATT (PSM nearest)",
    tipo      = "Emparejado"
  )

############################################################
#### 13. IPW (Inverse Probability Weighting) como comparación
############################################################

# Definición de pesos para ATT:
#   w_i^ATT = 1                  si D_i = 1
#             p̂_i / (1 - p̂_i)   si D_i = 0
#
# Uso la misma muestra con soporte común (data_psm_cs).

data_psm_cs <- data_psm_cs |>
  mutate(
    w_ate = if_else(treat == 1, 1 / pscore, 1 / (1 - pscore)),
    w_att = if_else(treat == 1, 1, pscore / (1 - pscore))
  )

summary(data_psm_cs$w_att)

# Distribución de pesos ATT, truncando el eje x en el percentil 99
p_watt <- ggplot(data_psm_cs, aes(x = w_att)) +
  geom_histogram(bins = 30, fill = "#7570b3", color = "white") +
  coord_cartesian(xlim = c(0, quantile(data_psm_cs$w_att, 0.99, na.rm = TRUE))) +
  labs(
    x = "Pesos IPW (ATT)",
    y = "Frecuencia",
    title = "Distribución de pesos IPW para ATT (región de soporte común)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank()
  )

print(p_watt)

ggsave(
  filename = file.path(results, "fig_weights_ipw_att.pdf"),
  plot     = p_watt,
  width    = 7, height = 5
)

# Estimación de ATT con IPW
ipw_att_lm <- lm(
  tvip_z ~ treat,
  data    = data_psm_cs,
  weights = w_att
)

ipw_att_res <- coeftest(ipw_att_lm,
                        vcov = vcovHC(ipw_att_lm, type = "HC1"))
print(ipw_att_res)

ipw_tidy <- broom::tidy(ipw_att_lm, conf.int = TRUE) |>
  filter(term == "treat") |>
  mutate(
    estimador = "ATT (IPW)",
    tipo      = "Ponderado"
  )

############################################################
#### 14. Resumen comparativo de estimadores
############################################################

resultados_ATT <- bind_rows(
  naive_tidy,
  att_psm_tidy,
  ipw_tidy
) |>
  select(
    estimador, tipo,
    estimate, std.error, statistic,
    p.value, conf.low, conf.high
  ) |>
  rename(
    tau_hat   = estimate,
    se        = std.error,
    t_value   = statistic,
    p_value   = p.value,
    ci_low95  = conf.low,
    ci_high95 = conf.high
  )

print(resultados_ATT)

write.csv(
  resultados_ATT,
  file.path(results, "resumen_estimadores_psm.csv"),
  row.names = FALSE
)
