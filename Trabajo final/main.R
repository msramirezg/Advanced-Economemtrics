#### 0. Paquetes y rutas ----
library(dplyr)
library(ggplot2)
library(readr)
library(broom)
library(MatchIt)

#### 2. Directorio de trabajo y rutas
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

root    <- getwd()
datos   <- file.path(root, "data")
results <- file.path(root, "results")
if (!dir.exists(results)) dir.create(results, recursive = TRUE)


base_final <- readRDS(file.path(datos, "base_final.rds"))

#### 1. Preparar datos de análisis ----

datos <- base_final |>
  mutate(
    # TVIP estandarizado
    tvip_std = as.numeric(scale(tvip_score)),
    # Tratamiento numérico 0/1
    D = as.integer(treat_prek_4_5),
    # Tratamiento como factor con etiquetas
    treat_prek = factor(
      D,
      levels = c(0, 1),
      labels = c("No asistió a Pre-Kínder", "Asistió a Pre-Kínder")
    ),
    # Ingreso laboral del cuidador en 2012 (sueldos) en log(1 + x)
    log_inc_sueldos_2012 = log(1 + cp_inc_sueldos_2012)
  ) |>
  # filtro por casos con info completa para PSM (mínimo viable)
  filter(
    !is.na(tvip_std),
    !is.na(D),
    !is.na(edad_meses_tvip),
    !is.na(cp_edu_nivel_2012),
    !is.na(log_inc_sueldos_2012)
  )

#### 2. Estadísticas descriptivas básicas ----

vars_desc <- c("tvip_std", "D", "edad_meses_tvip",
               "cp_edu_nivel_2012", "log_inc_sueldos_2012")

resumen_simple <- function(x) {
  c(
    media = mean(x, na.rm = TRUE),
    sd    = sd(x, na.rm = TRUE),
    n     = sum(!is.na(x))
  )
}

tabla_desc <- datos |>
  summarise(
    tvip_std              = resumen_simple(tvip_std),
    D                     = resumen_simple(D),
    edad_meses_tvip       = resumen_simple(edad_meses_tvip),
    cp_edu_nivel_2012     = resumen_simple(cp_edu_nivel_2012),
    log_inc_sueldos_2012  = resumen_simple(log_inc_sueldos_2012)
  ) |>
  tidyr::pivot_longer(
    everything(),
    names_to  = "variable",
    values_to = "valor"
  ) |>
  tidyr::separate(variable, into = c("var", "estadistico"), sep = "_(?=media|sd|n)") |>
  tidyr::pivot_wider(names_from = estadistico, values_from = valor)

write_csv(tabla_desc, file.path(results, "tabla_descriptivas_tvip.csv"))

#### 3. Gráficos descriptivos de TVIP ----

# Histograma por tratamiento
p_hist <- ggplot(datos, aes(x = tvip_std)) +
  geom_histogram(bins = 25, color = "white") +
  facet_wrap(~ treat_prek) +
  labs(
    title    = "Distribución de puntajes TVIP estandarizados",
    x        = "TVIP (estandarizado)",
    y        = "Frecuencia"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(results, "fig_hist_tvip_prekinder.pdf"),
  plot     = p_hist,
  width    = 7,
  height   = 5
)

# Boxplot por tratamiento
p_box <- ggplot(datos, aes(x = treat_prek, y = tvip_std)) +
  geom_boxplot() +
  labs(
    title = "TVIP estandarizado por asistencia a Pre-Kínder",
    x     = "",
    y     = "TVIP (estandarizado)"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(results, "fig_box_tvip_prekinder.pdf"),
  plot     = p_box,
  width    = 7,
  height   = 5
)

# Diferencias de medias en TVIP por tratamiento
diff_stats <- datos |>
  group_by(treat_prek) |>
  summarise(
    media = mean(tvip_std, na.rm = TRUE),
    sd    = sd(tvip_std, na.rm = TRUE),
    n     = sum(!is.na(tvip_std)),
    .groups = "drop"
  ) |>
  mutate(
    se    = sd / sqrt(n),
    ic_lo = media - 1.96 * se,
    ic_hi = media + 1.96 * se
  )

p_diff <- ggplot(diff_stats, aes(x = treat_prek, y = media)) +
  geom_col() +
  geom_errorbar(aes(ymin = ic_lo, ymax = ic_hi), width = 0.15) +
  labs(
    title = "Media de TVIP estandarizado por asistencia a Pre-Kínder",
    x     = "",
    y     = "Media de TVIP (estandarizado)"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(results, "fig_diff_means_tvip.pdf"),
  plot     = p_diff,
  width    = 7,
  height   = 5
)

#### 4. Propensity Score Matching ----

# Fórmula del propensity score (ajústala si incorporas más covariables)
form_ps <- D ~ edad_meses_tvip + cp_edu_nivel_2012 + log_inc_sueldos_2012

# PSM: vecino más cercano 1:1, distancia logit, caliper moderado
m_ps <- matchit(
  form_ps,
  data    = datos,
  method  = "nearest",
  distance = "logit",
  ratio   = 1,
  caliper = 0.2  # en unidades de sd del logit del pscore
)

# Datos emparejados
dat_match <- match.data(m_ps)

#### 5. ATT vía PSM ----

# Regresión simple en la muestra emparejada con pesos de matching
mod_att <- lm(tvip_std ~ D, data = dat_match, weights = weights)

res_att <- tidy(mod_att)

write_csv(res_att, file.path(results, "resultados_psm_att_tvip.csv"))

# El coeficiente de D es el ATT estimado
print(res_att)

#### 6. Gráfico de distribución de propensity score antes y después ----

# Antes de matching
pscore_raw <- data.frame(
  pscore = m_ps$distance,
  D      = datos$D
)

p_pscore_before <- ggplot(pscore_raw, aes(x = pscore, fill = factor(D))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("grey60", "grey20"),
                    labels = c("Control", "Tratado"),
                    name   = "Grupo") +
  labs(
    title = "Distribución del propensity score antes del matching",
    x     = "Propensity score estimado",
    y     = "Densidad"
  ) +
  theme_minimal()

# Después de matching (usamos solo muestra emparejada)
pscore_match <- dat_match |>
  transmute(
    pscore = .distance,
    D      = D
  )

p_pscore_after <- ggplot(pscore_match, aes(x = pscore, fill = factor(D))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("grey60", "grey20"),
                    labels = c("Control", "Tratado"),
                    name   = "Grupo") +
  labs(
    title = "Distribución del propensity score después del matching",
    x     = "Propensity score emparejado",
    y     = "Densidad"
  ) +
  theme_minimal()

# Combinar en una sola figura (o guarda dos; aquí uso patchwork si lo tienes cargado)
# library(patchwork)
p_pscore_both <- p_pscore_before + p_pscore_after + plot_layout(ncol = 1)

ggsave(
  filename = file.path(results, "fig_pscore_overlap.pdf"),
  plot     = p_pscore_both,
  width    = 7,
  height   = 8
)

#### 7. Balance de covariables antes y después del PSM ----

covs <- c("edad_meses_tvip", "cp_edu_nivel_2012", "log_inc_sueldos_2012")

# Función para diferencias estandarizadas
smd <- function(x_t, x_c) {
  mt <- mean(x_t, na.rm = TRUE)
  mc <- mean(x_c, na.rm = TRUE)
  vt <- var(x_t, na.rm = TRUE)
  vc <- var(x_c, na.rm = TRUE)
  (mt - mc) / sqrt((vt + vc) / 2)
}

# Antes del matching
bal_before <- lapply(covs, function(v) {
  x_t <- datos |> filter(D == 1) |> pull(all_of(v))
  x_c <- datos |> filter(D == 0) |> pull(all_of(v))
  data.frame(
    variable = v,
    smd     = smd(x_t, x_c),
    etapa   = "Antes"
  )
}) |> bind_rows()

# Después del matching (usar pesos implícitos a través de réplica de observaciones ya está incorporado en dat_match)
bal_after <- lapply(covs, function(v) {
  x_t <- dat_match |> filter(D == 1) |> pull(all_of(v))
  x_c <- dat_match |> filter(D == 0) |> pull(all_of(v))
  data.frame(
    variable = v,
    smd     = smd(x_t, x_c),
    etapa   = "Después"
  )
}) |> bind_rows()

balance_all <- bind_rows(bal_before, bal_after)

write_csv(balance_all, file.path(results, "tabla_balance_psm.csv"))

# Gráfico de diferencias estandarizadas
p_balance <- ggplot(balance_all,
                    aes(x = smd, y = variable, color = etapa, group = etapa)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Diferencias estandarizadas de covariables\nantes y después del PSM",
    x     = "Diferencia estandarizada (tratado - control)",
    y     = "Covariable",
    color = "Etapa"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(results, "fig_balance_covariables.pdf"),
  plot     = p_balance,
  width    = 7,
  height   = 5
)
