#### Ejercicio 2
# Proyecto: Evaluación de programa de incentivos con control sintético
# Autores: Mahicol Ramírez - Simón Briceño
# Fecha: 24 de noviembre de 2025

rm(list = ls())  # Limpiamos el entorno para evitar conflictos de objetos

#### 1. Cargar librerías necesarias para el análisis y la graficación
library(Synth)        # Implementa el método de control sintético
library(SCtools)      # Extensiones de Synth: placebos, mspe.test, etc.
library(haven)        # Permite leer bases en formato Stata (.dta)
library(dplyr)        # Facilita manipulación de datos tipo tabla
library(ggplot2)      # Permite construir gráficas con gramática de gráficos
library(scales)       # Proporciona etiquetas y escalas para ejes
library(rstudioapi)   # Permite fijar el directorio según la ruta del script
library(tidyr)        # Permite reestructurar datos (long ↔ wide)
library(patchwork)    # Une varias gráficas en un solo panel

##### 2. Directorio de trabajo y carga de datos
# Fijamos el directorio de trabajo en la carpeta del script para replicabilidad
this_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this_path)

# Definimos rutas a carpetas de datos y resultados para ordenar el proyecto
root       <- getwd()
datos      <- file.path(root, "datos")
resultados <- file.path(root, "Resultados")
if (!dir.exists(resultados)) dir.create(resultados, recursive = TRUE)

# Cargamos la base principal con información de ciudades y meses
df <- read_dta(file.path(datos, "CS_data.dta"))

colnames(df)
#### 3. Conversión de fechas y definición de variables básicas
# Función que convierte fechas Stata %tm a objetos Date (primer día del mes)
tm_to_date <- function(tm) {
  y <- 1960 + floor(tm / 12)
  m <- (tm %% 12) + 1
  as.Date(sprintf("%04d-%02d-01", y, m))
}

# Convertimos la variable date a fecha calendario y fijamos city_id como entero
df <- df |>
  mutate(
    date_m  = tm_to_date(as.integer(date)),  # Fecha mensual a partir de %tm
    city_id = as.integer(city_id)            # city_id como entero para Synth
  )

# Definimos la ciudad tratada y la fecha de inicio del programa de incentivos
treat_city       <- 8L
treat_start_date <- as.Date("2024-04-01")

# Creamos indicador de ciudad tratada para uso posterior en filtros y gráficas
df <- df |>
  mutate(
    treated_city = city_id == treat_city
  )

# Obtenemos el month_number correspondiente al inicio del tratamiento
treat_start_num <- df |>
  filter(date_m == treat_start_date) |>
  distinct(month_number) |>
  pull()

# Creamos tabla de equivalencia mes-fecha para usar en resultados del Synth
date_lookup <- df |>
  distinct(month_number, date_m) |>
  arrange(month_number)

#### 4. Tema base para todas las gráficas del ejercicio
# Definimos un tema común para asegurar formato homogéneo de las figuras
theme_base <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "grey30"),
    plot.caption     = element_text(color = "grey40", size = 9),
    axis.title.x     = element_blank(),
    axis.text.x      = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position  = "none"
  )

#### 5. Parámetros del tratamiento y control sintético para revenue
# Construimos log(revenue) para trabajar en escala log y comparar niveles
df <- df |>
  mutate(lrev = log(revenue))

# Definimos los meses pretratamiento para revenue (0 a 74: 2018-01 a 2024-03)
time_pre <- 0:74

# Identificamos ciudades de control excluyendo la ciudad tratada
control_units <- setdiff(sort(unique(df$city_id)), treat_city)

# Preparamos los datos para Synth usando lrev como dependiente
# Incluimos predictores que capturan estructura de mercado y siniestralidad
dataprep.out <- Synth::dataprep(
  foo                  = as.data.frame(df),
  dependent            = "lrev",
  unit.variable        = "city_id",
  time.variable        = "month_number",
  treatment.identifier = treat_city,
  controls.identifier  = control_units,
  time.predictors.prior = time_pre,
  time.optimize.ssr     = time_pre,
  time.plot             = sort(unique(df$month_number)),
  predictors = c(
    "drivers_active",
    "avg_income_per_hour",
    "total_trips",
    "accidents_std",
    "lrev"
  ),
  special.predictors = list(
    list("drivers_active",       72, "mean"),
    list("avg_income_per_hour",  72, "mean"),
    list("total_trips",          72, "mean"),
    list("accidents_std",        72, "mean"),
    list("lrev",                 72, "mean")
  )
)

# Estimamos el control sintético para revenue vía minimización de SSR
synth.out <- synth(dataprep.out)

# Extraemos tablas de balance de predictores y pesos para análisis en el informe
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth.out
)

# Guardamos tabla de balance para documentar calidad del ajuste pretratamiento
write.csv(
  synth.tables$tab.pred,
  file.path(resultados, "tabla_balance_synth.csv"),
  row.names = FALSE
)

# Guardamos pesos del control sintético para identificar ciudades donantes
write.csv(
  synth.tables$tab.w,
  file.path(resultados, "tabla_pesos_synth.csv"),
  row.names = FALSE
)

# Construimos trayectorias observada y sintética de lrev para toda la ventana
Y1        <- dataprep.out$Y1plot
Y0s       <- dataprep.out$Y0plot %*% synth.out$solution.w
time_plot <- dataprep.out$tag$time.plot

df_synth_abadie <- data.frame(
  month_number     = time_plot,
  date_m           = date_lookup$date_m[
    match(time_plot, date_lookup$month_number)
  ],
  lrevenue_treated = as.numeric(Y1),
  lrevenue_synth   = as.numeric(Y0s)
)

#### 6. Método alternativo: Pesos de Regresión (OLS) para revenue
# Construimos formato wide con una columna por ciudad para lrev
wide_df <- df |>
  select(month_number, date_m, city_id, lrev) |>
  arrange(month_number, city_id) |>
  pivot_wider(
    names_from  = city_id,
    values_from = lrev,
    names_prefix = "city_"
  ) |>
  arrange(month_number)

# Identificamos la columna de la tratada y las del grupo de control
city_cols         <- grep("^city_", names(wide_df), value = TRUE)
treated_col_name  <- paste0("city_", treat_city)
control_col_names <- setdiff(city_cols, treated_col_name)

# Separamos periodo pre y posttratamiento en términos de month_number
wide_pre  <- wide_df |> filter(month_number < treat_start_num)
wide_post <- wide_df |> filter(month_number >= treat_start_num)

# Matrices de serie para tratada y controles en pre y post
Y_pre_mat  <- as.matrix(wide_pre[, city_cols])
Y_post_mat <- as.matrix(wide_post[, city_cols])

treated_col_idx <- which(colnames(Y_pre_mat) == treated_col_name)
control_col_idx <- which(colnames(Y_pre_mat) != treated_col_name)

# Estimamos modelo OLS sin intercepto para obtener pesos de combinación
fmla    <- reformulate(control_col_names, response = treated_col_name)
ols_fit <- lm(update(fmla, . ~ . - 1), data = wide_pre)

# Normalizamos los coeficientes para que los pesos sumen uno
ols_weights_norm <- coef(ols_fit) / sum(coef(ols_fit))

# Construimos tabla de pesos OLS por ciudad de control 
ols_weights_df <- data.frame(
  ciudad         = gsub("city_", "Ciudad ", names(ols_weights_norm)),
  peso_regresion = as.numeric(ols_weights_norm)
) |>
  arrange(ciudad)

# Guardamos la tabla de pesos OLS como insumo para la tabla comparativa
write.csv(
  ols_weights_df,
  file.path(resultados, "tabla_pesos_regresion.csv"),
  row.names = FALSE
)

# Construimos contrafactual postratamiento de lrev usando pesos OLS
Y_post_controls  <- Y_post_mat[, control_col_idx, drop = FALSE]
Y_synth_post_ols <- as.numeric(Y_post_controls %*% ols_weights_norm)
Y_treated_post   <- Y_post_mat[, treated_col_idx]

df_synth_ols <- data.frame(
  date_m           = wide_post$date_m,
  lrevenue_treated = Y_treated_post,
  lrevenue_synth   = Y_synth_post_ols
)

#### 7. Tabla combinada de pesos: Abadie (2005) vs OLS
# Reorganizamos pesos de Abadie para compararlos con los de regresión OLS
pesos_abadie_df <- synth.tables$tab.w |>
  mutate(ciudad = paste0("Ciudad ", unit.names)) |>
  select(ciudad, peso_abadie = w.weights) |>
  arrange(ciudad)

# Unimos pesos de ambas metodologías en una sola tabla [Tabla 2 del TALLER]
tabla_pesos_completa <- dplyr::full_join(
  pesos_abadie_df,
  ols_weights_df,
  by = "ciudad"
) |>
  mutate(ciudad = as.character(ciudad)) |>
  arrange(
    if_else(
      ciudad == paste0("Ciudad ", treat_city),
      Inf,
      as.numeric(gsub("Ciudad ", "", ciudad))
    )
  )

# Guardamos la tabla comparativa de pesos para análisis de robustez
write.csv(
  tabla_pesos_completa,
  file.path(resultados, "tabla_pesos_abadie_vs_ols.csv"),
  row.names = FALSE
)

#### 8. Control sintético para accidents_std (Ciudad 8)
# Definimos meses pretratamiento para accidentes según fecha de inicio
time_pre_acc <- df |>
  filter(date_m < treat_start_date) |>
  distinct(month_number) |>
  arrange(month_number) |>
  pull()

# Preparamos datos para Synth usando accidents_std como dependiente
dataprep.acc <- dataprep(
  foo                  = as.data.frame(df),
  predictors           = c(
    "drivers_active",
    "avg_income_per_hour",
    "total_trips",
    "accidents_std"
  ),
  predictors.op        = "mean",
  dependent            = "accidents_std",
  unit.variable        = "city_id",
  time.variable        = "month_number",
  treatment.identifier = treat_city,
  controls.identifier  = control_units,
  time.predictors.prior = time_pre_acc,
  time.optimize.ssr     = time_pre_acc,
  time.plot             = sort(unique(df$month_number)),
  special.predictors    = list(
    list("drivers_active",      72, "mean"),
    list("avg_income_per_hour", 72, "mean"),
    list("total_trips",         72, "mean"),
    list("accidents_std",       72, "mean")
  )
)

# Estimamos control sintético para accidentes y extraemos ajuste óptimo
synth.acc <- synth(dataprep.acc)
synth.acc$rgV.optim  # Se usa en el informe para documentar ajuste

# Guardamos pesos de accidentes para identificar ciudades donantes relevantes
# Synth.tables.acc contiene 
write.csv(
  synth.tables.acc <- synth.tab(
    dataprep.res = dataprep.acc,
    synth.res    = synth.acc
  )$tab.w,
  file.path(resultados, "tabla_pesos_accidentes.csv"),
  row.names = FALSE
)

# Construimos trayectorias observada y sintética de accidentes_std
# Se usa después para graficación y análisis de RMSPE
Y1_acc  <- dataprep.acc$Y1plot
Y0s_acc <- dataprep.acc$Y0plot %*% synth.acc$solution.w

df_synth_acc <- data.frame(
  month_number    = dataprep.acc$tag$time.plot,
  date_m          = date_lookup$date_m[
    match(dataprep.acc$tag$time.plot, date_lookup$month_number)
  ],
  accidents_obs   = as.numeric(Y1_acc),
  accidents_synth = as.numeric(Y0s_acc)
)

#### 9. Pesos de Regresión (OLS) para accidents_std (Ciudad 8)
# Construimos formato wide para accidents_std con una columna por ciudad
wide_acc <- df |>
  select(month_number, date_m, city_id, accidents_std) |>
  arrange(month_number, city_id) |>
  pivot_wider(
    names_from  = city_id,
    values_from = accidents_std,
    names_prefix = "city_"
  ) |>
  arrange(month_number)

# Identificamos tratada y controles en la matriz de accidentes
city_cols_acc        <- grep("^city_", names(wide_acc), value = TRUE)
treated_col_acc_name <- paste0("city_", treat_city)
control_cols_acc     <- setdiff(city_cols_acc, treated_col_acc_name)

# Separamos periodo pre y posttratamiento para accidentes_std
wide_pre_acc  <- wide_acc |> filter(month_number < treat_start_num)
wide_post_acc <- wide_acc |> filter(month_number >= treat_start_num)

# Matrices pre y post de accidentes por ciudad
Y_pre_acc  <- as.matrix(wide_pre_acc[, city_cols_acc])
Y_post_acc <- as.matrix(wide_post_acc[, city_cols_acc])

treated_idx_acc <- which(colnames(Y_pre_acc) == treated_col_acc_name)
control_idx_acc <- which(colnames(Y_pre_acc) != treated_col_acc_name)

# Estimamos modelo OLS sin intercepto para accidents_std
fmla_acc <- reformulate(control_cols_acc, response = treated_col_acc_name)
ols_acc  <- lm(update(fmla_acc, . ~ . - 1), data = wide_pre_acc)

# Normalizamos los pesos OLS para que sumen uno
ols_acc_weights_norm <- coef(ols_acc) / sum(coef(ols_acc))

# Construimos contrafactual posttratamiento de accidentes_std con OLS
Y_post_controls_acc   <- Y_post_acc[, control_idx_acc, drop = FALSE]
Y_synth_post_acc_ols  <- as.numeric(
  Y_post_controls_acc %*% ols_acc_weights_norm
)
Y_treated_post_acc    <- Y_post_acc[, treated_idx_acc]

df_synth_acc_ols <- data.frame(
  date_m          = wide_post_acc$date_m,
  accidents_obs   = Y_treated_post_acc,
  accidents_synth = Y_synth_post_acc_ols
)

#### 10. RMSPE pretratamiento: Abadie vs OLS (accidents_std)
# Calculamos RMSPE para el control sintético de Abadie en el periodo pre
Y1_pre_acc  <- dataprep.acc$Y1plot[
  dataprep.acc$tag$time.plot %in% time_pre_acc
]
Y0s_pre_acc <- (dataprep.acc$Y0plot %*% synth.acc$solution.w)[
  dataprep.acc$tag$time.plot %in% time_pre_acc
]

rmspe_abadie_acc <- sqrt(mean((Y1_pre_acc - Y0s_pre_acc)^2))

# Calculamos RMSPE para el contrafactual OLS en el periodo pre
Y_pre_controls_acc   <- Y_pre_acc[, control_idx_acc, drop = FALSE]
Y_synth_pre_acc_ols  <- as.numeric(
  Y_pre_controls_acc %*% ols_acc_weights_norm
)
Y_treated_pre_acc    <- Y_pre_acc[, treated_idx_acc]

rmspe_ols_acc <- sqrt(mean((Y_treated_pre_acc - Y_synth_pre_acc_ols)^2))

# Construimos tabla comparativa de RMSPE para evaluar ajuste pretratamiento
rmspe_tab_acc <- data.frame(
  Modelo = c(
    "Control sintético (Synth)",
    "Pesos por regresión (OLS)"
  ),
  RMSPE = c(rmspe_abadie_acc, rmspe_ols_acc)
)

# Guardamos la tabla de RMSPE para usarla en la interpretación del informe
write.csv(
  rmspe_tab_acc,
  file.path(resultados, "tabla_rmspe_accidentes.csv"),
  row.names = FALSE
)

print(rmspe_tab_acc)

#### 11. Placebos espaciales para log(revenue) (Control Sintético de Abadie) ----
# Generamos placebos espaciales usando la misma especificación de Abadie (dataprep.out, synth.out)

placebos_lrev <- generate.placebos(
  dataprep.out = dataprep.out,
  synth.out    = synth.out,
  Sigf.ipop    = 5,
  strategy     = "sequential"   # sin paralelizar, más fácil de reproducir
)

# Extraemos la información necesaria
df_plac_lrev <- placebos_lrev$df
n_lrev       <- placebos_lrev$n         # número de unidades placebo (donantes)
t1_lrev      <- placebos_lrev$t1        # primer periodo post-tratamiento (en month_number)

# Nos quedamos solo con los periodos post-tratamiento
post_lrev <- df_plac_lrev[df_plac_lrev$year >= t1_lrev, ]

# Brecha de la unidad tratada: Y1 - synthetic.Y1
gap_treated_lrev <- post_lrev$Y1 - post_lrev$synthetic.Y1

# Brechas de los placebos: (Y_control - Y_sintético_control) para cada ciudad donante
# En generate.placebos, las primeras n columnas son los sintéticos y las siguientes n son los Y0
# Entonces: gap_i = Y0_i - synthetic_i = df[, n+i] - df[, i]
gaps_placebos_lrev <- as.matrix(post_lrev[ , (n_lrev + 1):(2 * n_lrev)]) -
  as.matrix(post_lrev[ , 1:n_lrev])

# p-valor_t = proporción de placebos con |gap_{t,j}| >= |gap_{t,tratada}|
pvals_lrev <- sapply(seq_along(gap_treated_lrev), function(k) {
  mean(abs(gaps_placebos_lrev[k, ]) >= abs(gap_treated_lrev[k]))
})

# Armamos tabla con fechas y p-valores por mes post-tratamiento
tabla_pvals_lrev <- data.frame(
  month_number = post_lrev$year,
  date_m       = date_lookup$date_m[match(post_lrev$year, date_lookup$month_number)],
  p_value      = pvals_lrev
)

# Guardamos tabla para usar en el informe / LaTeX
write.csv(
  tabla_pvals_lrev,
  file.path(resultados, "tabla_pval_placebos_lrev.csv"),
  row.names = FALSE
)

# Gráfica de p-valores por mes (log(revenue))
x11()
p_pvals_lrev <- ggplot(tabla_pvals_lrev, aes(x = date_m, y = p_value)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  labs(
    title    = "Placebos espaciales: p-valores por mes (log(revenue))",
    subtitle = "Control sintético de Abadie; línea discontinua: nivel de significancia 5%",
    y        = "p-valor",
    x        = NULL
  ) +
  theme_base
print(p_pvals_lrev)

#### 12. Placebos espaciales para accidents_std (Control Sintético de Abadie) ----
# Placebos espaciales con la especificación de Abadie para accidentes

placebos_acc <- generate.placebos(
  dataprep.out = dataprep.acc,
  synth.out    = synth.acc,
  Sigf.ipop    = 5,
  strategy     = "sequential"
)

df_plac_acc <- placebos_acc$df
n_acc       <- placebos_acc$n
t1_acc      <- placebos_acc$t1

# Periodos post-tratamiento
post_acc <- df_plac_acc[df_plac_acc$year >= t1_acc, ]

# Brecha de la unidad tratada en accidentes: Y1 - synthetic.Y1
gap_treated_acc <- post_acc$Y1 - post_acc$synthetic.Y1

# Brechas de los placebos en accidentes
gaps_placebos_acc <- as.matrix(post_acc[ , (n_acc + 1):(2 * n_acc)]) -
  as.matrix(post_acc[ , 1:n_acc])

# p-valor_t para accidentes
pvals_acc <- sapply(seq_along(gap_treated_acc), function(k) {
  mean(abs(gaps_placebos_acc[k, ]) >= abs(gap_treated_acc[k]))
})

# Tabla con fechas y p-valores por mes
tabla_pvals_acc <- data.frame(
  month_number = post_acc$year,
  date_m       = date_lookup$date_m[match(post_acc$year, date_lookup$month_number)],
  p_value      = pvals_acc
)

# Guardamos tabla para el informe
write.csv(
  tabla_pvals_acc,
  file.path(resultados, "tabla_pval_placebos_accidentes.csv"),
  row.names = FALSE
)

# Gráfica de p-valores por mes (accidents_std)
x11()
p_pvals_acc <- ggplot(tabla_pvals_acc, aes(x = date_m, y = p_value)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  labs(
    title    = "Placebos espaciales: p-valores por mes (accidentes estandarizados)",
    subtitle = "Control sintético de Abadie; línea discontinua: nivel de significancia 5%",
    y        = "p-valor",
    x        = NULL
  ) +
  theme_base
print(p_pvals_acc)
########################################################
#### SECCIÓN DE GRÁFICAS
########################################################

# 11.1. Revenue por ciudad en niveles, destacando Ciudad 8 y tratamiento
p1 <- ggplot() +
  geom_line(
    data = df |> filter(!treated_city),
    aes(x = date_m, y = revenue / 1e6, group = city_id),
    color = "grey75", linewidth = 0.4, alpha = 0.9
  ) +
  geom_line(
    data = df |> filter(treated_city),
    aes(x = date_m, y = revenue / 1e6),
    color = "#1f78b4", linewidth = 1.1
  ) +
  geom_vline(
    xintercept = treat_start_date,
    linetype   = "dashed",
    linewidth  = 0.6
  ) +
  labs(
    title    = "Revenue por ciudad (niveles)",
    subtitle = "Ciudad 8 destacada; línea discontinua = inicio tratamiento",
    y        = "Millones",
    caption  = "Fuente: CS_data.dta. Elaboración propia."
  ) +
  scale_y_continuous(
    labels = label_number(scale = 1, suffix = " M", accuracy = 0.1)
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme_base

ggsave(
  file.path(resultados, "fig_1_revenue_niveles.png"),
  p1, width = 9, height = 4.8, dpi = 300
)

# 11.2. Revenue por ciudad en logaritmos, destacando Ciudad 8
p2 <- ggplot() +
  geom_line(
    data = df |> filter(!treated_city),
    aes(x = date_m, y = lrev, group = city_id),
    color = "grey75", linewidth = 0.4, alpha = 0.9
  ) +
  geom_line(
    data = df |> filter(treated_city),
    aes(x = date_m, y = lrev),
    color = "#33a02c", linewidth = 1.1
  ) +
  geom_vline(xintercept = treat_start_date, linetype = "dashed") +
  labs(
    title    = "Revenue por ciudad (log)",
    subtitle = "Ciudad 8 destacada",
    y        = "log(Revenue)"
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme_base

ggsave(
  file.path(resultados, "fig_2_revenue_log.png"),
  p2, width = 9, height = 4.8, dpi = 300
)

# 11.3. Accidentes normalizados por ciudad, destacando Ciudad 8
p3 <- ggplot() +
  geom_line(
    data = df |> filter(!treated_city),
    aes(x = date_m, y = accidents_std, group = city_id),
    color = "grey75", linewidth = 0.4, alpha = 0.9
  ) +
  geom_line(
    data = df |> filter(treated_city),
    aes(x = date_m, y = accidents_std),
    color = "#e31a1c", linewidth = 1.1
  ) +
  geom_vline(xintercept = treat_start_date, linetype = "dashed") +
  labs(
    title    = "Accidentes normalizados por ciudad",
    subtitle = "Ciudad 8 destacada",
    y        = "Desviaciones estándar",
    caption  = "Fuente: CS_data.dta."
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme_base

ggsave(
  file.path(resultados, "fig_3_accidentes_std.png"),
  p3, width = 9, height = 4.8, dpi = 300
)

# 11.4. Control sintético (Abadie) para revenue en todo el periodo
p_synth_abadie <- ggplot(df_synth_abadie, aes(x = date_m)) +
  geom_line(aes(y = lrevenue_treated), color = "#1f78b4", linewidth = 1) +
  geom_line(
    aes(y = lrevenue_synth),
    color = "grey30",
    linetype = "dashed"
  ) +
  geom_vline(xintercept = treat_start_date, linetype = "dashed") +
  labs(
    title    = "Control sintético (Abadie)",
    subtitle = "Revenue en log: observado vs sintético",
    y        = "log(Revenue)",
    x        = "",
    caption  = "Fuente: CS_data.dta."
  ) +
  theme_base

ggsave(
  file.path(resultados, "fig_4_synth_abadie.png"),
  p_synth_abadie, width = 9, height = 4.8, dpi = 300
)

# 11.5. Control sintético vía OLS para revenue
p_synth_ols <- ggplot(df_synth_ols, aes(x = date_m)) +
  geom_line(aes(y = lrevenue_treated), color = "#1f78b4") +
  geom_line(
    aes(y = lrevenue_synth),
    color = "grey40",
    linetype = "dashed"
  ) +
  geom_text(
    data  = df_synth_ols |> dplyr::slice_tail(n = 1),
    aes(y = lrevenue_treated, label = "Ciudad 8"),
    color = "#1f78b4", hjust = -0.1
  ) +
  geom_text(
    data  = df_synth_ols |> dplyr::slice_tail(n = 1),
    aes(y = lrevenue_synth, label = "Sintético (OLS)"),
    color = "grey40", hjust = -0.1
  ) +
  geom_vline(xintercept = treat_start_date, linetype = "dashed") +
  labs(
    title    = "Control sintético vía OLS",
    subtitle = "Revenue en log",
    y        = "log(Revenue)",
    x        = "",
    caption  = "Fuente: CS_data.dta."
  ) +
  coord_cartesian(clip = "off") +
  theme_base +
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5))

ggsave(
  file.path(resultados, "fig_5_synth_ols.png"),
  p_synth_ols, width = 9, height = 4.8, dpi = 300
)

# 11.6. Control sintético (Abadie) solo periodo postratamiento
df_synth_abadie_post <- df_synth_abadie |>
  filter(month_number >= treat_start_num)

p_synth_abadie_post <- ggplot(df_synth_abadie_post, aes(x = date_m)) +
  geom_line(aes(y = lrevenue_treated), color = "#1f78b4") +
  geom_line(
    aes(y = lrevenue_synth),
    color = "grey30",
    linetype = "dashed"
  ) +
  geom_text(
    data  = df_synth_abadie_post |> dplyr::slice_tail(n = 1),
    aes(y = lrevenue_treated, label = "Ciudad 8"),
    color = "#1f78b4", hjust = -0.1
  ) +
  geom_text(
    data  = df_synth_abadie_post |> dplyr::slice_tail(n = 1),
    aes(y = lrevenue_synth, label = "Sintético (Abadie)"),
    color = "grey30", hjust = -0.1
  ) +
  geom_vline(xintercept = treat_start_date, linetype = "dashed") +
  labs(
    title    = "Control sintético (Abadie) – Postratamiento",
    subtitle = "Revenue en log",
    y        = "log(Revenue)",
    x        = ""
  ) +
  coord_cartesian(clip = "off") +
  theme_base

ggsave(
  file.path(resultados, "fig_4_synth_abadie_post.png"),
  p_synth_abadie_post, width = 9, height = 4.8, dpi = 300
)

# 11.7. Control sintético de accidentes (observado vs sintético)
p_acc <- ggplot(df_synth_acc, aes(x = date_m)) +
  geom_line(aes(y = accidents_obs), color = "#e31a1c", linewidth = 1.1) +
  geom_line(
    aes(y = accidents_synth),
    color = "grey25",
    linetype = "dashed"
  ) +
  geom_vline(xintercept = treat_start_date, linetype = "dashed") +
  labs(
    title    = "Control sintético de accidentes (Ciudad 8)",
    subtitle = "Serie observada vs sintética",
    y        = "Desv. estándar",
    x        = "",
    caption  = "Fuente: CS_data.dta."
  ) +
  theme_base

ggsave(
  file.path(resultados, "fig_6_synth_accidentes.png"),
  p_acc, width = 9, height = 4.8, dpi = 300
)

# Panel 2x1: control sintético para revenue y accidentes
panel_synth <- p_synth_abadie + p_acc +
  plot_layout(ncol = 2) +
  plot_annotation(
    title   = "Efectos estimados vía Control Sintético (Ciudad 8)",
    caption = "Línea continua = observada; discontinua = sintético"
  )

ggsave(
  file.path(resultados, "fig_7_panel_synth_revenue_accidentes.png"),
  panel_synth, width = 11, height = 4.5, dpi = 300
)

# Mostramos las figuras en el visor para revisión rápida de resultados
print(p1); print(p2); print(p3)
print(p_synth_abadie); print(p_synth_ols); print(p_synth_abadie_post)
print(p_acc); print(panel_synth)


#### 9.Z. Gráficas de placebos en panel 1x2 ----

# Placebos para log(revenue)
p_placebos_lrev <- plot_placebos(
  tdf             = placebos_lrev,
  discard.extreme = FALSE,
  mspe.limit      = 5
) +
  theme_base +
  labs(
    title    = "Placebos espaciales: log(revenue)",
    subtitle = "Brechas tratada vs. placebos",
    x        = NULL,
    y        = "Brecha log(revenue)"
  )

# Placebos para accidentes_std
p_placebos_acc <- plot_placebos(
  tdf             = placebos_acc,
  discard.extreme = TRUE,
  mspe.limit      = 5,
) +
  theme_base +
  labs(
    title    = "Placebos espaciales: accidentes",
    subtitle = "Brechas tratada vs. placebos",
    x        = NULL,
    y        = "Brecha accidentes_std"
  )

# Panel 1x2 con patchwork
fig_placebos_panel <- (p_placebos_lrev + p_placebos_acc) + plot_layout(nrow = 1)

# Mostrar en dispositivo gráfico
x11()
print(fig_placebos_panel)

# Guardar en carpeta 'Resultados'
ggplot2::ggsave(
  filename = file.path(resultados, "fig_placebos_panel.png"),
  plot     = fig_placebos_panel,
  width    = 10,
  height   = 4,
  dpi      = 1000
)
