#### Ejercicio 2
# Proyecto:
# Autores: Mahicol Ramírez - Simón Briceño
# Fecha: 21 de noviembre de 2025
# Descripción general:
# 1) Graficar revenue por ciudad, destacando Ciudad 8 y línea vertical en 2024-04.
# 2) Graficar log(revenue) con el mismo formato y discutir ventajas de logs.
# 3) Graficar accidents_std por ciudad, destacando Ciudad 8 y línea vertical en 2024-04.
#
#### 1. Cargar librerías
library(Synth)        # Métodos de control sintético
library(haven)        # Lectura de .dta (Stata)
library(dplyr)        # Manipulación de datos (pipes, summarise, etc.)
library(sandwich)     # Matrices de var-cov robustas (HC)
library(lmtest)       # coeftest() con vcov robusta
library(knitr)        # kable() para LaTeX
library(kableExtra)   # Estética LaTeX (booktabs/striped)
library(ggplot2)      # Gráficas elegantes
library(lubridate)    # Manejo de fechas (ym)
library(scales)       # Formatos (labels, scales)
library(rstudioapi)   # WD relativo al script
library(tidyr)        # Para pivot_wide
library(patchwork)    # Para paneles de ggplot

##### 2. Directorio y datos
# Replicabilidad: fijar WD al directorio del script (RStudio).
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
root       <- dirname(rstudioapi::getActiveDocumentContext()$path)
datos      <- file.path(root, "datos")
resultados <- file.path(root, "Resultados")
if (!dir.exists(resultados)) dir.create(resultados, recursive = TRUE)

# Importar base principal.
df <- read_dta(file.path(datos, "CS_data.dta"))

#### Conversión correcta de Stata %tm -> Date (primer día del mes)
tm_to_date <- function(tm) {
  y <- 1960 + floor(tm / 12)
  m <- (tm %% 12) + 1
  as.Date(sprintf("%04d-%02d-01", y, m))
}

# Aplicación
df <- df |>
  mutate(
    date_m  = tm_to_date(as.integer(date)),  # date viene como meses desde 1960-01
    city_id = as.integer(city_id)
  )

# Verificación rápida
range(df$date)         # esperado: 696 777
range(df$date_m)       # esperado: "2018-01-01" "2024-10-01"
head(df |> arrange(date) |> select(city_id, date, date_m), 5)
head(df |> arrange(desc(date)) |> select(city_id, date, date_m), 5)

# Variables auxiliares
treat_city      <- 8L
treat_start     <- as.Date("2024-04-01")  # abril 2024
df <- df |>
  mutate(
    city_id        = as.integer(city_id),
    treated_city   = city_id == treat_city,
    revenue_pos    = ifelse(revenue <= 0 | is.na(revenue), NA_real_, revenue),
    lrevenue       = log(revenue_pos)
  )

#### 3. Tema base para gráficas
theme_base <- theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold"),
    plot.subtitle   = element_text(color = "grey30"),
    plot.caption    = element_text(color = "grey40", size = 9),
    axis.title.x    = element_blank(),
    axis.text.x     = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.minor= element_blank(),
    legend.position = "none"
  )

#### 4. Gráfica 1: Revenue en niveles por ciudad (resaltamos Ciudad 8, valores en millones)
p1 <- ggplot() +
  geom_line(
    data = df |> filter(city_id != treat_city),
    aes(x = date_m, y = revenue / 1e6, group = city_id),
    color = "grey75", linewidth = 0.4, alpha = 0.9
  ) +
  geom_line(
    data = df |> filter(city_id == treat_city),
    aes(x = date_m, y = revenue / 1e6),
    color = "#1f78b4", linewidth = 1.1
  ) +
  geom_vline(xintercept = treat_start, linetype = "dashed", linewidth = 0.6) +
  labs(
    title    = "Ganancias mensuales por ciudad (niveles, en millones)",
    subtitle = "Ciudad 8 destacada; línea discontinua en abril 2024 (inicio del tratamiento)",
    y        = "Revenue (millones de unidades monetarias)",
    caption  = "Fuente: CS_data.dta. Elaboración propia."
  ) +
  scale_y_continuous(labels = label_number(scale = 1, suffix = " M", accuracy = 0.1)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme_base
ggsave(filename = file.path(resultados, "fig_1_revenue_niveles.png"),
       plot = p1, width = 9, height = 4.8, dpi = 300)

#### 5. Gráfica 2: log(revenue) por ciudad (resaltar Ciudad 8)
p2 <- ggplot() +
  geom_line(
    data = df |> filter(city_id != treat_city),
    aes(x = date_m, y = lrevenue, group = city_id),
    color = "grey75", linewidth = 0.4, alpha = 0.9
  ) +
  geom_line(
    data = df |> filter(city_id == treat_city),
    aes(x = date_m, y = lrevenue),
    color = "#33a02c", linewidth = 1.1
  ) +
  geom_vline(xintercept = treat_start, linetype = "dashed", linewidth = 0.6) +
  labs(
    title    = "Ganancias mensuales por ciudad (logaritmos)",
    subtitle = "Ciudad 8 destacada; línea discontinua en abril 2024 (inicio del tratamiento)",
    y        = "log(Revenue)",
    caption  = "Nota: log aplicado sólo si revenue>0. Fuente: CS_data.dta. Elaboración propia."
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme_base
ggsave(filename = file.path(resultados, "fig_2_revenue_log.png"),
       plot = p2, width = 9, height = 4.8, dpi = 300)

#### 6. Gráfica 3: Accidentes per cápita (normalizados) por ciudad
p3 <- ggplot() +
  geom_line(
    data = df |> filter(city_id != treat_city),
    aes(x = date_m, y = accidents_std, group = city_id),
    color = "grey75", linewidth = 0.4, alpha = 0.9
  ) +
  geom_line(
    data = df |> filter(city_id == treat_city),
    aes(x = date_m, y = accidents_std),
    color = "#e31a1c", linewidth = 1.1
  ) +
  geom_vline(xintercept = treat_start, linetype = "dashed", linewidth = 0.6) +
  labs(
    title    = "Accidentes per cápita (normalizados) por ciudad",
    subtitle = "Ciudad 8 destacada; línea discontinua en abril 2024 (inicio del tratamiento)",
    y        = "Accidentes (desviaciones estándar)",
    caption  = "Valores estandarizados respecto a la media muestral. Fuente: CS_data.dta."
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme_base
ggsave(filename = file.path(resultados, "fig_3_accidentes_std.png"),
       plot = p3, width = 9, height = 4.8, dpi = 300)

# Mostrar en visor interactivo (opcional en sesión)
print(p1); print(p2); print(p3)

colnames(df)

#### 7. Control sintético para la Ciudad 8 (método de Abadie, 2005)
# ---------------------------------------------------------------

#### 8. Parámetros básicos del tratamiento ----
treat_city  <- 8L
treat_start <- as.Date("2024-04-01")

# month_number correspondiente al inicio del tratamiento
treat_time <- df |>
  filter(date_m == treat_start) |>
  distinct(month_number) |>
  pull()

# month_number correspondiente a enero de 2024 (3 meses antes del tratamiento)
pred_month_num <- df |>
  filter(date_m == as.Date("2024-01-01")) |>
  distinct(month_number) |>
  pull()

# Vector de tiempos pretratamiento
time_pre <- df |>
  filter(date_m < treat_start) |>
  distinct(month_number) |>
  arrange(month_number) |>
  pull()

# Unidades de control
control_units <- setdiff(sort(unique(df$city_id)), treat_city)

#### 9. Control Sintético con Synth (Abadie, 2005) ----
# Variable dependiente: lrevenue
# Predictores (enero 2024): drivers_active, avg_income_per_hour, total_trips, accidents_std
# + log(revenue) en enero 2024

dataprep.out <- dataprep(
  foo = as.data.frame(df),
  predictors = c("drivers_active", "avg_income_per_hour",
                 "total_trips", "accidents_std", "lrevenue"),
  predictors.op = "mean",
  dependent = "lrevenue",
  unit.variable = "city_id",
  time.variable = "month_number",
  treatment.identifier = treat_city, # Ciudad 8
  controls.identifier = control_units, # Otras ciudades
  # covariables definidas solo en enero de 2024
  time.predictors.prior = time_pre, # Periodos pre tratamiento 0-74
  # ventana pretratamiento para ajustar el SSR
  time.optimize.ssr = pred_month_num, # Periodo enero 2024 = 72
  # serie completa para graficar
  time.plot = sort(unique(df$month_number)),
  special.predictors = list(
    list("lrevenue", pred_month_num, "mean")  # log(revenue) en 2024-01
  )
)
#  16.35 / drivers active 306.01

# Estimar el control sintético
synth.out <- synth(dataprep.out)
# Tablas de síntesis: balance de predictores y pesos
synth.tables <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)

# 9.1. Tabla de balance de predictores (para llenar la tabla de Abadie, 2005) ----
balance_table <- synth.tables$tab.pred
balance_table

# Exportar para inspección / copiar a LaTeX
write.csv(balance_table,
          file.path(resultados, "tabla_balance_synth.csv"),
          row.names = FALSE)

# 9.2. Pesos del control sintético (Abadie, 2005) ----
weights_synth <- synth.tables$tab.w
weights_synth

# Exportar pesos de Abadie
write.csv(weights_synth,
          file.path(resultados, "tabla_pesos_synth.csv"),
          row.names = FALSE)

# 9.3. Trayectorias tratada vs sintética (en log) ----
Y1  <- dataprep.out$Y1plot     # ciudad tratada
Y0s <- dataprep.out$Y0plot %*% synth.out$solution.w  # combinación sintética

time_plot <- dataprep.out$tag$time.plot

df_synth_abadie <- data.frame(
  month_number = time_plot,
  date_m       = df |> distinct(month_number, date_m) |>
    arrange(month_number) |> filter(month_number %in% time_plot) |> pull(date_m),
  lrevenue_treated = as.numeric(Y1),
  lrevenue_synth   = as.numeric(Y0s)
)

p_synth_abadie <- ggplot(df_synth_abadie, aes(x = date_m)) +
  geom_line(aes(y = lrevenue_treated),
            color = "#1f78b4", linewidth = 1) +
  geom_line(aes(y = lrevenue_synth),
            color = "grey30", linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = treat_start, linetype = "dashed") +
  labs(
    title    = "Control sintético (Abadie, 2005) para la Ciudad 8",
    subtitle = "Ganancias en logaritmos: observado vs sintético",
    y        = "log(Revenue)",
    x        = "",
    caption  = "Fuente: CS_data.dta. Elaboración propia."
  ) +
  theme_base

ggsave(file.path(resultados, "fig_4_synth_abadie.png"),
       p_synth_abadie, width = 9, height = 4.8, dpi = 300)
print(p_synth_abadie)


#### 10. Método alternativo: Pesos de Regresión (OLS, sin covariables) ----
# Formato wide: cada ciudad = una columna con lrevenue

wide_df <- df |>
  select(month_number, date_m, city_id, lrevenue) |>
  arrange(month_number, city_id) |>
  pivot_wider(
    names_from  = city_id,
    values_from = lrevenue,
    names_prefix = "city_"
  )

# Identificar columnas de ciudades
city_cols <- grep("^city_", names(wide_df), value = TRUE)
treated_col_name <- paste0("city_", treat_city)
control_col_names <- setdiff(city_cols, treated_col_name)

# Separar periodo pre y post tratamiento
wide_pre  <- wide_df |> filter(month_number < treat_time)
wide_post <- wide_df |> filter(month_number >= treat_time)

Y_pre_mat  <- as.matrix(wide_pre[ , city_cols])
Y_post_mat <- as.matrix(wide_post[ , city_cols])

# Índices numéricos de tratada y controles
treated_col_idx  <- which(colnames(Y_pre_mat) == treated_col_name)
control_col_idx  <- which(colnames(Y_pre_mat) %in% control_col_names)

# 10.1. Estimar pesos OLS para la Ciudad 8 ----
df_reg <- data.frame(
  Y_treated = Y_pre_mat[, treated_col_idx],
  Y_pre_mat[, control_col_idx, drop = FALSE]
)

ols_fit <- lm(Y_treated ~ 0 + ., data = df_reg)
ols_weights <- coef(ols_fit)          # pesos sin normalizar
ols_weights_norm <- ols_weights / sum(ols_weights)  # normalizados a que sumen 1

# Ordenar en data.frame legible
ols_weights_df <- data.frame(
  ciudad          = gsub("city_", "Ciudad ", names(ols_weights_norm)),
  peso_regresion  = as.numeric(ols_weights_norm)
) |>
  arrange(ciudad)

# Exportar pesos OLS para llenar tabla \ref{tabla_pesos}
write.csv(ols_weights_df,
          file.path(resultados, "tabla_pesos_regresion.csv"),
          row.names = FALSE)

# 10.2. Construir contrafactual sintético postratamiento (OLS) ----
Y_post_controls <- Y_post_mat[, control_col_idx, drop = FALSE]
Y_synth_post_ols <- as.numeric(Y_post_controls %*% ols_weights_norm)
Y_treated_post    <- Y_post_mat[, treated_col_idx]

df_synth_ols <- data.frame(
  date_m          = wide_post$date_m,
  lrevenue_treated = Y_treated_post,
  lrevenue_synth   = Y_synth_post_ols
)

p_synth_ols <- ggplot(df_synth_ols, aes(x = date_m)) +
  geom_line(aes(y = lrevenue_treated),
            color = "#1f78b4", linewidth = 1) +
  geom_line(aes(y = lrevenue_synth),
            color = "grey40", linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = treat_start, linetype = "dashed") +
  labs(
    title    = "Control sintético vía Pesos de Regresión (OLS)",
    subtitle = "Ganancias en logaritmos: observado vs combinación OLS",
    y        = "log(Revenue)",
    x        = "",
    caption  = "Fuente: CS_data.dta. Elaboración propia."
  ) +
  theme_base

ggsave(file.path(resultados, "fig_5_synth_ols.png"),
       p_synth_ols, width = 9, height = 4.8, dpi = 300)
print(p_synth_ols)


#### 11. Tabla combinada de pesos: Abadie (2005) vs OLS ----
# Pesos de Abadie (2005): vienen en synth.tables$tab.w
# synth.tables$tab.w tiene columnas: unit.number, unit.name, w.weights

pesos_abadie_df <- synth.tables$tab.w |>
  mutate(
    ciudad = paste0("Ciudad ", unit.names)
  ) |>
  select(ciudad, peso_abadie = w.weights) |>
  arrange(ciudad)

# Unir con pesos OLS
tabla_pesos_completa <- full_join(
  pesos_abadie_df,
  ols_weights_df,
  by = "ciudad"
) |>
  arrange(ciudad)

# Ver para copiar a LaTeX
print(tabla_pesos_completa, digits=3)


write.csv(tabla_pesos_completa,
          file.path(resultados, "tabla_pesos_abadie_vs_ols.csv"),
          row.names = FALSE)

#### 12. Control Sintético para Accidentes de Tránsito (Ciudad 8) ----
# Objetivo: estimar el contrafactual de "accidents_std" en la Ciudad 8
# usando los mismos controles y estructura que en el ejercicio de revenue.

# Definir nuevamente parámetros de tratamiento
treat_city  <- 8L
treat_start <- as.Date("2024-04-01")

# Períodos
time_pre <- df |> filter(date_m < treat_start) |> distinct(month_number) |> arrange(month_number) |> pull()
pred_month_num <- df |> filter(date_m == as.Date("2024-01-01")) |> distinct(month_number) |> pull()
control_units <- setdiff(sort(unique(df$city_id)), treat_city)

# Preparar los datos para el paquete Synth
dataprep.acc <- dataprep(
  foo = as.data.frame(df),
  predictors = c("drivers_active", "avg_income_per_hour", "total_trips"),
  predictors.op = "mean",
  dependent = "accidents_std",
  unit.variable = "city_id",
  time.variable = "month_number",
  treatment.identifier = treat_city,
  controls.identifier = control_units,
  time.predictors.prior = time_pre,      # enero 2024
  time.optimize.ssr    = time_pre,              # ventana pretratamiento
  time.plot = sort(unique(df$month_number)),
  special.predictors = list(
    list("accidents_std", pred_month_num, "mean")  # incluir accidentes enero 2024
  )
)

# Ejecutar el control sintético
synth.acc <- synth(dataprep.acc)
synth.acc$rgV.optim

# Tablas de balance y pesos
synth.tables.acc <- synth.tab(dataprep.res = dataprep.acc, synth.res = synth.acc)
synth.tables.acc$tab.pred # Synth 305 value hunting para entender lógica de R
# Mostrar pesos (ciudades de control)
pesos_acc <- synth.tables.acc$tab.w
pesos_acc

# Exportar resultados
write.csv(pesos_acc, file.path(resultados, "tabla_pesos_accidentes.csv"), row.names = FALSE)
write.csv(synth.tables.acc$tab.pred, file.path(resultados, "tabla_balance_accidentes.csv"), row.names = FALSE)


#### 13. Gráfica de Accidentes: Ciudad 8 vs Sintético ----

# Trayectorias observada (tratada) y sintética (contrafactual)
Y1_acc <- dataprep.acc$Y1plot
Y0s_acc <- dataprep.acc$Y0plot %*% synth.acc$solution.w

# Construir data frame para graficar
df_synth_acc <- data.frame(
  month_number = dataprep.acc$tag$time.plot,
  date_m       = df |> distinct(month_number, date_m) |>
    arrange(month_number) |> filter(month_number %in% dataprep.acc$tag$time.plot) |> pull(date_m),
  accidents_obs   = as.numeric(Y1_acc),
  accidents_synth = as.numeric(Y0s_acc)
)

# Gráfica
p_acc <- ggplot(df_synth_acc, aes(x = date_m)) +
  geom_line(aes(y = accidents_obs), color = "#e31a1c", linewidth = 1.1) +
  geom_line(aes(y = accidents_synth), color = "grey25", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = treat_start, linetype = "dashed", linewidth = 0.6) +
  labs(
    title    = "Control sintético de accidentes de tránsito (Ciudad 8)",
    subtitle = "Variable: accidentes normalizados. Línea roja: observada. Línea gris: contrafactual sintético.",
    y        = "Accidentes (desviaciones estándar)",
    x        = "",
    caption  = "Fuente: CS_data.dta. Elaboración propia."
  ) +
  theme_base

ggsave(file.path(resultados, "fig_6_synth_accidentes.png"),
       p_acc, width = 9, height = 4.8, dpi = 300)

print(p_acc)

#### 14. Panel 2x1: efectos sintéticos en ganancias y accidentes ----

panel_synth <- p_synth_abadie + p_acc +
  plot_layout(ncol = 2) +
  plot_annotation(
    title   = "Efectos estimados mediante Control Sintético (Ciudad 8)",
    caption = "Línea continua: serie observada. Línea discontinua: contrafactual sintético."
  )

ggsave(
  filename = file.path(resultados, "fig_7_panel_synth_revenue_accidentes.png"),
  plot     = panel_synth,
  width    = 11,
  height   = 4.5,
  dpi      = 300
)

print(panel_synth)
