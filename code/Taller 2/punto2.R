# --------------------------------------------------------------
# Proyecto: Lumeria 1989 — TV extranjera y apoyo al régimen
# Autores: Mahicol Ramírez - Simón Briceño
# Fecha: 24 de octubre de 2025
# Descripción:
# 1) Descriptivas útiles para IV y tablas LaTeX
# Bloque 1: insumos para estadística descriptiva.
# 2) MCO
# Bloque 2: benchmark potencialmente sesgado.
# 3) IV (2SLS) con z como instrumento de d
# Bloque 3: estrategia causal principal (LATE).
# 4) Comparación MCO vs IV
# Bloque 4: contraste de magnitud/signo (sesgo MCO).
# 5) Wald y comentarios analíticos
# Bloque 5: versión cerrada con Z,D binarios.
# Referencia metodológica general sesión 9 (s9_IV.pdf) sobre IV
# --------------------------------------------------------------

# Cargar librerías necesarias para EDA, IV y reporte.
# Instalación (opcional) comentada para no interrumpir.
# install.packages(c(
#   "haven","dplyr","AER","sandwich","lmtest",
#   "modelsummary","knitr","kableExtra"
# ))

# Lectura de .dta (Stata), permite importar Lumeria1989.dta.
library(haven)
# Manipulación de datos (pipes, summarise, group_by, etc.).
library(dplyr)
# Función ivreg() para 2SLS y diagnósticos de instrumentos.
library(AER)
# Matrices de var-cov robustas (HC) para errores robustos.
library(sandwich)
# coeftest() para estimaciones con vcov robusta.
library(lmtest)
# Tablas de modelos comparables (MCO vs IV) compactas.
library(modelsummary)
# kable() para generar tablas LaTeX de salida.
library(knitr)
# Estética LaTeX (booktabs/striped) para tablas descriptivas.
library(kableExtra)

# Replicabilidad: fijar WD al directorio del script (RStudio).
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Inicia el flujo: importar y declarar nombres relevantes.
df <- read_dta("Lumeria1989.dta")

# Vector de controles X para MCO/IV (edad, mujer, educ., urbano).
ctrls <- c("age","female","educ","urban")

# Variables binarias para descriptivas útiles.
bin_vars <- c("Y","D","Z","female","urban","valley")

# Variables continuas para dispersión/outliers.
cont_vars <- c("age","educ")

# 2. Estadísticas descriptivas (útiles para IV) --------------
# Computa proporciones y tamaños que informan prevalencia/balance.
desc_bin <- df %>%
  summarise(across(
    all_of(bin_vars),
    list(
      share = ~mean(.x, na.rm = TRUE),
      n     = ~sum(!is.na(.x)),
      n0    = ~sum(.x == 0, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  ))

# Muestra en consola para revisión rápida.
print(desc_bin, width = Inf)

# Helper para percentiles sin nombres (limpio para tablas).
qfun <- function(x, p) quantile(x, probs = p, na.rm = TRUE, names = FALSE)

# Pipeline para continuas: medias, dispersión y cuantiles.
desc_cont <- df %>%
  summarise(across(
    all_of(cont_vars),
    list(
      mean = ~mean(.x, na.rm = TRUE),
      sd   = ~sd(.x, na.rm = TRUE),
      p25  = ~qfun(.x, 0.25),
      p50  = ~qfun(.x, 0.50),
      p75  = ~qfun(.x, 0.75),
      min  = ~min(.x, na.rm = TRUE),
      max  = ~max(.x, na.rm = TRUE),
      iqr  = ~IQR(.x, na.rm = TRUE),
      miss_share = ~mean(is.na(.x))
    ),
    .names = "{.col}_{.fn}"
  ))

# Imprime la tabla de continuas.
print(desc_cont, width = Inf)

# 2.2. Por acceso (z): balance y 1ª etapa intuitiva ----------
# Medias de Y,D, continuas y proporciones de binarias por Z.
by_z_means <- df %>%
  group_by(Z ) %>%
  summarise(
    across(c(Y, D, all_of(cont_vars)),
           ~mean(.x, na.rm = TRUE),
           .names = "{.col}_mean"),
    across(all_of(bin_vars[bin_vars != "Z"]),
           ~mean(.x, na.rm = TRUE),
           .names = "{.col}_share"),
    n = dplyr::n(),
    .groups = "drop"
  )

# Visualiza diferencias por Z.
print(by_z_means, width = Inf)

# 2.3. Balance por z (SMD) -----------------------------------
# Diferencia estandarizada: métrica de balance (unidad libre).
smd <- function(x, g) {
  m1 <- mean(x[g == 1], na.rm = TRUE)
  m0 <- mean(x[g == 0], na.rm = TRUE)
  s1 <- var(x[g == 1], na.rm = TRUE)
  s0 <- var(x[g == 0], na.rm = TRUE)
  n1 <- sum(g == 1, na.rm = TRUE)
  n0 <- sum(g == 0, na.rm = TRUE)
  sd_pooled <- sqrt(((n1 - 1) * s1 + (n0 - 1) * s0) / (n1 + n0 - 2))
  (m1 - m0) / sd_pooled
}

# Construye tabla de balance para X.
balance_tbl <- data.frame(
  var = c("age","educ","female","urban"),
  mean_z1 = sapply(c("age","educ","female","urban"),
                   \(v) mean(df[[v]][df$Z == 1], na.rm = TRUE)),
  mean_z0 = sapply(c("age","educ","female","urban"),
                   \(v) mean(df[[v]][df$Z == 0], na.rm = TRUE))
)

# Diferencia en medias (escala natural).
balance_tbl$diff <- balance_tbl$mean_z1 - balance_tbl$mean_z0

# SMD por variable.
balance_tbl$SMD <- sapply(c("age","educ","female","urban"),
                          \(v) smd(df[[v]], df$Z))

# Imprime balance; |SMD|<0.1 muy bueno, <0.25 aceptable.
print(balance_tbl)

# 2.4. Momentos clave IV: E[D|Z] y E[Y|Z] --------------------
# Relevancia (1ª etapa) y reduced form (numerador de Wald).
ED_Z <- df %>%
  group_by(Z) %>%
  summarise(E_D = mean(D, na.rm = TRUE), n = n(), .groups = "drop")
EY_Z <- df %>%
  group_by(Z) %>%
  summarise(E_Y = mean(Y, na.rm = TRUE), n = n(), .groups = "drop")

# Vista de ambos valores esperados condicionales.
print(ED_Z)
print(EY_Z)

# Diferencias: relevancia y reduced form para Wald.
cat("ΔE[D|Z] =", diff(ED_Z$E_D),
    " |  ΔE[Y|Z] =", diff(EY_Z$E_Y), "\n")

# 2.5. Outliers informativos (regla IQR) en continuas --------
# Diagnóstico simple para errores de captura.
flag_outliers <- function(x) {
  qs <- quantile(x, c(.25, .75), na.rm = TRUE)
  i <- IQR(x, na.rm = TRUE)
  lo <- qs[1] - 1.5 * i
  hi <- qs[2] + 1.5 * i
  which(x < lo | x > hi)
}

# Reporta cantidad de outliers en age y educ.
cat("Outliers(age)  =", length(flag_outliers(df$age)),
    " | Outliers(educ) =", length(flag_outliers(df$educ)), "\n")

# 2.6. Correlaciones básicas ---------------------------------
# Chequeo de señales y colinealidad simple entre Y,D,Z y X.
corr_vars <- c("Y","D","Z","age","educ","female","urban")
corr_mat  <- cor(df[, corr_vars], use = "pairwise.complete.obs", method = "pearson")

# Redondeo a 3 decimales para lectura.
print(round(corr_mat, 2))

# 3. MCO -----------------------------------------------------
# Estimación benchmark: susceptible a sesgo por endogeneidad.
f_ols <- Y ~ D + age + female + educ + urban
ols_fit <- lm(f_ols, data = df)
summary(ols_fit)
# Var-Cov robusta (heterocedasticidad-consistente).
ols_vcov <- vcovHC(ols_fit, type = "HC1")

# Coeficientes con EE robustos (interpretar β con cautela).
print(coeftest(ols_fit, vcov. = ols_vcov))

## ================== 4. IV / 2SLS con Z como instrumento de D ==================
## Notación:
##  1ª etapa:  D_i = θ Z_i + W_i'φ + η_i
##  2ª etapa:  Y_i = τ D̂_i + W_i'γ + μ_i
## τ es el efecto causal local (LATE) sobre compliers.

# --- Primera etapa (4a): D ~ Z + X --------------------------------------------
fs_fit  <- lm(D ~ Z + age + female + educ + urban, data = df)           # 1ª etapa
fs_vcov <- vcovHC(fs_fit, type = "HC1")                                 # EE robustas
fs_ct   <- coeftest(fs_fit, vcov. = fs_vcov)                            # Coefs robustos
summary(fs_fit)
# Guardar coef y métricas del instrumento Z:
theta_hat <- unname(fs_ct["Z","Estimate"])
theta_se  <- unname(fs_ct["Z","Std. Error"])
theta_t   <- unname(fs_ct["Z","t value"])
theta_p   <- unname(fs_ct["Z","Pr(>|t|)"])

# --- 2SLS (4b): Y ~ D + X | Z + X ---------------------------------------------
f_iv   <- Y ~ D + age + female + educ + urban | Z + age + female + educ + urban
iv_fit <- AER::ivreg(f_iv, data = df)
iv_vcov <- vcovHC(iv_fit, type = "HC1")
iv_ct   <- coeftest(iv_fit, vcov. = iv_vcov)

# Mostrar resumen con diagnósticos
summary(iv_fit , diagnostics = TRUE)

# Extraer τ̂_IV (coeficiente de D):
tau_iv     <- unname(iv_ct["D","Estimate"])
tau_iv_se  <- unname(iv_ct["D","Std. Error"])
tau_iv_t   <- unname(iv_ct["D","t value"])
tau_iv_p   <- unname(iv_ct["D","Pr(>|t|)"])

# Var-Cov robusta para IV (heterocedasticidad-consistente).
iv_vcov <- vcovHC(iv_fit, type = "HC1")

# Coeficientes IV con EE robustos; foco en τ (D).
print(coeftest(iv_fit, vcov. = iv_vcov))

# Primera etapa explícita y F de exclusión -------------------
# Reporte estándar para descartar instrumentos débiles.
fs_fit  <- lm(d ~ z + age + female + educ + urban, data = df)
fs_vcov <- vcovHC(fs_fit, type = "HC1")

# Coef de Z (π1) con EE robustos: magnitud y significancia.
print(coeftest(fs_fit, vcov. = fs_vcov))

# Diagnósticos AER: incluye “Weak instruments” (F).
print(summary(iv_fit, vcov. = iv_vcov, diagnostics = TRUE)$diagnostics)

# 5. Comparación MCO vs IV -----------------------------------
# Contraste de magnitud y signo: dirección del sesgo MCO.
modelsummary(
  list("MCO" = ols_fit, "IV (2SLS)" = iv_fit),
  vcov = list(ols_vcov, iv_vcov),
  gof_map = tribble(
    ~raw,        ~clean,          ~fmt,
    "nobs",      "Observaciones", 0,
    "r.squared", "R^2",           3
  ),
  statistic = "({std.error}){stars}",
  stars = c('*' = .1, '**' = .05, '***' = .01)
)

# 6. Estimador de Wald ---------------------------------------
# Versión cerrada cuando Z y D son binarios (LATE de compliers).
EY1 <- mean(df$y[df$z == 1], na.rm = TRUE)
EY0 <- mean(df$y[df$z == 0], na.rm = TRUE)
ED1 <- mean(df$d[df$z == 1], na.rm = TRUE)
ED0 <- mean(df$d[df$z == 0], na.rm = TRUE)

# Cociente de Wald: LATE si se cumplen los supuestos.
wald_hat <- (EY1 - EY0) / (ED1 - ED0)

# Imprime τ̂_W como chequeo rápido del 2SLS.
cat("Wald (muestral) =", wald_hat, "\n")

# 7. Tabla(s) LaTeX de estadísticas descriptivas -------------
# Reporte formal reproducible para el documento.

# Binarios: proporción global y por Z.
# En IV interesa prevalencia y primera etapa visual.
tab_bin <- df %>%
  summarise(across(
    all_of(bin_vars),
    list(share = ~mean(.x, na.rm = TRUE)),
    .names = "{.col}_{.fn}"
  )) %>%
  tidyr::pivot_longer(
    everything(),
    names_to = c("var", ".value"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  left_join(
    df %>%
      group_by(z) %>%
      summarise(
        across(
          all_of(bin_vars),
          ~mean(.x, na.rm = TRUE),
          .names = "{.col}_z{z}"
        ),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(-z,
                          names_to = "var_z",
                          values_to = "share_z") %>%
      tidyr::separate(var_z, into = c("var", "zflag"), sep = "_z") %>%
      tidyr::pivot_wider(
        names_from = zflag,
        values_from = share_z,
        names_prefix = "share_z"
      ),
    by = "var"
  ) %>%
  mutate(N = nrow(df)) %>%
  select(var, share, share_z0, share_z1, N)

# Continuas: media, sd, p25, p50, p75, min, max y medias por Z.
tab_cont <- df %>%
  summarise(across(
    all_of(cont_vars),
    list(
      mean = ~mean(.x, na.rm = TRUE),
      sd   = ~sd(.x, na.rm = TRUE),
      p25  = ~qfun(.x, 0.25),
      p50  = ~qfun(.x, 0.50),
      p75  = ~qfun(.x, 0.75),
      min  = ~min(.x, na.rm = TRUE),
      max  = ~max(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  tidyr::pivot_longer(
    everything(),
    names_to = c("var", ".value"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  left_join(
    df %>%
      group_by(z) %>%
      summarise(
        across(
          all_of(cont_vars),
          ~mean(.x, na.rm = TRUE),
          .names = "{.col}_z{z}"
        ),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(-z,
                          names_to = "var_z",
                          values_to = "mean_z") %>%
      tidyr::separate(var_z, into = c("var", "zflag"), sep = "_z") %>%
      tidyr::pivot_wider(
        names_from = zflag,
        values_from = mean_z,
        names_prefix = "mean_z"
      ),
    by = "var"
  ) %>%
  mutate(N = nrow(df)) %>%
  select(var, mean, sd, p25, p50, p75, min, max, mean_z0, mean_z1, N)

# Diccionario de nombres → etiquetas en español.
var_labels <- c(
  y = "Apoyo al régimen (y)",
  d = "Ve TV extranjera (d)",
  z = "Acceso señal (z)",
  female = "Mujer (female)",
  urban  = "Urbano (urban)",
  age    = "Edad (age)",
  educ   = "Educación (años)"
)

# Agrega etiqueta de variable a tabla de binarios.
tab_bin$Variable <- var_labels[tab_bin$var]

# Agrega etiqueta de variable a tabla de continuas.
tab_cont$Variable <- var_labels[tab_cont$var]

# Ordena columnas finales (binarios) y redondea a 3 decimales.
tab_bin_out <- tab_bin %>%
  select(Variable, share, share_z0, share_z1, N) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

# Ordena columnas (continuas) y redondea homogéneo.
tab_cont_out <- tab_cont %>%
  select(Variable, mean, sd, p25, p50, p75, min, max, mean_z0, mean_z1, N) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

# Comentario LaTeX para legibilidad del .tex.
cat("\n% --- Tabla LaTeX: Binarios ---\n")

# Produce código LaTeX con booktabs y estilo consistente.
kable(
  tab_bin_out,
  format = "latex",
  booktabs = TRUE,
  linesep = "",
  caption = "Estadísticas descriptivas — variables binarias",
  col.names = c(
    "Variable","Proporción","Proporción (Z=0)","Proporción (Z=1)","N"
  ),
  label = "tab:desc_bin"
) %>%
  kable_styling(latex_options = c("hold_position","striped")) %>%
  print()

# Comentario LaTeX para separar tablas en el .tex.
cat("\n% --- Tabla LaTeX: Continuas ---\n")

# Produce código LaTeX para continuas con estilo homogéneo.
kable(
  tab_cont_out,
  format = "latex",
  booktabs = TRUE,
  linesep = "",
  caption = "Estadísticas descriptivas — variables continuas",
  col.names = c(
    "Variable","Media","Desv. Est.","P25","P50","P75",
    "Mín","Máx","Media (Z=0)","Media (Z=1)","N"
  ),
  label = "tab:desc_cont"
) %>%
  kable_styling(latex_options = c("hold_position","striped")) %>%
  print()
