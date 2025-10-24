#### Ejercicio 2
# Proyecto: Lumeria 1989 — TV extranjera y apoyo al régimen
# Autores: Mahicol Ramírez - Simón Briceño
# Fecha: 24 de octubre de 2025
# Descripción general:
# 1) Descriptivas útiles para IV y tablas LaTeX
#    Bloque 1: insumos para estadística descriptiva.
# 2) MCO
#    Bloque 2: benchmark potencialmente sesgado.
# 3) IV (2SLS) con z como instrumento de d
#    Bloque 3: estrategia causal principal (LATE).
# 4) Comparación MCO vs IV
#    Bloque 4: contraste de magnitud/signo (sesgo MCO).
# 5) Wald y comentarios analíticos
#    Bloque 5: versión cerrada con Z,D binarios.
# Referencia metodológica: sesión 9 (s9_IV.pdf) sobre IV.

#### 1. Cargar librerías
# Instalación opcional (comentada para no interrumpir).
# install.packages(c("haven","dplyr","AER","sandwich","lmtest",
#                    "modelsummary","knitr","kableExtra"))
library(haven)       # Lectura de .dta (Stata)
library(dplyr)       # Manipulación de datos (pipes, summarise, etc.)
library(AER)         # ivreg() para 2SLS y diagnósticos
library(sandwich)    # Matrices de var-cov robustas (HC)
library(lmtest)      # coeftest() con vcov robusta
library(modelsummary)# Tablas de modelos MCO vs IV
library(knitr)       # kable() para LaTeX
library(kableExtra)  # Estética LaTeX (booktabs/striped)

#### 2. Directorio y datos
# Replicabilidad: fijar WD al directorio del script (RStudio).
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
root <- dirname(rstudioapi::getActiveDocumentContext()$path)
datos <- paste0(root,"\\datos")
resultados <- paste0(root,"\\Resultados")

# Importar base principal.
df <- read_dta(paste0(datos,"\\Lumeria1989.dta"))

# Vector de controles X (edad, mujer, educación, urbano).
ctrls <- c("age","female","educ","urban")

# Variables binarias para descriptivas.
bin_vars  <- c("Y","D","Z","female","urban","valley")

# Variables continuas para dispersión/outliers.
cont_vars <- c("age","educ")

#### 3. Descriptivas: tabla de binarias
# Proporciones y tamaños que informan prevalencia/balance.
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
print(desc_bin, width = Inf)

#### 4. Descriptivas: continuas (medidas y cuantiles)
# Helper de percentiles sin nombres (limpio para tablas).
qfun <- function(x, p) quantile(x, probs = p, na.rm = TRUE, names = FALSE)

# Medias, dispersión y cuantiles para continuas.
desc_cont <- df %>%
  summarise(across(
    all_of(cont_vars),
    list(
      mean       = ~mean(.x, na.rm = TRUE),
      sd         = ~sd(.x, na.rm = TRUE),
      p25        = ~qfun(.x, 0.25),
      p50        = ~qfun(.x, 0.50),
      p75        = ~qfun(.x, 0.75),
      min        = ~min(.x, na.rm = TRUE),
      max        = ~max(.x, na.rm = TRUE),
      iqr        = ~IQR(.x, na.rm = TRUE),
      miss_share = ~mean(is.na(.x))
    ),
    .names = "{.col}_{.fn}"
  ))
print(desc_cont, width = Inf)

#### 5. Por acceso (Z): balance y 1ª etapa intuitiva
# Medias de Y, D, continuas y proporciones de binarias por Z.
by_z_means <- df %>%
  group_by(Z) %>%
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
print(by_z_means, width = Inf)

#### 6. Balance por Z: SMD
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

# Construcción de tabla de balance para X.
balance_tbl <- data.frame(
  var     = c("age","educ","female","urban"),
  mean_z1 = sapply(c("age","educ","female","urban"),
                   \(v) mean(df[[v]][df$Z == 1], na.rm = TRUE)),
  mean_z0 = sapply(c("age","educ","female","urban"),
                   \(v) mean(df[[v]][df$Z == 0], na.rm = TRUE))
)
balance_tbl$diff <- balance_tbl$mean_z1 - balance_tbl$mean_z0
balance_tbl$SMD  <- sapply(c("age","educ","female","urban"),
                           \(v) smd(df[[v]], df$Z))
print(balance_tbl)

#### 7. Momentos IV: E[D|Z] y E[Y|Z]
# Relevancia (1ª etapa) y reduced form (numerador de Wald).
ED_Z <- df %>% group_by(Z) %>%
  summarise(E_D = mean(D, na.rm = TRUE), n = n(), .groups = "drop")
EY_Z <- df %>% group_by(Z) %>%
  summarise(E_Y = mean(Y, na.rm = TRUE), n = n(), .groups = "drop")
print(ED_Z)
print(EY_Z)
cat("ΔE[D|Z] =", diff(ED_Z$E_D),
    " |  ΔE[Y|Z] =", diff(EY_Z$E_Y), "\n")

#### 8. Outliers informativos (regla IQR)
# Diagnóstico simple para errores de captura.
flag_outliers <- function(x) {
  qs <- quantile(x, c(.25, .75), na.rm = TRUE)
  i  <- IQR(x, na.rm = TRUE)
  lo <- qs[1] - 1.5 * i
  hi <- qs[2] + 1.5 * i
  which(x < lo | x > hi)
}
cat("Outliers(age)  =", length(flag_outliers(df$age)),
    " | Outliers(educ) =", length(flag_outliers(df$educ)), "\n")

#### 9. Correlaciones básicas
# Señales y colinealidad entre Y, D, Z y controles.
corr_vars <- c("Y","D","Z","age","educ","female","urban")
corr_mat  <- cor(df[, corr_vars],
                 use = "pairwise.complete.obs",
                 method = "pearson")
print(round(corr_mat, 2))

#### 10. MCO (benchmark)
# Estimación susceptible a sesgo por endogeneidad.
f_ols  <- Y ~ D + age + female + educ + urban
ols_fit <- lm(f_ols, data = df)
summary(ols_fit)

# Var-Cov robusta (heterocedasticidad-consistente).
ols_vcov <- vcovHC(ols_fit, type = "HC1")

# Coeficientes con EE robustos (interpretar β con cautela).
print(coeftest(ols_fit, vcov. = ols_vcov))

#### 11. IV / 2SLS con Z como instrumento de D
# Notación:
#  1ª etapa:  D_i = θ Z_i + W_i'φ + η_i
#  2ª etapa:  Y_i = τ D̂_i + W_i'γ + μ_i
# τ es el efecto causal local (LATE) sobre compliers.

# 11a. Primera etapa: D ~ Z + X
fs_fit  <- lm(D ~ Z + age + female + educ + urban, data = df)
fs_vcov <- vcovHC(fs_fit, type = "HC1")
fs_ct   <- coeftest(fs_fit, vcov. = fs_vcov)
summary(fs_fit)

# Guardar coef y métricas del instrumento Z.
theta_hat <- unname(fs_ct["Z","Estimate"])
theta_se  <- unname(fs_ct["Z","Std. Error"])
theta_t   <- unname(fs_ct["Z","t value"])
theta_p   <- unname(fs_ct["Z","Pr(>|t|)"])

# 11b. Segunda etapa: Y ~ D + X | Z + X
f_iv    <- Y ~ D + age + female + educ + urban |
                 Z + age + female + educ + urban
iv_fit  <- AER::ivreg(f_iv, data = df)
iv_vcov <- vcovHC(iv_fit, type = "HC1")
iv_ct   <- coeftest(iv_fit, vcov. = iv_vcov)

# Resumen con diagnósticos.
summary(iv_fit, diagnostics = TRUE)

# Extraer τ̂_IV (coeficiente de D).
tau_iv    <- unname(iv_ct["D","Estimate"])
tau_iv_se <- unname(iv_ct["D","Std. Error"])
tau_iv_t  <- unname(iv_ct["D","t value"])
tau_iv_p  <- unname(iv_ct["D","Pr(>|t|)"])

# Coeficientes IV con EE robustos; foco en τ (D).
print(coeftest(iv_fit, vcov. = iv_vcov))

# 11c. Primera etapa explícita y F de exclusión
# Reporte estándar para descartar instrumentos débiles.
fs_fit  <- lm(D ~ Z + age + female + educ + urban, data = df)
fs_vcov <- vcovHC(fs_fit, type = "HC1")
print(coeftest(fs_fit, vcov. = fs_vcov))

# Diagnósticos AER: incluye “Weak instruments” (F).
print(summary(iv_fit, vcov. = iv_vcov,
              diagnostics = TRUE)$diagnostics)

#### 12. Comparación MCO vs IV
# Contraste de magnitud y signo: dirección del sesgo MCO.
modelsummary(
  list("MCO" = ols_fit, "IV (2SLS)" = iv_fit),
  vcov     = list(ols_vcov, iv_vcov),
  gof_map  = tribble(
    ~raw,        ~clean,          ~fmt,
    "nobs",      "Observaciones", 0,
    "r.squared", "R^2",           3
  ),
  statistic = "({std.error}){stars}",
  stars     = c('*' = .1, '**' = .05, '***' = .01)
)

#### 13. Estimador de Wald (Z y D binarios)
# Versión cerrada (LATE de compliers), chequeo rápido.
EY1 <- mean(df$Y[df$Z == 1], na.rm = TRUE)
EY0 <- mean(df$Y[df$Z == 0], na.rm = TRUE)
ED1 <- mean(df$D[df$Z == 1], na.rm = TRUE)
ED0 <- mean(df$D[df$Z == 0], na.rm = TRUE)

wald_hat <- (EY1 - EY0) / (ED1 - ED0)
cat("Wald (muestral) =", wald_hat, "\n")
