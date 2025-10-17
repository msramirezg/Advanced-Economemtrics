# ----------------------------------------------------------  # 
# Proyecto: Lumeria 1989 — TV extranjera y apoyo al régimen   # Contexto. 
# Autor: Mahicol Ramírez - Simón Briceño                      # Autores.
# Fecha: 24 de octubre de 2025                                # Fecha.
# Descripción: 
# - 1. Descriptivas útiles para IV y tablas LaTeX             # Bloque 1: insumos para estadística descriptiva.
# - 2. MCO                                                    # Bloque 2: benchmark potencialmente sesgado.
# - 3. IV (2SLS) con z como instrumento de d                  # Bloque 3: estrategia causal principal (LATE).
# - 4. Comparación MCO vs IV                                  # Bloque 4: contraste de magnitud/signo (sesgo MCO).
# - 5. Wald y comentarios analíticos                          # Bloque 5: versión cerrada con Z,D binarios.
# Metodología: sesión 9 (s9_IV.pdf) sobre IV                  # Referencia metodológica general.
# ----------------------------------------------------------

# 0. Paquetes ------------------------------------------------ # Cargar librerías necesarias para EDA, IV y reporte.
# install.packages(c("haven","dplyr","AER","sandwich","lmtest","modelsummary","knitr","kableExtra"))  # Instalación (si faltan); se deja comentada para no interrumpir.
library(haven)        # Lectura de .dta (Stata), permite importar Lumeria1989.dta.
library(dplyr)        # Manipulación de datos (pipes, summarise, group_by, etc.).
library(AER)          # Función ivreg() para 2SLS y diagnósticos de instrumentos.
library(sandwich)     # Matrices de var-cov robustas (HC) para errores robustos.
library(lmtest)       # coeftest() para estimaciones con vcov robusta.
library(modelsummary) # Tablas de modelos comparables (MCO vs IV) de forma compacta.
library(knitr)        # kable() para generar tablas LaTeX de salida.
library(kableExtra)   # Estética LaTeX (booktabs/striped) para tablas descriptivas.

# Fijar WD al directorio del script (RStudio) ---- # Conveniencia: hace que rutas relativas apunten al script.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Cambia el WD al folder del script;

# 1. Cargar datos y definir variables ------------------------# Inicia el flujo: importar y declarar nombres relevantes.
df <- read_dta("Lumeria1989.dta")                             # Lee la base en formato Stata con variables ya estandarizadas.

# - Nombres esperados en la base -----------------------------# Documentamos la expectativa de nombres (no ejecuta lógica).
# id, region, z, d, y, age, female, educ, urban, valley
ctrls     <- c("age","female","educ","urban")                 # Vector de controles X usados en MCO/IV (edad, mujer, educación, urbano).
bin_vars  <- c("Y","D","Z","female","urban", "valley")        # Variables binarias para proporciones útiles (prevalencias).
cont_vars <- c("age","educ")                                  # Variables continuas para dispersión/outliers (no normalidad).

# 2. Estadísticas descriptivas (útiles para IV) --------------#
# 2.1. Global: binarios (proporción) y continuos (dispersión) # Computa proporciones y tamaños que informan prevalencia/balance.
desc_bin <- df %>%                                            # Inicia pipeline sobre df.
  summarise(across(all_of(bin_vars),                          # Aplica a todas las binarias definidas.
                   list(share = ~mean(.x, na.rm=TRUE),        # share = proporción de 1s (media).
                        n     = ~sum(!is.na(.x)),             # n = conteo no-missing, útil para ver tamaño de muestra real.
                        n0    = ~sum(.x==0, na.rm=TRUE)),     # n0 = cantidad de ceros, complemento de n1.
                   .names="{.col}_{.fn}"))                    # Nombra columnas como var_función para facilitar reshape.
print(desc_bin, width = Inf)                                               # Muestra en consola para revisión rápida.

qfun <- function(x,p) quantile(x, probs=p, na.rm=TRUE, names=FALSE) # Helper para percentiles sin nombres (limpio para tablas).
desc_cont <- df %>%                                           # Pipeline para continuas.
  summarise(across(all_of(cont_vars),                         # Aplica a age y educ.
                   list(mean=~mean(.x,na.rm=TRUE),            # Media: tendencia central (útil para comparar por Z).
                        sd=~sd(.x,na.rm=TRUE),                # Desviación estándar: dispersión (heterogeneidad).
                        p25=~qfun(.x,0.25),                   # Percentil 25: límite inferior de rango intercuartílico.
                        p50=~qfun(.x,0.50),                   # Mediana: robusta a outliers, útil para asimetrías.
                        p75=~qfun(.x,0.75),                   # Percentil 75: límite superior de IQR.
                        min=~min(.x,na.rm=TRUE),              # Mínimo observado: detectar valores extremos bajos.
                        max=~max(.x,na.rm=TRUE),              # Máximo observado: detectar valores extremos altos.
                        iqr=~IQR(.x,na.rm=TRUE),              # Rango intercuartílico: dispersión robusta.
                        miss_share=~mean(is.na(.x))),         # Proporción de NA: calidad de datos y tamaño efectivo.
                   .names="{.col}_{.fn}"))                    # Nombres compuestos por claridad.
print(desc_cont, width = Inf)                                              # Imprime la tabla de continuas.

# 2.2. Por acceso (z): prevalencias/medias y tamaño ----------# Compara por Z para ver balance y 1ª etapa intuitiva.
by_z_means <- df %>%                                          # Inicia pipeline por grupos de Z.
  group_by("Z") %>%                                             # Agrupa por el instrumento (cobertura vs sombra).
  summarise(
    across(c("Y","D",all_of(cont_vars)), ~mean(.x, na.rm=TRUE), .names="{.col}_mean"), # Medias de Y,D y continuas por Z.
    across(all_of(bin_vars), ~mean(.x, na.rm=TRUE), .names="{.col}_share"),            # Proporciones de binarias por Z.
    n = dplyr::n(),                                            # Tamaño del grupo (diagnóstico de equilibrio muestral).
    .groups="drop"                                             # Soltar estructura de agrupamiento tras summarise.
  )
print(by_z_means, width = Inf)                                             # Visualiza diferencia por Z.

# 2.3. Balance por z (SMD) ----------------------------------- # Diferencia estandarizada: métrica de balance (unidad libre).
smd <- function(x, g){                                        # Define función para SMD entre z=1 y z=0.
  m1 <- mean(x[g==1], na.rm=TRUE); m0 <- mean(x[g==0], na.rm=TRUE) # Medias por grupo.
  s1 <- var(x[g==1], na.rm=TRUE);  s0 <- var(x[g==0], na.rm=TRUE)  # Varianzas por grupo.
  sd_pooled <- sqrt( ((sum(g==1,na.rm=TRUE)-1)*s1 + (sum(g==0,na.rm=TRUE)-1)*s0) /    # Desvío combinado (pooled).
                       (sum(g %in% c(0,1),na.rm=TRUE)-2) )
  (m1 - m0) / sd_pooled                                       # Retorna SMD: >|0.1| alerta desbalance sustantivo.
}
balance_tbl <- data.frame(                                    # Construye tabla de balance para X.
  var = c("age","educ","female","urban"),                     # Variables a evaluar en balance (controles).
  mean_z1 = sapply(c("age","educ","female","urban"), \(v) mean(df[[v]][df$Z==1], na.rm=TRUE)), # Media en Z=1.
  mean_z0 = sapply(c("age","educ","female","urban"), \(v) mean(df[[v]][df$Z==0], na.rm=TRUE))  # Media en Z=0.
)
balance_tbl$diff <- balance_tbl$mean_z1 - balance_tbl$mean_z0 # Diferencia en medias (escala natural).
balance_tbl$SMD  <- sapply(c("age","educ","female","urban"), \(v) smd(df[[v]], df$Z)) # SMD por variable.
print(balance_tbl)                                            # Imprime balance; |SMD|<0.1 muy bueno, <0.25 aceptable.

# 2.4. Momentos clave IV: E[D|Z] y E[Y|Z] -------------------- # Insumos para relevancia (1ª etapa) y reduced form (numerador Wald).
ED_Z <- df %>% group_by(Z) %>% summarise(E_D = mean(D, na.rm=TRUE), n=n(), .groups="drop") # E[D|Z] y tamaño por Z.
EY_Z <- df %>% group_by(Z) %>% summarise(E_Y = mean(Y, na.rm=TRUE), n=n(), .groups="drop") # E[Y|Z] y tamaño por Z.
print(ED_Z); print(EY_Z)                                     # Vista de ambos valores esperados  condicionales.
cat("ΔE[D|Z] =", diff(ED_Z$E_D), " |  ΔE[Y|Z] =", diff(EY_Z$E_Y), "\n") # Diferencias: relevancia y reduced form para Wald.

# 2.5. Outliers informativos (regla IQR) en continuas -------- # Diagnóstico simple para errores de captura.
flag_outliers <- function(x){                                 # Define detector basado en 1.5*IQR.
  qs <- quantile(x, c(.25,.75), na.rm=TRUE); i <- IQR(x, na.rm=TRUE) # Cuartiles y rango intercuartílico.
  lo <- qs[1] - 1.5*i; hi <- qs[2] + 1.5*i                   # Umbrales inferior y superior.
  which(x < lo | x > hi)                                      # Índices outlier; sólo informativo (no modifica datos).
}
cat("Outliers(age)  =", length(flag_outliers(df$age)),        # Reporta cantidad de outliers en age (posibles errores).
    " | Outliers(educ) =", length(flag_outliers(df$educ)), "\n") # Reporta cantidad en educ.

# 2.6. Correlaciones básicas --------------------------------- # Chequeo de señales y colinealidad simple entre Y,D,Z y X.
corr_vars <- c("Y","D","Z","age","educ","female","urban")     # Conjunto de interés para correlación.
corr_mat  <- cor(df[, corr_vars], use="pairwise.complete.obs")# Matriz de correlaciones con pairwise NA-handling.
print(round(corr_mat, 2))                                     # Redondeo a 3 decimales para lectura.

# 3. MCO ----------------------------------------------------- # Estimación benchmark: susceptible a sesgo por endogeneidad de d.
f_ols   <- Y ~ D + age + female + educ + urban                # Fórmula: Y en función de D y controles X.
ols_fit <- lm(f_ols, data=df)                                 # Estima MCO; útil para comparar con IV.
ols_vcov <- vcovHC(ols_fit, type="HC1")                       # Var-Cov robusta (heterocedasticidad-consistente).
print(coeftest(ols_fit, vcov.=ols_vcov))                      # Muestra coeficientes con EE robustos (interpretar β con cautela).
# - β (coef de d) = cambio medio condicional; no causal si d está correlacionado con omitidas u_i.

# 4. IV / 2SLS con z como instrumento de d ------------------- # Estrategia causal: usa variación inducida por Z (geografía de señal).
# - 1ª etapa: d_i = π0 + π1 z_i + δ'X_i + v_i                 # Verifica relevancia (π1≠0) y reporta F de exclusión.
# - 2ª etapa:  y_i = α + τ d̂_i + γ'X_i + ε_i                 # τ identifica LATE en compliers, bajo independencia/exclusión/monotonicidad.
f_iv   <- y ~ d + age + female + educ + urban | z + age + female + educ + urban # Fórmula IV: Y ~ D+X | Z+X.
iv_fit <- ivreg(f_iv, data=df)                                # Estima 2SLS (ivreg): obtiene τ_IV (coef de D instrumentado).
iv_vcov <- vcovHC(iv_fit, type="HC1")                         # Var-Cov robusta para IV (heterocedasticidad-consistente).
print(coeftest(iv_fit, vcov.=iv_vcov))                        # Coeficientes IV con EE robustos; foco en τ (D).

# - Primera etapa explícita y F de exclusión ----------------- # Reporte estándar para descartar instrumentos débiles.
fs_fit  <- lm(d ~ z + age + female + educ + urban, data=df)   # Modelo de 1ª etapa (D en función de Z y X).
fs_vcov <- vcovHC(fs_fit, type="HC1")                         # Var-Cov robusta para la 1ª etapa.
print(coeftest(fs_fit, vcov.=fs_vcov))                        # Coef de Z (π1) con EE robustos: magnitud y significancia.
print(summary(iv_fit, vcov.=iv_vcov, diagnostics=TRUE)$diagnostics) # Diagnósticos AER: incluye prueba de “Weak instruments” (F).

# 5. Comparación MCO vs IV ----------------------------------- # Contraste de magnitud y signo: revela dirección del sesgo MCO.
modelsummary(
  list("MCO" = ols_fit, "IV (2SLS)" = iv_fit),               # Lista de modelos a comparar.
  vcov = list(ols_vcov, iv_vcov),                             # EE robustos para ambos modelos.
  gof_map = tribble(                                          # Selección de medidas de ajuste a reportar.
    ~raw,        ~clean,          ~fmt,
    "nobs",      "Observaciones", 0,
    "r.squared", "R^2",           3
  ),
  statistic = "({std.error}){stars}",                         # Muestra EE entre paréntesis y estrellas de significancia.
  stars = c('*'=.1,'**'=.05,'***'=.01)                        # Umbrales visuales estándar (informativos, no dogmáticos).
)                                                              # La tabla sintetiza diferencias; si IV cambia magnitud/signo, sugiere sesgo OLS.

# 6. Estimador de Wald --------------------------------------- # Versión cerrada cuando Z y D son binarios (LATE de compliers).
EY1 <- mean(df$y[df$z==1], na.rm=TRUE); EY0 <- mean(df$y[df$z==0], na.rm=TRUE) # Reduced form: diferencia en Y por Z.
ED1 <- mean(df$d[df$z==1], na.rm=TRUE); ED0 <- mean(df$d[df$z==0], na.rm=TRUE) # Primera etapa: diferencia en D por Z.
wald_hat <- (EY1 - EY0) / (ED1 - ED0)                         # Cociente de Wald: LATE si se cumplen los supuestos.
cat("Wald (muestral) =", wald_hat, "\n")                      # Imprime τ̂_W como chequeo rápido del 2SLS.

# 7. Tabla(s) LaTeX de estadísticas descriptivas ------------- # Reporte formal reproducible para el documento.
# - Binarios: proporción global y por Z                        # En IV interesa prevalencia y primera etapa visual.
tab_bin <- df %>%
  summarise(across(all_of(bin_vars),
                   list(share = ~mean(.x, na.rm=TRUE)),
                   .names = "{.col}_{.fn}")) %>%
  tidyr::pivot_longer(everything(),
                      names_to = c("var",".value"),
                      names_pattern = "(.*)_(.*)") %>%
  left_join(
    df %>%
      group_by(z) %>%
      summarise(across(all_of(bin_vars), ~mean(.x, na.rm=TRUE),
                       .names="{.col}_z{z}"),
                .groups="drop") %>%
      tidyr::pivot_longer(-z, names_to="var_z", values_to="share_z") %>%
      tidyr::separate(var_z, into=c("var","zflag"), sep="_z") %>%
      tidyr::pivot_wider(names_from=zflag, values_from=share_z,
                         names_prefix="share_z"),
    by="var"
  ) %>%
  mutate(N = nrow(df)) %>%
  select(var, share, share_z0, share_z1, N)

# - Continuas: media, sd, p25, p50, p75, min, max, y medias por Z # Dispersión global + balance por Z (medias).
tab_cont <- df %>%
  summarise(across(all_of(cont_vars),
                   list(mean=~mean(.x,na.rm=TRUE),
                        sd=~sd(.x,na.rm=TRUE),
                        p25=~qfun(.x,0.25),
                        p50=~qfun(.x,0.50),
                        p75=~qfun(.x,0.75),
                        min=~min(.x,na.rm=TRUE),
                        max=~max(.x,na.rm=TRUE)),
                   .names="{.col}_{.fn}")) %>%
  tidyr::pivot_longer(everything(),
                      names_to = c("var",".value"),
                      names_pattern = "(.*)_(.*)") %>%
  left_join(
    df %>%
      group_by(z) %>%
      summarise(across(all_of(cont_vars), ~mean(.x, na.rm=TRUE),
                       .names="{.col}_z{z}"),
                .groups="drop") %>%
      tidyr::pivot_longer(-z, names_to="var_z", values_to="mean_z") %>%
      tidyr::separate(var_z, into=c("var","zflag"), sep="_z") %>%
      tidyr::pivot_wider(names_from=zflag, values_from=mean_z,
                         names_prefix="mean_z"),
    by="var"
  ) %>%
  mutate(N = nrow(df)) %>%
  select(var, mean, sd, p25, p50, p75, min, max, mean_z0, mean_z1, N)

# - Etiquetas legibles y redondeo ---------------------------- # Mejora presentación para el paper/nota técnica.
var_labels <- c(                                             # Diccionario de nombres → etiquetas en español.
  y="Apoyo al régimen (y)",
  d="Ve TV extranjera (d)",
  z="Acceso señal (z)",
  female="Mujer (female)",
  urban="Urbano (urban)",
  age="Edad (age)",
  educ="Educación (años)"
)
tab_bin$Variable  <- var_labels[tab_bin$var]                  # Agrega etiqueta de variable a tabla de binarios.
tab_cont$Variable <- var_labels[tab_cont$var]                 # Agrega etiqueta a tabla de continuas.

tab_bin_out <- tab_bin %>%
  select(Variable, share, share_z0, share_z1, N) %>%          # Ordena columnas finales (binarios).
  mutate(across(where(is.numeric), ~round(.x, 3)))            # Redondea a 3 decimales para LaTeX limpio.

tab_cont_out <- tab_cont %>%
  select(Variable, mean, sd, p25, p50, p75, min, max, mean_z0, mean_z1, N) %>% # Ordena columnas (continuas).
  mutate(across(where(is.numeric), ~round(.x, 3)))            # Redondeo homogéneo.

# - Imprimir LaTeX (booktabs) -------------------------------- # Produce código LaTeX pegable en el documento final.
cat("\n% --- Tabla LaTeX: Binarios ---\n")                    # Comentario LaTeX para legibilidad del .tex.
kable(tab_bin_out,
      format = "latex", booktabs = TRUE, linesep = "",        # Formato LaTeX con booktabs y sin líneas extra.
      caption = "Estadísticas descriptivas — variables binarias", # Título de la tabla.
      col.names = c("Variable","Proporción","Proporción (Z=0)","Proporción (Z=1)","N"), # Encabezados claros.
      label = "tab:desc_bin") %>%                             # Etiqueta para \ref.
  kable_styling(latex_options=c("hold_position","striped")) %>% # Fijar posición y estilo listado.
  print()                                                     # Emite el código LaTeX en la consola/salida.

cat("\n% --- Tabla LaTeX: Continuas ---\n")                   # Comentario LaTeX para separar tablas en el .tex.
kable(tab_cont_out,
      format = "latex", booktabs = TRUE, linesep = "",        # Mismo estilo visual.
      caption = "Estadísticas descriptivas — variables continuas", # Título de la tabla.
      col.names = c("Variable","Media","Desv. Est.","P25","P50","P75","Mín","Máx","Media (Z=0)","Media (Z=1)","N"), # Encabezados.
      label = "tab:desc_cont") %>%                            # Etiqueta para \ref.
  kable_styling(latex_options=c("hold_position","striped")) %>% # Consistencia estética.
  print()                                                     # Emite el código LaTeX listo para pegar/compilar.
