############################################################
# Proyecto: Lumeria 1989 — TV extranjera y apoyo al régimen
# Autor: (tu nombre)
# Fecha: Sys.Date()
# Descripción: Script reproducible para (1) descriptivas,
# (2) MCO, (3) IV 2SLS con Z como instrumento de D,
# (4) comparación MCO vs IV, (5) Wald, (6) interpretación LATE.
# Notación/Metodología basada en "s9_IV.pdf".
#   - Teorema LATE y supuestos I–IV (independencia, exclusión,
#     relevancia, monotonicidad)  :contentReference[oaicite:0]{index=0}  :contentReference[oaicite:1]{index=1}
#   - Estimador de Wald (poblacional y análogo muestral)        :contentReference[oaicite:2]{index=2}  :contentReference[oaicite:3]{index=3}
#   - MC2E / 2SLS formal (proyecciones y 2ª etapa)              :contentReference[oaicite:4]{index=4}  :contentReference[oaicite:5]{index=5}  :contentReference[oaicite:6]{index=6}
#   - Primera etapa y relevancia (condición de rango / F)       :contentReference[oaicite:7]{index=7}  :contentReference[oaicite:8]{index=8}
#   - LATE, ATT, ATE y cuándo coinciden                         :contentReference[oaicite:9]{index=9}  :contentReference[oaicite:10]{index=10}
############################################################

#### 0) Paquetes ----
# Cada library() carga un paquete necesario para lectura, estimación y tablas.
# Si algún paquete no está instalado, descomenta la línea install.packages().
# install.packages(c("haven","dplyr","stringr","broom","AER","sandwich","lmtest","modelsummary"))
library(haven)        # leer .dta
library(dplyr)        # manipulación
library(stringr)      # utilidades de cadenas
library(broom)        # tidiers para modelos
library(AER)          # ivreg (2SLS) y diagnósticos
library(sandwich)     # VCOV robustas / cluster
library(lmtest)       # coeftest con vcov
library(modelsummary) # tablas comparativas (opcional)

#### 1) Cargar datos ----
# Leemos la base entregada: Lumeria1989.dta
df <- read_dta("Lumeria1989.dta")

# Estandarizamos nombres a minúsculas para detección flexible
names(df) <- tolower(names(df))

#### 1.1) Mapeo de variables clave ----
# Intentamos detectar automáticamente:
#  - z: acceso técnico a la señal extranjera (instrumento, binario)
#  - d: ver efectivamente la TV extranjera (tratamiento, binario)
#  - y: apoyo al régimen (resultado, binario o continuo)
#  - controles: edad, sexo, educación, urbano (si existen)
# Si tus nombres difieren, AJUSTA el vector 'aliases' o asigna manualmente.

aliases <- list(
  z = c("z_ir","z","acceso","access","signal","cobertura"),
  d = c("d_i","d","ver","tv_ext","tvextranjera","watch"),
  y = c("y_i","y","apoyo","support","vota_regimen","voto_regimen"),
  edad = c("edad","age"),
  sexo = c("sexo","sex","female","male"),
  educ = c("educ","educacion","schooling","years_school"),
  urbano = c("urbano","urban","ciudad")
)

pick_var <- function(candidates, data){
  found <- intersect(candidates, names(data))
  if(length(found)==0) return(NA_character_)
  found[1]
}

var_z     <- pick_var(aliases$z, df)
var_d     <- pick_var(aliases$d, df)
var_y     <- pick_var(aliases$y, df)
var_edad  <- pick_var(aliases$edad, df)
var_sexo  <- pick_var(aliases$sexo, df)
var_educ  <- pick_var(aliases$educ, df)
var_urb   <- pick_var(aliases$urbano, df)

# Lista de controles disponibles en la base
ctrls <- c(var_edad, var_sexo, var_educ, var_urb)
ctrls <- ctrls[!is.na(ctrls)]

# Validación mínima
needed <- c(var_z, var_d, var_y)
if(any(is.na(needed))){
  stop("No pude detectar Z/D/Y automáticamente. Ajusta 'aliases' o asigna var_z/var_d/var_y manualmente.")
}

#### 1.2) Limpieza ligera ----
# Aseguramos binariedad de Z y D si vienen como numéricas no 0/1:
df <- df %>%
  mutate(
    !!var_z := as.integer(!!sym(var_z) > 0),
    !!var_d := as.integer(!!sym(var_d) > 0)
  )

#### 2) (P1) Estadísticas descriptivas ----
# Objetivo: caracterizar apoyo (Y), exposición (D), acceso (Z) y demografía.
# Producimos medias generales y por Z (cobertura vs sombra).
desc_vars <- c(var_y, var_d, var_z, ctrls)
desc_label <- setNames(desc_vars, desc_vars)

# Descriptivas globales
desc_global <- df %>%
  summarise(across(all_of(desc_vars),
                   list(mean = ~mean(.x, na.rm=TRUE),
                        sd   = ~sd(.x, na.rm=TRUE)),
                   .names = "{.col}_{.fn}"))

print(desc_global)

# Descriptivas por acceso (Z)
desc_by_z <- df %>%
  group_by(!!sym(var_z)) %>%
  summarise(across(all_of(desc_vars), ~mean(.x, na.rm=TRUE), .names = "{.col}_mean"),
            n = n(), .groups="drop")
print(desc_by_z)

# Diferencias de medias Y, D y controles entre Z=1 vs Z=0 (t-test):
diff_tests <- lapply(setNames(desc_vars, desc_vars), function(v){
  t.test(df[[v]] ~ df[[var_z]])
})
# Vistazo rápido:
lapply(diff_tests[c(var_y, var_d, ctrls)], function(tt) c(est=unname(diff(tt$estimate)), p=tt$p.value))

# RESPUESTA P1 (≤150 palabras, comentario):
# Hallamos diferencias descriptivas entre regiones con y sin acceso (Z): si E[D|Z=1] >> E[D|Z=0],
# hay relevancia mecánica del instrumento. Si E[Y|Z=1] ≠ E[Y|Z=0], el "reduced form" sugiere
# asociación total (vía todos los canales). Diferencias en X por Z alertan sobre composición:
# si controles difieren (edad, educación, urbano), el ajuste por X será clave. Implicación:
# MCO en Y~D puede estar sesgado por selección; usar Z como variable instrumental permite
# separar la variación “as good as random” inducida por la geografía de señal. No obstante,
# si Z correlaciona con factores políticos locales que afectan Y aparte de D, podría fallar
# la exclusión. Estas pautas condicionan la interpretación causal posterior.

#### 3) (P2) MCO y sesgo de selección ----
# Ecuación: Y_i = α + β D_i + γ' X_i + u_i
# β capta el cambio promedio en Y al pasar de no ver a ver TV extranjera, condicional en X.
# Sesgo: si D_i está correlacionada con u_i (gusto político, redes, censura local), β_MCO no es causal.

# Fórmulas de regresión
f_ols <- as.formula(
  paste(var_y, "~", var_d, if(length(ctrls)>0) paste("+", paste(ctrls, collapse="+")) else "")
)

# Estimación MCO con errores robustos (HC1)
ols_fit <- lm(f_ols, data=df)
ols_vcov <- vcovHC(ols_fit, type="HC1")
ols_res  <- coeftest(ols_fit, vcov.=ols_vcov)
print(ols_res)

# RESPUESTA P2(a) (≤150 palabras):
# Interpretación: β es el efecto medio condicional de D sobre Y. No es necesariamente causal
# porque D_i puede ser endógena (autoselección en ver TV extranjera, variables omitidas).
# Sin un diseño cuasi-experimental, β mezcla causalidad con correlaciones espurias.
# RESPUESTA P2(b) (≤150 palabras):
# No observados potencialmente sesgantes: (i) preferencia política previa por el régimen;
# (ii) tolerancia al riesgo de sanciones; (iii) capital social y acceso a redes informativas;
# (iv) calidad del cable clandestino; (v) vigilancia local. Tales factores influyen en D (ver)
# y en Y (apoyo), correlacionando D con u_i.

#### 4) (P3) IV / 2SLS con Z como instrumento de D ----
# Primera etapa: D_i = π0 + π1 Z_i + δ' X_i + v_i   (relevancia: π1 ≠ 0)
# Segunda etapa: Y_i = α + τ D̂_i + γ' X_i + ε_i   (τ es el efecto causal LATE sobre compliers)
# Notación/MC2E: ver s9_IV.pdf  :contentReference[oaicite:11]{index=11}  :contentReference[oaicite:12]{index=12}

# Fórmulas para ivreg (AER): Y ~ D + X | Z + X
f_iv <- as.formula(
  paste(var_y, "~", var_d, if(length(ctrls)>0) paste("+", paste(ctrls, collapse="+")) else "",
        "|", var_z,        if(length(ctrls)>0) paste("+", paste(ctrls, collapse="+")) else "")
)

iv_fit <- ivreg(f_iv, data=df)

# VCOV robusta (HC1)
iv_vcov <- sandwich::vcovHC(iv_fit, type="HC1")
iv_sum  <- coeftest(iv_fit, vcov.=iv_vcov)
print(iv_sum)

# 4(a) Primera etapa explícita (para reportar π1 y F de exclusión)
f_fs <- as.formula(
  paste(var_d, "~", var_z, if(length(ctrls)>0) paste("+", paste(ctrls, collapse="+")) else "")
)
fs_fit <- lm(f_fs, data=df)
fs_vcov <- vcovHC(fs_fit, type="HC1")
fs_res  <- coeftest(fs_fit, vcov.=fs_vcov)
print(fs_res)

# Diagnósticos de instrumentos (AER): incluye F de primera etapa y pruebas de debilidad
iv_diag <- summary(iv_fit, vcov.=iv_vcov, diagnostics=TRUE)
print(iv_diag$diagnostics)  # mira "Weak instruments"

# RESPUESTA P3(a):
# Primera etapa:  D_i = π0 + π1 Z_i + δ' X_i + v_i
# Segunda etapa:  Y_i = α + τ D̂_i + γ' X_i + ε_i
# τ (coef. de D̂_i) es el efecto causal (LATE) bajo supuestos I–IV.  :contentReference[oaicite:13]{index=13}
# RESPUESTA P3(b) — Supuestos LATE:
# I) Independencia: (Y(1),Y(0),D(1),D(0)) ⟂ z  ; 
# II) Exclusión: Y(D,1)=Y(D,0)=Y(D);
# III) Relevancia: E[D(1)-D(0)]≠0 ; 
# IV) Monotonicidad: D(1)−D(0)≥0 (sin defiers).  :contentReference[oaicite:14]{index=14}
# En Lumeria, (I) plausible por geografía; (II) puede fallar si el acceso afecta Y por canales
# distintos a ver TV (miedo/sanciones); (III) fuerte si la sombra topográfica reduce señal;
# (IV) creíble: tener señal no debería reducir la probabilidad de ver TV extranjera.
# RESPUESTA P3(c):
# Tipos: nunca-tratados, siempre-tratados, compliers, defiers; monotonicidad excluye defiers.  :contentReference[oaicite:15]{index=15}
# LATE se estima sobre los compliers (quienes cambian D por Z).  :contentReference[oaicite:16]{index=16}
# RESPUESTA P3(d):
# ATE (pob. total), ATT (tratados), LATE (compliers). Coinciden si los efectos son homogéneos
# (τ_i = τ ∀i); o si la subpoblación complier coincide con la tratada o con toda la población.  :contentReference[oaicite:17]{index=17}

#### 5) (P4) Comparar MCO vs IV y reportar relevancia ----
# Tabla compacta con β_MCO y τ_IV
modelsummary(
  list("MCO"=ols_fit, "IV (2SLS)"=iv_fit),
  vcov = list(ols_vcov, iv_vcov),
  gof_map = tribble(
    ~raw,                     ~clean,                            ~fmt,
    "nobs",                   "Observaciones",                    0,
    "r.squared",              "R^2",                              3
  ),
  statistic = "({std.error}){stars}",
  stars = c('*' = .1, '**' = .05, '***' = .01)
)

# RESPUESTA P4(a):
# Reporta π1 (coef de Z en la 1ª etapa) y su p-valor/IC. Revisa el F de exclusión:
# F>10 sugiere instrumento no débil. (ver 'Weak instruments' en iv_diag).  :contentReference[oaicite:18]{index=18}
# RESPUESTA P4(b):
# Compara magnitud y signo: si |τ_IV| ≠ |β_MCO|, indica sesgo MCO (p.ej., selección negativa/positiva).
# RESPUESTA P4(c) (≤200 palabras):
# τ_IV es un LATE: efecto causal para compliers (hogares que verían TV extranjera si hay señal,
# pero no la verían sin señal). La inferencia es local al margen inducido por Z; su validez interna
# es alta, pero la validez externa depende de cuán representativos son los compliers del resto
# (diferentes instrumentos identifican distintos LATE).  :contentReference[oaicite:19]{index=19}

#### 6) (P5) Estimador de Wald y binarización de tratamientos ----
# Wald muestral:  τ̂_W = [E(Y|Z=1)-E(Y|Z=0)] / [E(D|Z=1)-E(D|Z=0)]  (binario Z y D)
EY1 <- mean(df[[var_y]][df[[var_z]]==1], na.rm=TRUE)
EY0 <- mean(df[[var_y]][df[[var_z]]==0], na.rm=TRUE)
ED1 <- mean(df[[var_d]][df[[var_z]]==1], na.rm=TRUE)
ED0 <- mean(df[[var_d]][df[[var_z]]==0], na.rm=TRUE)
wald_hat <- (EY1 - EY0) / (ED1 - ED0)
wald_hat

# RESPUESTA P5(a):
# Wald (poblacional): τ = [E(Y|z=1)-E(Y|z=0)] / [E(D|z=1)-E(D|z=0)].
# Numerador: efecto reducido de z sobre Y; Denominador: 1ª etapa de z sobre D.  :contentReference[oaicite:20]{index=20}
# RESPUESTA P5(b):
# Si el “tratamiento” real es continuo (horas W_i) y lo binarizamos D_i=1{W_i≥j*}, el
# instrumento puede afectar Y a través de variación “intensiva” en W dentro de los bins
# (no sólo cruzando j*), violando exclusión respecto a D. Válido sólo si (i) Y depende de W
# únicamente vía el umbral (outcome-threshold invariance), o (ii) el instrumento desplaza W
# exclusivamente cruzando j* (sin mover intensidades dentro de cada lado). Tests: chequear
# independencia de Z con W condicional a D (balance interno), y sensibilidad del τ̂_W a
# umbrales alternativos j*. (Idea basada en Eckhof & Huber, 2018).

#### 7) (P6) Nueva evidencia: hogares con acceso deciden no ver por temor ----
# RESPUESTA P6(a):
# Relevancia (III) se debilita si el miedo reduce fuertemente D|Z=1; exclusión (II) peligra si Z
# afecta Y vía miedo/sanción además de D; monotonicidad (IV) generalmente sigue plausible:
# tener acceso no aumenta la probabilidad de NO ver con respecto a no tenerlo.  :contentReference[oaicite:21]{index=21}
# RESPUESTA P6(b):
# τ_IV sigue interpretándose como LATE para compliers restantes; si la 1ª etapa cae (F bajo),
# aumentan preocupaciones por instrumentos débiles.  :contentReference[oaicite:22]{index=22}
# RESPUESTA P6(c) (≤150 palabras):
# No invalida por completo la estrategia: si (II) se sostiene tras controles/contexto y (IV) es creíble,
# recuperamos un LATE local interpretable. Pero si el miedo impacta Y directamente, la exclusión
# falla y el IV pierde interpretación causal. Análisis de falsación/balance y evidencia institucional
# son cruciales.

#### 8) Guardar resultados (opcional) ----
# modelsummary puede exportar a LaTeX/HTML/MD:
# modelsummary(list("MCO"=ols_fit,"IV (2SLS)"=iv_fit), output="modelos_IV_Lumeria.html", vcov=list(ols_vcov,iv_vcov))

#### 9) Notas finales ----
# - Considera clúster por región si existe variable 'region' en df (p.ej., usando vcovCL con ~region).
# - Verificar robustez: especificaciones con/ sin controles X; submuestras geográficas; placebo outcomes.
# - Documentar claramente la plausibilidad de (II) exclusión a la luz del contexto institucional.
