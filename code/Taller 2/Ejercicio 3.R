#### Ejercicio 3
# Proyecto: IVE Medellín — SISBEN, tutelas y jueces (sexo)
# Autores: Simón Briceño - Mahicol Ramírez
# Fecha: 24 de octubre de 2025
# Descripción general:
# 1) Subconjuntos y resumen SISBEN
#    Bloque 1: descriptivas para la tabla del punto 3.
# 2) Rechazo IVE por sexo del juez
#    Bloque 2: tasas por juez y gráfico de distribución.
# 3) MCO con FE de oficina
#    Bloque 3: OLS de rechazo ~ woman_judge | id_oficina (cluster id_juez).
# 4) Control por severidad en NO-IVE
#    Bloque 4: severidad_nonIVE y estimación con FE.
# 5) IV (2SLS): Nacimiento, Muerte e Infección
#    Bloque 5: instrumento Female_Judge; coef, IC95 y gráfico.
# 6) Tablas y medias de no rechazadas
#    Bloque 6: integración de resultados y redondeo para reporte.
# Archivos: sisben.dta, casos_tutelas.dta, estimation_sample.dta
# Salidas: Densidad de rechazo H/M, coefplot_iv.png y tablas en consola
# Referencia metodológica: Londoño-Vélez y Saravia (OLS/IV con jueces).
#### 1. Limpiar entorno
rm(list = ls())

#### 2. Definir directorios
root <- dirname(rstudioapi::getActiveDocumentContext()$path)
datos <- paste0(root,"\\datos")
resultados <- paste0(root,"\\Resultados")

#### 3. Cargar paquetes
# install.packages(c("haven","dplyr","purrr","tidyr","fixest","tidyverse",
#                    "ggplot2"))
library(haven)
library(dplyr)
library(purrr)
library(tidyr)
library(fixest)
library(tidyverse)
library(ggplot2)
library(tidyverse)
library(broom)

#### 4. Cargar datos (SISBEN)
sisben <- read_dta(paste0(datos, "\\sisben.dta"))

#### 5. Subconjuntos y resumen (punto 3 de la tabla)
subsets <- list(
  "Viven en Medellín" = sisben,
  "Cualquier tutela" = sisben %>% filter(tutela == 1),
  "Tutela acceso IVE" = sisben %>% filter(tutela_ive == 1)
)

#### 5.1 Función para resumir estadísticas del data.frame
summarize_sisben <- function(data){
  data %>%
    summarise(
      age_mean            = mean(age_census, na.rm = TRUE),
      no_educ             = mean(no_educ, na.rm = TRUE) ,
      prim_educ           = mean(prim_educ, na.rm = TRUE),
      sec_educ            = mean(sec_educ, na.rm = TRUE),
      bach_educ           = mean(bach_educ, na.rm = TRUE),
      sup_educ            = mean(sup_educ, na.rm = TRUE),
      sisben_mean         = mean(sisben_score, na.rm = TRUE),
      n_household_mean    = mean(n_household, na.rm = TRUE),
      any_children        = mean(any_children, na.rm = TRUE),
      n_children_mean     = mean(n_children, na.rm = TRUE),
      n_obs               = n()
    )
}

#### 5.2 Aplicar función a subconjuntos y unir resultados con map_dfr
# Nota: la unión de resultados se hace en una sola tabla con map_dfr.
table <- map_dfr(names(subsets), ~ {
  summarize_sisben(subsets[[.x]]) %>%
    mutate(group = .x)
}, .id = NULL) %>%
  pivot_longer(-group, names_to = "stat", values_to = "value") %>%
  pivot_wider(names_from = group, values_from = value) %>%
  mutate(stat = factor(
    stat,
    levels = c("age_mean",
               "no_educ","prim_educ","sec_educ","bach_educ","sup_educ",
               "sisben_mean","n_household_mean",
               "any_children","n_children_mean","n_obs")
  )) %>%
  arrange(stat)

#### 5.3 Ordenar tabla y redondear a dos decimales
tabla_final <- table %>%
  mutate(across(-stat, ~round(., 2)))
print(tabla_final)

#### 6. Distribución: propensión a rechazo en IVE por sexo del juez
# 6.1 Importar base de datos de tutelas
casos_tutelas <- read_dta(paste0(datos, "\\casos_tutelas.dta"))

# 6.2 Filtrar tutelas de acceso a IVE
casos_tutelas <- casos_tutelas %>% filter(tutela_ive == 1)

# 6.3 Calcular tasa de rechazo por juez (ponderado por número de casos)
judge_rates <- casos_tutelas %>%
  filter(!is.na(id_juez)) %>%        # excluir casos sin id_juez
  group_by(id_juez, woman_judge) %>%
  summarise(
    n_cases = n(),
    reject_rate = mean(rechazo == 1, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(sex = ifelse(woman_judge == 1, "Mujer", "Hombre"))

# 6.4 Histograma de jueces y juezas
hst=ggplot(judge_rates, aes(x = reject_rate, fill = sex, weight = n_cases)) +
  geom_histogram(position = "identity", alpha = 0.45, bins = 20) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Tasa de rechazo (por juez)",
    y = "Recuento ponderado (por número de tutelas gestionadas)",
    fill = "",
    title = "Distribución de la tasa de rechazo por juez",
    subtitle = "Comparación: jueces hombres vs. juezas (cada juez ponderado por su número de tutelas)"
  ) +
  theme_minimal()
ggsave(paste0(resultados, "\\densidad_rechazo_HM.png"))
# 6.5 Número de jueces y casos por sexo
judge_rates %>% group_by(sex) %>%
  summarise(n_jueces = n(), total_cases = sum(n_cases))

# 6.6 Media ponderada por número de casos y sexo
judge_rates %>% group_by(sex) %>%
  summarise(weighted_mean = sum(reject_rate * n_cases) / sum(n_cases))

#### 7. Regresión OLS: woman_judge con FE de oficina
# Especificación basada en Londoño & Saravia (OLS). Variable dependiente:
# rechazo (0/1). Interesa el coeficiente de woman_judge. FE por id_oficina.
fe_formula <- "rechazo ~ woman_judge | id_oficina"

# Estimación con feols (fixest), con clustering por id_juez
estimacion <- feols(as.formula(fe_formula), data = casos_tutelas,
                    cluster = "id_juez")

# Resultados
summary(estimacion)

#### 8. Metodología: severidad en NO-IVE como control
# 8.1 Cargar data completa nuevamente
tutelas <- read_dta(paste0(datos, "\\casos_tutelas.dta"))

# 8.2 Construir severidad del juez usando solo casos NO-IVE
severidad_nonIVE <- tutelas %>%
  filter(tutela_ive == 0) %>%
  group_by(id_juez) %>%
  summarise(
    n_nonIVE = n(),
    severidad_nonIVE = mean(rechazo, na.rm = TRUE)
  ) %>%
  ungroup()

# 8.3 Unir severidad al dataset y filtrar IVE
df <- tutelas %>%
  left_join(severidad_nonIVE, by = "id_juez")
df_ive <- df %>% filter(tutela_ive == 1)

# 8.4 Estimar modelo con FE por oficina
fe_severidad_formula <- "rechazo ~ woman_judge + severidad_nonIVE | id_oficina"
esimacion1 <- feols(as.formula(fe_severidad_formula), data = df_ive,
                    cluster = "id_juez")
summary(esimacion1)

#### 9. Estimación IV (2SLS): Nacimiento, Muerte e Infección
estimation_sample = read_dta(file.path(datos, "estimation_sample.dta"))

# Nacimiento
iv_nacimiento <- feols(
  Nacimiento ~ 1 | Rechazo_IVE ~ Female_Judge,
  data = estimation_sample
)
summary(iv_nacimiento)

# Muerte
iv_muerte <- feols(
  Muerte ~ 1 | Rechazo_IVE ~ Female_Judge,
  data = estimation_sample
)
summary(iv_muerte)

# Infección
iv_infeccion <- feols(
  Infeccion ~ 1 | Rechazo_IVE ~ Female_Judge,
  data = estimation_sample
)
summary(iv_infeccion)

#### 9.1 Coeficientes 2SLS con IC al 95%
tidy_one <- function(model, outcome){
  broom::tidy(model, conf.int = TRUE) |>
    dplyr::filter(term == "fit_Rechazo_IVE") |>
    dplyr::mutate(outcome = outcome)
}

coefs <- dplyr::bind_rows(
  tidy_one(iv_nacimiento, "Nacimiento"),
  tidy_one(iv_muerte,     "Muerte"),
  tidy_one(iv_infeccion,  "Infección")
) |>
  dplyr::select(outcome, estimate, std.error, conf.low, conf.high, p.value)

# Orden para graficar
coefs$outcome <- factor(coefs$outcome,
                        levels = c("Nacimiento","Muerte","Infección"))

#### 9.2 Gráfico de coeficientes e intervalos de confianza
g_coef <- ggplot(coefs, aes(x = estimate, y = outcome)) +
  geom_point(size = 2.6, color = "#000000") +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.15,
    orientation = "y",
    color = "#ff0800",
    linewidth = 0.3
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  labs(
    x = "Efecto 2SLS de Rechazo de tutela de IVE",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_text(size = 13, face = "bold"),
    axis.title.y = element_text(size = 13, face = "bold"),
    axis.text.x  = element_text(size = 11),
    axis.text.y  = element_text(size = 11),
    plot.title   = element_blank(),
    plot.subtitle= element_blank(),
    legend.position = "none"
  )
ggsave(paste0(resultados, "\\coefs.png"))
x11()
print(g_coef)
#### 9.3 Tabla detallada (segunda etapa)
extract_row <- function(model, outcome) {
  b   <- coef(model)["fit_Rechazo_IVE"]
  se_ <- fixest::se(model)["fit_Rechazo_IVE"]   # SE clásicos (no cluster)
  ci  <- confint(model, level = 0.95)["fit_Rechazo_IVE", ]
  p   <- 2 * pnorm(-abs(b / se_))
  N   <- stats::nobs(model)

  tibble::tibble(
    Outcome     = outcome,
    Instrumento = "Female_Judge",
    Coef        = as.numeric(b),
    SE          = as.numeric(se_),
    CI_95_inf   = as.numeric(ci[1]),
    CI_95_sup   = as.numeric(ci[2]),
    p_value     = as.numeric(p),
    N           = as.integer(N)
  )
}

tab_female <- dplyr::bind_rows(
  extract_row(iv_nacimiento, "Nacimiento"),
  extract_row(iv_muerte,     "Muerte"),
  extract_row(iv_infeccion,  "Infección")
)

#### 9.4 Medias en no rechazadas (según solicitud)
means_no_rej <- estimation_sample |>
  dplyr::filter(Rechazo_IVE == 0) |>
  dplyr::summarise(
    Nacimiento = mean(Nacimiento, na.rm = TRUE),
    Muerte     = mean(Muerte,     na.rm = TRUE),
    `Infección`= mean(Infeccion,  na.rm = TRUE)
  ) |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = "Outcome",
                      values_to = "Mean_no_rechazo")

#### 9.5 Tabla final combinada y redondeo
tab_full <- tab_female |>
  dplyr::left_join(means_no_rej, by = "Outcome") |>
  dplyr::mutate(
    Outcome     = factor(Outcome,
                         levels = c("Nacimiento","Muerte","Infección")),
    Instrumento = factor(Instrumento, levels = "Female_Judge")
  ) |>
  dplyr::arrange(Outcome)

# Redondeo para lectura en consola
tab_full_print <- tab_full |>
  dplyr::mutate(
    Coef            = round(Coef, 6),
    SE              = round(SE, 6),
    CI_95_inf       = round(CI_95_inf, 6),
    CI_95_sup       = round(CI_95_sup, 6),
    p_value         = signif(p_value, 3),
    Mean_no_rechazo = round(Mean_no_rechazo, 6)
  )

print(tab_full_print, n = Inf, width = Inf)
