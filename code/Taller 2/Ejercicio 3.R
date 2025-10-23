#### 1. Limpiar entorno
rm(list = ls())

#### 2. Definir directorios
root <- dirname(rstudioapi::getActiveDocumentContext()$path)
datos <- paste0(root,"\\datos")
resultados <- paste0(root,"\\Resultados")

#install.packages(c("haven", "dplyr", "purrr", "tidyr", "fixest","tidyverse","ggplot2"))
library(haven)
library(dplyr)
library(purrr)
library(tidyr)
library(fixest)
library(tidyverse)
library(ggplot2)
library(tidyverse)
#### 4. Cargar datos
sisben <- read_dta(paste0(datos, "\\sisben.dta"))

#### 5.  Lista de subconjuntos a resumir para completar la tabla del punto 3
subsets <- list(
  "Viven en Medellín" = sisben,
  "Cualquier tutela" = sisben %>% filter(tutela == 1),
  "Tutela acceso IVE" = sisben %>% filter(tutela_ive == 1)
)

    # 5.1 Función sencilla para calculas las estadísticas del data.frame
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

    # 5.2 Aplicando la función a los subconjuntos que definimos y uniendo los resultados
    #     Nota: no sabiamos bien como unir los resultados de los subconjuntos en una sola tabla
    #         asi que preguntamos a ChatGPT y nos siguirío este pedazo de código usando la función
    #         map_dfr de tidyverse.
table <- map_dfr(names(subsets), ~ {
  summarize_sisben(subsets[[.x]]) %>%
    mutate(group = .x)
}, .id = NULL) %>%
  pivot_longer(-group, names_to = "stat", values_to = "value") %>%
  pivot_wider(names_from = group, values_from = value) %>%
  mutate(stat = factor(stat,
                       levels = c("age_mean",
                                  "no_educ","prim_educ","sec_educ","bach_educ","sup_educ",
                                  "sisben_mean","n_household_mean",
                                  "any_children","n_children_mean","n_obs"))) %>%
  arrange(stat)

    # 5.3 Ordenando la tabla y aproximando a dos decimales
tabla_final <- table %>%
  mutate(across(-stat, ~round(., 2)))
print(tabla_final)

#### 6. propensión de que un juez hombre rechace una tutela para el acceso a IVE
#       vs una jueza

    # Histograma con la distribución de probabilidad (jueces y juezas)

    #Importar la base de datos
casos_tutelas <- read_dta(file.path(datos, "casos_tutelas.dta"))
          
    # Filtrando solo para las tutelas de acceso a IVE
casos_tutelas <- casos_tutelas %>% filter(tutela_ive == 1)

    #Calculamos la tasa de rechazo por juez en las tutelas de acceso a IVE
judge_rates <- casos_tutelas %>%
  filter(!is.na(id_juez)) %>%        #excluimos los casos en que no tenemos id_juez
  group_by(id_juez, woman_judge) %>%
  summarise(
    n_cases = n(),                        
    reject_rate = mean(rechazo == 1, na.rm = TRUE) # 
  ) %>%
  ungroup() %>%
  mutate(
    sex = ifelse(woman_judge == 1, "Mujer", "Hombre")
  )

    # Histograma de jueces y juezas
ggplot(judge_rates, aes(x = reject_rate, fill = sex, weight = n_cases)) +
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
          
    # Número de jueces y de casos por sexo
judge_rates %>% group_by(sex) %>%
  summarise(n_jueces = n(), total_cases = sum(n_cases))


    # Mediana ponderada por número de casos por sexo
judge_rates %>% group_by(sex) %>%
  summarise(weighted_mean = sum(reject_rate * n_cases) / sum(n_cases))


#### 7. regresión para poner a prueba la hipótesis de que, en promedio, los jueces hombres rechazan
#       más el acceso a la IVE relativo a las jueces mujeres
      #Nota: para este ejercicio utilizamos la especificación de Londoño & Saravia (OLS)
                      # y_i = rechazo_i (0/1)
                      # obtendremos el coeficiente sobre woman_judge (indicador de si la jueza es mujer)
                      # Usamos efectos fijos de oficina

fe_formula <- "rechazo ~ woman_judge | id_oficina"


    # Ejecutamos la regresión usando la función feols del paquete fixest, que está diseñado para la estimación
    # de modelos de regresión con efectos fijos.
estimacion <- feols(as.formula(fe_formula), data = casos_tutelas, cluster = "id_juez")

    # Resultados
summary(estimacion)


#### 8. metodología econométrica para refutar el argumento de nuesto compañero según el cual el hecho de que 
#       las juezas sean más severas en las tutelas relacionadas con el aborto se debe a que las juezas son en
#       general menos severas en todos los casos

    #Cargamos la data completa nuevamente, porque la habíamos manipulado
tutelas <- read_dta(file.path(datos, "casos_tutelas.dta"))

    # construir la variable de "Severidad" del juez usando solo casos NO-IVE
severidad_nonIVE <- tutelas %>%
  filter(tutela_ive == 0) %>%
  group_by(id_juez) %>%
  summarise(
    n_nonIVE = n(),
    severidad_nonIVE = mean(rechazo, na.rm = TRUE)
  ) %>%
  ungroup()

    # unir la variable al dataset de casos de IVE
df <- tutelas %>%
  left_join(severidad_nonIVE, by = "id_juez")
    
    #Usamos solo los casos de tutelas relacionadas a la IVE
df_ive <- df %>% filter(tutela_ive == 1)
    
    
fe_severidad_formula <- "rechazo ~ woman_judge + severidad_nonIVE | id_oficina"

esimacion1 <- feols(as.formula(fe_severidad_formula), data = df_ive, cluster = "id_juez")
summary(esimacion1)

#### 9. Estimación de la propuesta realizada en el punto 6 del taller,
# usando Nacimiento, Muerte e Infección. 
estimation_sample = read_dta(file.path(datos, "estimation_sample.dta"))

# --- (1) NACIMIENTO -------------------------------------------------
iv_nacimiento <- feols(
  Nacimiento ~ 1 | oficina^time | Rechazo_IVE ~ Female_Judge,
  data    = estimation_sample,
  cluster = ~ id_juez
)
summary(iv_nacimiento)

# --- (2) MUERTE -----------------------------------------------------
iv_muerte <- feols(
  Muerte ~ 1 | oficina^time | Rechazo_IVE ~ Female_Judge,
  data    = estimation_sample,
  cluster = ~ id_juez
)
summary(iv_muerte)

# --- (3) INFECCIÓN --------------------------------------------------
iv_infeccion <- feols(
  Infeccion ~ 1 | oficina^time | Rechazo_IVE ~ Female_Judge,
  data    = estimation_sample,
  cluster = ~ id_juez
)
summary(iv_infeccion)

# ---------- Instrumento alternativo: severidad del juez (leave-one-out) ----------
# Z_{j(i)} = (sum_j D - D_i)/(n_j - 1)
estimation_sample <- estimation_sample |>
  dplyr::group_by(id_juez) |>
  dplyr::mutate(
    .sumD = sum(Rechazo_IVE, na.rm = TRUE),
    .n    = dplyr::n(),
    Z_severity_loo = dplyr::if_else(.n > 1, (.sumD - Rechazo_IVE)/(.n - 1), NA_real_)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(Z_severity_loo = pmin(pmax(Z_severity_loo, 0), 1)) |>
  dplyr::select(-.sumD, -.n)

# Estimar con severidad LOO (misma FE: oficina^time)
iv_nacimiento_sev <- feols(
  Nacimiento ~ 1 | oficina^time | Rechazo_IVE ~ Z_severity_loo,
  data    = estimation_sample,
  cluster = ~ id_juez
)
summary(iv_nacimiento_sev)
iv_muerte_sev <- feols(
  Muerte ~ 1 | oficina^time | Rechazo_IVE ~ Z_severity_loo,
  data    = estimation_sample,
  cluster = ~ id_juez
)
summary(iv_muerte_sev)
iv_infeccion_sev <- feols(
  Infeccion ~ 1 | oficina^time | Rechazo_IVE ~ Z_severity_loo,
  data    = estimation_sample,
  cluster = ~ id_juez
)
summary(iv_infeccion_sev)

# Recolectar coeficientes de 2SLS (segunda etapa) con IC95%
tidy_one <- function(model, outcome){
  broom::tidy(model, conf.int = TRUE) |>
    dplyr::filter(term == "fit_Rechazo_IVE") |>
    dplyr::mutate(outcome = outcome)
}

# Instrumento: Female_Judge
coefs <- dplyr::bind_rows(
  tidy_one(iv_nacimiento, "Nacimiento"),
  tidy_one(iv_muerte,     "Muerte"),
  tidy_one(iv_infeccion,  "Infección")
) |>
  dplyr::select(outcome, estimate, std.error, conf.low, conf.high, p.value) |>
  dplyr::mutate(instrumento = "Female_Judge")

# Instrumento: Severidad LOO
coefs_sev <- dplyr::bind_rows(
  tidy_one(iv_nacimiento_sev, "Nacimiento"),
  tidy_one(iv_muerte_sev,     "Muerte"),
  tidy_one(iv_infeccion_sev,  "Infección")
) |>
  dplyr::select(outcome, estimate, std.error, conf.low, conf.high, p.value) |>
  dplyr::mutate(instrumento = "Severidad_LOO")

# Combinar y ordenar
coefs_all <- dplyr::bind_rows(coefs, coefs_sev)
coefs_all$instrumento <- factor(coefs_all$instrumento, levels = c("Female_Judge","Severidad_LOO"))
coefs_all$outcome     <- factor(coefs_all$outcome,     levels = c("Nacimiento","Muerte","Infección"))

# Gráfico único (ambos instrumentos en el mismo espacio)
x11()
g_coef <- ggplot(coefs_all,
                 aes(x = estimate, y = outcome,
                     color = instrumento, shape = instrumento)) +
  geom_point(size = 2, position = position_dodge(width = 0.30)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                height = 0.15, orientation = "y",
                position = position_dodge(width = 0.30)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Efecto 2SLS de Rechazo_IVE",
    y = NULL,
    title = "Efectos IV (2SLS) de Rechazo_IVE: ambos instrumentos con FE oficina×tiempo",
    subtitle = "IC 95% | FE: oficina×tiempo | SE agrupados por id_juez"
  ) +
  theme_minimal(base_size = 12) +
  guides(color = guide_legend(title = "Instrumento"),
         shape = guide_legend(title = "Instrumento"))

print(g_coef)
ggsave("coefplot_iv_unico.png", g_coef, width = 12, height = 10, dpi = 400)

# Helper para extraer fila de la segunda etapa
extract_row <- function(model, outcome, instrumento) {
  b   <- coef(model)["fit_Rechazo_IVE"]
  se_ <- fixest::se(model)["fit_Rechazo_IVE"]
  ci  <- confint(model, level = 0.95)["fit_Rechazo_IVE", ]
  p   <- 2 * pnorm(-abs(b / se_))
  N   <- stats::nobs(model)

  tibble::tibble(
    Outcome     = outcome,
    Instrumento = instrumento,
    Coef        = as.numeric(b),
    SE          = as.numeric(se_),
    CI_95_inf   = as.numeric(ci[1]),
    CI_95_sup   = as.numeric(ci[2]),
    p_value     = as.numeric(p),
    N           = as.integer(N)
  )
}

# Tablas por instrumento
tab_female <- dplyr::bind_rows(
  extract_row(iv_nacimiento, "Nacimiento", "Female_Judge"),
  extract_row(iv_muerte,     "Muerte",     "Female_Judge"),
  extract_row(iv_infeccion,  "Infección",  "Female_Judge")
)

tab_severity <- dplyr::bind_rows(
  extract_row(iv_nacimiento_sev, "Nacimiento", "Severidad_LOO"),
  extract_row(iv_muerte_sev,     "Muerte",     "Severidad_LOO"),
  extract_row(iv_infeccion_sev,  "Infección",  "Severidad_LOO")
)

# Medias de los outcomes entre no rechazadas (Rechazo_IVE == 0)
means_no_rej <- estimation_sample |>
  dplyr::filter(Rechazo_IVE == 0) |>
  dplyr::summarise(
    Nacimiento = mean(Nacimiento, na.rm = TRUE),
    Muerte     = mean(Muerte,     na.rm = TRUE),
    `Infección`  = mean(Infeccion,  na.rm = TRUE)
  ) |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = "Outcome",
                      values_to = "Mean_no_rechazo")

# Combinamos todo en una sola tabla
tab_full <- dplyr::bind_rows(tab_female, tab_severity) |>
  dplyr::left_join(means_no_rej, by = "Outcome") |>
  dplyr::mutate(
    Outcome     = factor(Outcome, levels = c("Nacimiento","Muerte","Infección")),
    Instrumento = factor(Instrumento, levels = c("Female_Judge","Severidad_LOO"))
  ) |>
  dplyr::arrange(Outcome, Instrumento)

# redondeo para lectura en consola
tab_full_print <- tab_full |>
  dplyr::mutate(
    Coef            = round(Coef, 6),
    SE              = round(SE, 6),
    CI_95_inf       = round(CI_95_inf, 6),
    CI_95_sup       = round(CI_95_sup, 6),
    p_value         = signif(p_value, 3),
    Mean_no_rechazo = round(Mean_no_rechazo, 6)
  )

# Imprimir la tabla completa en consola
print(tab_full_print, n = Inf, width = Inf)