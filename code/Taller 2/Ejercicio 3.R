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
