#- script para seguir el tutorial nº 8: Tablas con .qmd y .Rmd
#- en el tutorial hay más contenido
library(tidyverse)
options(scipen = 999) #- para quitar la notación científica


#- cargamos datos para generar algunos resultados para mostrarlos en tablas
my_url <- "https://raw.githubusercontent.com/perezp44/iris_data/master/data/PIAAC_data_small.csv"
df_original <- read_csv(my_url)

df <- df_original %>% select(Country, Gender, Education, Wage_month, Wage_hour, Numeracy_score)

#- remotes::install_github("perezp44/pjpv2020.01")
df_aa <- pjpv2020.01::pjp_f_estadisticos_basicos(df) #- estadísticos básicos del df
df_bb <- pjpv2020.01::pjp_f_unique_values(df)        #- valores únicos de cada variable de df




#- tt_6 ------------------------------------------------------------------------
#- Calculamos la media, el mínimo, el máximo y la desviación típica de Wage_month
df_tt_6 <- df %>% 
    group_by(Country) %>% 
    summarise(W_medio  = mean(Wage_month, na.rm = TRUE) ,
              W_minimo = min(Wage_month, na.rm = TRUE)  ,
              W_maximo = max(Wage_month, na.rm = TRUE)  ,
              W_sd = sd(Wage_month, na.rm = TRUE) ) %>% 
              ungroup()


knitr::kable(df_tt_6)      #- funcionará ok en ficheros .qmd y .Rmd
knitr::kable(df_tt_6, format = "pipe") #- solo para verlo ahora


#- tt_9 ------------------------------------------------------------------------
#- ¿Cuanto más cobran los hombres (en %)?
df_tt_9 <- df %>% 
  group_by(Country, Gender) %>% 
  summarise(W_mes_medio = mean(Wage_month, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Gender, values_from = W_mes_medio) %>% 
  mutate(dif_W = Male-Female, dif_percent_W = dif_W/Female)

knitr::kable(df_tt_9, format = "pipe")


#- tt_10 ------------------------------------------------------------------------
#- Numeracy Score por país y nivel de estudios. La tabla nos va a salir alargada
df_tt_10 <- df %>% 
  group_by(Country, Education) %>% 
  summarise(Numeracy_media = mean(Numeracy_score, na.rm = TRUE)) %>% 
  ungroup() 

knitr::kable(df_tt_10, format = "pipe")


#- tt_11 -----------------------------------------------------------------------
#- Hagamos la anterior tabla más ancha (Una columna para cada país)
df_tt_11 <- df_tt_10 %>% 
            pivot_wider(names_from = Education, 
                        values_from = Numeracy_media)

knitr::kable(df_tt_11, format = "pipe")


#- quiero ordenar la tabla de menor a mayor nivel educativo
df <- df %>% 
  mutate(Education.f = forcats::as_factor(Education), .after = Education)

levels(df$Education.f)
df %>% count(Education)


#- renombrando los levels de los factores
df <- df %>% 
  mutate(Education.f = forcats::fct_recode(Education.f,
                    "Primaria"         = "Primary",
                    "Secundaria"       = "Secondary", 
                    "Secundaria_post"  = "Upper_second",
                    "Terciaria"        = "Tertiary" )) 
levels(df$Education.f)
df %>% count(Education.f)


#- reordenando los levels de un factor
df <- df %>% 
  mutate(Education.f = forcats::fct_relevel(Education.f, 
                                  "Primaria", "Secundaria", "Secundaria_post")) 
levels(df$Education.f)
df %>% count(Education.f)


#- tt_12 -----------------------------------------------------------------------
df_tt_12 <- df %>% 
  group_by(Country, Education.f) %>% 
  summarise(Numeracy_media = mean(Numeracy_score, na.rm = TRUE)) %>% 
  ungroup() %>%  
  pivot_wider(names_from = Education.f, 
              values_from = Numeracy_media) 

knitr::kable(df_tt_12, format = "pipe")

#- kable() más cosas -----------------------------------------------------------
#- kable() admite dar formato a algunos elementos de la tabla
knitr::kable(df_tt_12, format = "pipe",
             align = "c", 
             caption = "Numeracy Score por país",
             digits = 2, 
             format.args = list(decimal.mark = ",", big.mark = "."))


#- kableExtra pkg --------------------------------------------------------------
knitr::kable(df_tt_12) %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))


#- scroll box
df_tt_12 %>%
  knitr::kable() %>%
  kableExtra::kable_styling(font_size = 11) %>%
  kableExtra::scroll_box(width = "50%", height = "60%")


#- colorines, girar ...
knitr::kable(df_tt_12) %>%
       kableExtra::kable_styling(full_width = F) %>%
       kableExtra::column_spec(1, bold = T, border_right = T) %>%
       kableExtra::column_spec(3, width = "20em", background = "yellow") %>% 
       kableExtra::row_spec(3:4, bold = T, color = "white", background = "#D7261E") %>% 
       kableExtra::row_spec(0, angle = 10)

no_quitar <- c("df", "df_tt_6")
rm(list=ls()[! ls() %in% no_quitar])

#- pkg gt: un ejemplo ----------------------------------------------------------
library(gt) #-remotes::install_github("rstudio/gt")

gt::gt(df_tt_6)


#- la tabla, al igual q un ggplot, se guarda en un objeto de clase "list"
gt_tbl <- df_tt_6 %>% gt()
gt_tbl

gt_tbl <- gt_tbl %>% tab_header(title = md("**Genero y nivel educativo**"),
                      subtitle = md("Porcentaje de *H y M* en cada nivel educativo"))
gt_tbl


#- pkg DT: un ejemplo ----------------------------------------------------------
DT::datatable(iris)
DT::datatable(df_tt_6)



DT::datatable(iris, filter = 'top', 
              options = list(pageLength = 5, autoWidth = TRUE ))

#- pkg reactable: un ejemplo ----------------------------------------------------------
reactable::reactable(iris)


#- pkg rpivotTable: un ejemplo ----------------------------------------------------------

library(rpivotTable) #- remotes::install_github(c("ramnathv/htmlwidgets", "smartinsightsfromdata/rpivotTable"))
rpivotTable(df, rows = "Gender", cols = c("Country"), width = "100%", height = "400px")


#- tablas para modelos ---------------------------------------------------------
urla <- "https://raw.githubusercontent.com/perezp44/iris_data/master/data/PIAAC_data_small.csv"
df <- read_csv(urla)


#- estimamos un modelo lineal
my_model <- lm(Wage_hour ~ Numeracy_score + Gender , data = df)
my_model

summary(my_model)


#- pkg stargazer ---------------------------------------------------------------
stargazer::stargazer(my_model, type = "html")
stargazer::stargazer(my_model, type = "text")


#- creo una variable dicotómica (para estimar un Logit)
df <- df %>% 
  mutate(Numeracy_score_b = 
                      ifelse(Numeracy_score > mean(Numeracy_score, na.rm = TRUE), 1, 0)) 

#- estimamos varios modelos y los almacenamos en una lista
my_models <- list()
my_models[['W:  OLS 1']]   <- lm( Wage_hour         ~ Numeracy_score + Gender , df)
my_models[['Nu: OLS 2']]   <- lm( Numeracy_score    ~ Education + Gender , df)
my_models[['Nu: Logit 1']] <- glm( Numeracy_score_b ~ Education + Gender , df, family = binomial())


stargazer::stargazer(my_models, type = "html", title = "Results", align = TRUE)
stargazer::stargazer(my_models, type = "text", title = "Results", align = TRUE)


#- pkg modelsummary ------------------------------------------------------------
library(modelsummary) #- remotes::install_github('vincentarelbundock/modelsummary')

mm <- modelsummary::msummary(my_models, title = "Resultados de estimación")
mm



#- más pkgs para tablas descriptivas -------------------------------------------

#- pkg janitor
df %>% janitor::tabyl(Education, Country, Gender)


