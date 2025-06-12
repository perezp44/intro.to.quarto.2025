#- ejemplo para ilustrar algunas ideas y pkgs para EDA
#- más cosas en el tutorial
library(tidyverse)

#- DATOS (de bebes) ------------------------------------------------------------
url_bebes <- "https://github.com/perezp44/archivos_download.2/raw/main/df_bebes_EDA.rds"
df <- rio::import(url_bebes)

df_dicc <- pjpv.curso.R.2022::pjp_dicc(df)
df_uniques <- pjpv.curso.R.2022::pjp_valores_unicos(df)

#- INFORMES DESCRIPTIVOS -------------------------------------------------------
#- Hay pkgs q hacen informes descriptivos completos

#- summarytools pkg
zz <- summarytools::dfSummary(df) #- genera un fichero/df con un resumen útil y agradable de ver
summarytools::view(zz)            #- para visualizar el informe


#- ej: un nuevo paquete con una tabla chula: https://github.com/agstn/dataxray
#devtools::install_github("glin/reactable")
#devtools::install_github("agstn/dataxray")
library(dataxray)

df %>% make_xray() %>% view_xray()



#- NA's ------------------------------------------------------------------------
naniar::gg_miss_var(df, show_pct = TRUE)
naniar::gg_miss_var(df, facet = estudios_madre.ff, show_pct = TRUE) #- faceted por la variable estudios_madre.ff

#- co-ocurrencias de NA's
naniar::gg_miss_upset(df)


#- ¿QUÉ HACEMOS con los NA's? --------------------------------------------------
#- quitamos las filas q tengan NA en cualquiera de las variables/columnas
zz1 <- df %>% tidyr::drop_na()

zz2 <- df %>% tidyr::drop_na(c(peso_bebe, parto_nn_semanas))   #- quito filas con NA en peso_bebe o en SEMANAS


#- SELECCIONAR las vv. NUMÉRICAS -----------------------------------------------
df_numeric <- df %>% select_if(is.numeric)      #- antigua sintaxis
df_numeric <- df %>% select(where(is.numeric))  #- nueva API

summarytools::descr(df)

DataExplorer::plot_histogram(df, ncol = 2)
DataExplorer::plot_density(df, ncol = 2)

corrr::correlate(df_numeric)

#- tabla con matriz de correlaciones, con gt::gt()
df_numeric %>% corrr::correlate() %>%
  gt::gt() %>%
  gt::fmt_number(decimals = 1, sep_mark = ".",  dec_mark = ",")

df %>% inspectdf::inspect_cor() %>% gt::gt()
df %>% inspectdf::inspect_cor() %>% inspectdf::show_plot()

#- https://r-coder.com/correlation-plot-r/
#- https://albert-rapp.de/posts/ggplot2-tips/13_alternative_corrplots/13_alternative_corrplots.html


#- Boxplots ------------------
#DataExplorer::plot_boxplot(df, by = "estudios_madre.ff")
DataExplorer::plot_boxplot(df, by = "parto_cesarea.f")

#- boxplots (3 v.)
df %>% explore::explore(edad_madre.1, estudios_madre.ff, target = parto_cesarea.f)

#- explore_all()
df %>% select(sexo_bebe.f, edad_madre.1, peso_bebe,  estudios_madre.ff, parto_cesarea.f) %>%
  explore::explore_all(target = parto_cesarea.f)

#- janitor:: tabulaciones cruzadas
df %>% janitor::tabyl(estudios_madre.ff, parto_cesarea.f) %>%
       janitor::adorn_percentages() %>%
       gt::gt()


#- VARIABLES CATEGÓRICAS -------------------------------------------------------
#- no pasa nada si no lo entendéis del todo (siempre se puede hacer "a mano")
df %>% select(where(is.factor)) %>% names()     #- se nos escapan las q son character.

vv_numericas <- df %>% select(where(is.numeric)) %>% names()
vv_numericas

zz <- df %>% select(!vv_numericas)         #- deprecated but funciona
names(zz)
zz <- df %>% select(!all_of(vv_numericas)) #- forma correcta


#- porcentajes
inspectdf::inspect_cat(zz) %>% inspectdf::show_plot(high_cardinality = 1)

#- graficos de barras
DataExplorer::plot_bar(zz)
DataExplorer::plot_bar(zz, by = "sexo_bebe.f")
DataExplorer::plot_bar(zz, by = "parto_cesarea.f")
DataExplorer::plot_bar(zz, by = "estudios_madre.ff")



#- TABLAS ----------------------------------------------------------------------
summarytools::freq(df$sexo_bebe.f, style = "rmarkdown")

#- cross-tabulations
summarytools::ctable(df$sexo_bebe.f, df$parto_cesarea.f)
summarytools::ctable(df$sexo_bebe.f, df$parto_cesarea.f, chisq = TRUE)


#- JANITOR
df %>% janitor::tabyl(parto_cesarea.f) %>% gt::gt()

df %>% janitor::tabyl(parto_cesarea.f, estudios_madre.ff)

df %>% janitor::tabyl(parto_cesarea.f, estudios_madre.ff) %>% janitor::adorn_percentages()

#- ademas, como janitor almacena las tablas en df, podemos usar gt()
df %>% janitor::tabyl(parto_cesarea.f, estudios_madre.ff) %>% gt::gt()


#- CONTRASTES ------------------------------------------------------------------
t.test(df$peso_bebe, mu = 3250)
t.test(df$peso_bebe ~ df$parto_cesarea.f)


#- MODELOS ---------------------------------------------------------------------
df_m <- df %>% select(peso_bebe, parto_nn_semanas, sexo_bebe.f, parto_cesarea.f, edad_madre.1, estudios_madre.ff)

#- frente a todas las v. del df
mod_1 <- lm(peso_bebe ~ . , data = df_m)
summary(mod_1)


mod_1 <- lm(peso_bebe ~ parto_nn_semanas + edad_madre.1 + sexo_bebe.f , data = df_m)

summary(mod_1)                   #- tabla resumen

#- varios modelos ----
m1 <- lm(peso_bebe ~ parto_nn_semanas + sexo_bebe.f + edad_madre.1 , data = df)
m2 <- lm(peso_bebe ~ parto_nn_semanas + sexo_bebe.f + edad_madre.1 + estudios_madre.ff , data = df)

sjPlot::tab_model(m1)

sjPlot::plot_model(m1, sort.est = TRUE)

sjPlot::tab_model(m1, m2, show.ci	= FALSE)


#- mas tablas de modelos
stargazer::stargazer(m1, type = "text")  #- latex. html

stargazer::stargazer(m1, m2,  type = "text")  #- latex. html


library(apaTables) #- install.packages("apaTables",dep = TRUE)
apaTables::apa.reg.table(m1)

apaTables::
#- en el tutorial de tablas vimos más posibilidades

