#- ejercicio_00  -----
#- Thom-Ivar van Dijk hizo un gráfico chulísimo sobre la curva de Phillips
#- https://twitter.com/ThomIvar/status/1516114838996631563
#- source: https://github.com/TIvanDijk/pRojects/blob/main/30DayChartChallenge/oecd.R
#- Vamos a replicar su  gráfico (con datos actualizados y para más países)
#- y haremos alguna tabla
#- luego usaremos este código para hacer informes/slides y una web con QMD

library(tidyverse)
library(ggtext)

#- Datos (de la OCDE) ----------------------------------------------------------
#- Inflación: https://data.oecd.org/price/inflation-cpi.htm 
url_inflation <- "https://stats.oecd.org/sdmx-json/data/DP_LIVE/.CPI.../OECD?contentType=csv&detail=code&separator=comma&csv-lang=en"
ruta_inflation <- "./datos/DP_LIVE_22032023113248591.csv"
# curl::curl_download(url_inflation, ruta_inflation)
# download.file(url_inflation, ruta_inflation)
inflation_orig <- readr::read_csv(ruta_inflation)
inflation_dicc <- pjpv.curso.R.2022::pjp_dicc(inflation_orig)

#- Desempleo: https://data.oecd.org/unemp/unemployment-rate.htm
url_unemployment <- "https://stats.oecd.org/sdmx-json/data/DP_LIVE/.HUR.../OECD?contentType=csv&detail=code&separator=comma&csv-lang=en"
ruta_unemployment <- "./datos/DP_LIVE_12042023090537614.csv"
# curl::curl_download(url_unemployment, ruta_unemployment)
unemployment_orig <- readr::read_csv(ruta_unemployment)
unemployment_dicc <- pjpv.curso.R.2022::pjp_dicc(unemployment_orig)

#- seleccionamos datos ---------------------------------------------------------
inflation <- inflation_orig %>%
  dplyr::filter(FREQUENCY == "M" & MEASURE == "AGRWTH", SUBJECT == "TOT") %>%
  select(place = LOCATION, date = TIME, cpi = Value)

unemployment <- unemployment_orig %>%
  dplyr::filter(FREQUENCY == 'M', SUBJECT == 'TOT') %>%
  select(place = LOCATION, date = TIME, rate = Value)

#- fusionamos los 2 df's -------------------------------------------------------
df <- left_join(inflation, unemployment) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(group = case_when(
    str_starts(date, "199") ~ "1991-1999",
    str_starts(date, "200") ~ "2000-2009",
    str_starts(date, "201") ~ "2010-2019",
    str_starts(date, "202") ~ ">= 2020",
    TRUE ~ "other" )) %>%
  dplyr::filter(group != "other")

#- plot nº 1 (1 país) ----------------------------------------------------------
#- plot con datos de 1 país (G-7) y 2 periodos (2000-2019)
my_country <- c("ESP")
my_country <- c("G-7")  #- seleccionamos "un país"
my_periodos <- c("2000-2009", "2010-2019") #- seleccionamos periodos
my_title <- glue::glue("What happened to the <b>Phillips Curve</b> in ", my_country  , "? (thanks to <b style='color:#fd5532'>@ThomIvar</b>)")



df_1p <- df %>%
  dplyr::filter(place == my_country) %>%
  dplyr::filter(group %in% my_periodos)


# -- make plot for 1 country
p1 <- ggplot(df_1p, aes(x = rate, y = cpi, group = group, color = group)) +
  geom_point(alpha = 0.5, size = 2.25)

p1a <- p1 + geom_smooth(method = 'lm', se = FALSE, linetype = 2) +
  scale_color_manual(values = c('#d1495b', '#00798c')) +
  annotate('text', x = 5.5, y = 3.75, label = '2000-2009', color = '#d1495b',
           family = 'American Typewriter', fontface = 'bold') +
  annotate('text', x = 4.5, y = 0.75, label = '2010-2019', color = '#00798c',
           family = 'American Typewriter', fontface = 'bold') +
  labs(title = my_title,
       subtitle = "The Phillips curve is an economic concept that states that inflation
       and unemployment have an inverse relationship. However, in the <b style='color:#00798c'>
       last decade</b> this relationship appears to have vanished.",
       caption = '@ThomIvar • source: OECD (monthly, G7 countries)',
       x = 'Unemployment rate, %',
       y = 'Inflation rate, %') +
  theme_void() +
  theme(text = element_text('American Typewriter', color = 'grey30'),
        legend.position = 'none',
        plot.title = element_textbox_simple(size = 20, margin = margin(b = 0.15, unit = 'cm')),
        plot.subtitle = element_textbox_simple(size = 11, color = 'grey60',
                                               margin = margin(b = 0.25, unit = 'cm')),
        plot.background = element_rect(fill = '#fffef7', color = '#fffef7'),
        plot.caption = element_text(size = 10, color = 'grey80'),
        plot.margin = margin(1, 1, 1, 1, 'cm'),
        panel.grid = element_line(color = 'grey70', linetype = 'dotted'),
        axis.title = element_text(margin = margin(t = 0.2, r = 0.2, unit = 'cm'), color = 'grey50'),
        axis.title.y = element_text(angle = 90),
        axis.text = element_text(color = 'grey70', size = 9,
                                 margin = margin(t = 0.1, r = 0.1, unit = 'cm')),
        axis.line = element_line(color = 'grey50'),
        axis.ticks = element_line(color = 'grey50', size = 0.6),
        axis.ticks.length = unit(0.10, 'cm'))

p1a
#ggsave("./imagenes/oecd.png", width = 9.5, height = 6)



#- plot nº 2 (varios países) ---------------------------------------------------
#- hacemos el gráfico para varios países 
my_paises <- c("ESP", "FRA", "GBR", "DEU", "USA", "JPN")
my_periodos <- c("2000-2009", "2010-2019") 

df_paises <- df %>%  
  dplyr::filter(place %in% my_paises) %>% 
  dplyr::filter(group %in% my_periodos)

#- plot feo para luego hacer un facetting
p2 <- ggplot(df_paises, aes(x = rate, y = cpi, color = group)) +
  geom_point(alpha = 0.5, size = 0.75) +
  geom_smooth(method = 'lm', se = FALSE, linetype = 1) +
  scale_color_manual(values = c(  '#d1495b', '#00798c', '#099205','#8e0592'))


#p2 + facet_wrap(vars(place))

p2a <- p2 +
  facet_wrap(vars(place), scales = "free")  +
  labs(title = "What happened to the <b>Phillips Curve</b>? ",
       subtitle = "The Phillips curve is an economic concept that states that inflation
       and unemployment have a inverse relationship. However, we could find some exceptions for <b style='color:#d1495b'>
       2000-2009</b> decade, and for the  <b style='color:#00798c'>
       last decade (2010-2019) </b>.",
       caption = "@pjpv4444  (thanks to  @ThomIvar) • source: OECD (monthly, G7 countries)",
       x = 'Unemployment rate, %',
       y = 'Inflation rate, %') +
  theme_void() +
  theme(text = element_text('American Typewriter', color = 'grey30'),
        legend.position = 'none',
        plot.title = element_textbox_simple(size = 20, margin = margin(b = 0.15, unit = 'cm')),
        plot.subtitle = element_textbox_simple(size = 11, color = 'grey60',
                                               margin = margin(b = 0.25, unit = 'cm')),
        plot.background = element_rect(fill = '#fffef7', color = '#fffef7'),
        plot.caption = element_text(size = 10, color = 'grey80'),
        plot.margin = margin(1, 1, 1, 1, 'cm'),
        panel.grid = element_line(color = 'grey70', linetype = 'dotted'),
        axis.title = element_text(margin = margin(t = 0.2, r = 0.2, unit = 'cm'), color = 'grey50'),
        axis.title.y = element_text(angle = 90),
        axis.text = element_text(color = 'grey70', size = 9,
                                 margin = margin(t = 0.1, r = 0.1, unit = 'cm')),
        axis.line = element_line(color = 'grey50'),
        axis.ticks = element_line(color = 'grey50', linewidth = 0.6),
        axis.ticks.length = unit(0.10, 'cm'))


p2a

#- algunas tablas --------------------------------------------------------------
