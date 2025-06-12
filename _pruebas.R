#- probando
library(tidyverse)

aa <- pjpv.datos.2024::atlas_exp_renta_2015_2021

bb <- aa %>% filter(ine_muni.n == "Pancrudo")
bb <- aa %>% filter(year == 2021) %>% mutate(year = as.character(year)) %>% 
  select(-geo) %>% 
  filter(is.na(ine_distrito))



#- 
delitos <- pjpv.datos.2024::delitos_boletin_muni_2015_22
cc <- delitos %>% filter(year == 2021)


df <- left_join(cc, bb)

names(df)


zz <- df %>% select(1:15, poblacion, pob_total)


zz <- zz %>% mutate(dif = poblacion - pob_total)
