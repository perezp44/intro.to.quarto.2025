#- eurovision_11  -----
#- https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-05-17/readme.md
#- https://gist.github.com/juliasilge/c5e76731e8dc4e2b709f5afd4ebf9b61

library(tidyverse)
library(widyr) #- (irlba, widyr)
library(maps)

#- cargamos datos -----
eurovision_votes <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision-votes.csv')

#- clustering -----
set.seed(234)
eurovision_clusters <-  eurovision_votes %>%
  # dimensionality reduction with SVD: https://www.youtube.com/watch?v=UyAfmAZU_WI
  widyr::widely_svd(from_country, to_country, points, nv = 24) %>%
  #- combination of SVD and k-means: https://www.researchgate.net/publication/284617182_Combination_of_Singular_Value_Decomposition_and_K-means_Clustering_Methods_for_Topic_Detection_on_Twitter
  widyr::widely_kmeans(from_country, dimension, value, k = 4)

#- tabla -----
table <- eurovision_clusters %>% 
  group_by(cluster) %>% 
  summarise(from_country = paste(from_country, collapse = ", ")) 
gt::gt(table)


#- mapa -----
map_data("world") %>%
  filter(region %in% eurovision_clusters$from_country) %>%
  left_join(eurovision_clusters, by = c("region" = "from_country")) %>%
  ggplot(aes(long, lat, group = group, fill = cluster)) +
  geom_polygon(alpha = 0.8) +
  coord_map() +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Which countries vote similarly for Eurovision?",
       subtitle = "Clusters identified with SVD and K-means",
       caption = "Big thanks to @juliasilge@fosstodon.org") +
  pjpv.curso.R.2022::theme_pjp_maps() +
  theme(panel.background = ggplot2::element_rect(fill = "white",  color = NA)) +
  theme(plot.background = ggplot2::element_rect(colour = "white",fill = "white"))

#- otro análisis con esos datos: https://twitter.com/geokaramanis/status/1527271169447014403

