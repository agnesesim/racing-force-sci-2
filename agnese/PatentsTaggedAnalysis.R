library(udpipe)
library(tidyverse)
library(rebus)
library(igraph)

patents <- read_csv("_data/patents_tags.csv")

patents <- patents %>%
  filter(tags != 'offtopic')


tags_monitoring = c("road monitoring", "vehicle classification", "speed & trajectory", "gps", "survillance")
tags_safety = c("pilot monitoring", "anomaly detection", "safety", "insurance", "damage")
tags_sport = c("gaming", "simulation", "esport", "sport")
tags_misc = c("accessories", "quantum computing", "image analysis", "generico")

patents <- patents %>%
  mutate(cluster = case_when(tags %in% tags_monitoring ~ 'monitoring', 
                                 tags %in% tags_safety ~ 'safety', 
                                 tags %in% tags_sport ~ 'sport', 
                                 tags %in% tags_misc ~ 'misc')
         )
  
patents %>%
  count(tags,cluster) %>%
  ggplot(aes(tags, n, fill=cluster)) +
  geom_col() + 
  scale_fill_brewer(palette="YlOrRd") +
  theme_bw()

  
monitoring <- patents %>% 
  filter(tags %in% tags_monitoring) 

safety <- patents %>% 
  filter(tags %in% tags_safety)

sport <- patents %>%
  filter(tags %in% tags_sport)

misc <- patents %>%
  filter(tags %in% tags_misc)



