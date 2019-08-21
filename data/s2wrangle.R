## Join qualtrics and spotify data 

library(tidyverse)

spotify2 <- read_csv("data/s2_clean.csv") %>% 
  dplyr::select(track_name:key_mode, ResponseId)

qual2 <- read_csv("data/QualtricsRAW.csv")
names(qual2)

s2_spotify_join <- full_join(qual2, spotify2, by = "ResponseId")

names(s2_spotify_join)

s2_spotify_join %>% 
  dplyr::select(ResponseId, 
                contains("track_name"),
                contains ("artist"),
                sad.song,
                valence) %>%
  view()


write_csv(s2_spotify_join, 
          path = "data/s2_spotify_join.csv")
