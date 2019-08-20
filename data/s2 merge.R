library(tidyverse)

s2qual <- read_csv("data/s2_raw.csv") %>% 
  filter (!is.na(ProlificID))
names(s2qual)

s2spotify <- read_csv("data/s2_Spotify_wide.csv") %>% 
  dplyr::select(ResponseID, 
         playlist_name:key_mode)

s2merged <- dplyr::full_join(s2qual, s2spotify, by = "ResponseID")

s2merged %>% 
  dplyr::select(ProlificID,
                sad.song.artist,
                artist_name,
                sad.song,
                track_name) %>% 
  view()

#export to csv for manual processing 

write_csv(s2merged, "data/s2ForCleaning.csv")
