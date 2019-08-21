## Join qualtrics and spotify data 

library(tidyverse)
library(naniar)

spotify2 <- read_csv("data/s2_clean.csv") %>% 
  dplyr::select(track_name:key_mode, ResponseId)

qual2 <- read_csv("data/QualtricsRAW.csv")

s2_spotify_join <- full_join(qual2, spotify2, by = "ResponseId")  %>% # join data
  dplyr::distinct(ProlificID, .keep_all = TRUE) %>% #remove duplicate participants 
  dplyr::filter(str_detect(StartDate, '^2018')) %>% #remove missing columns that don't contain data added in export
  naniar::replace_with_na(replace = list(ProlificID =c("-99", "test"))) %>% # recode variables where I was testing study
  tidyr::drop_na(ProlificID) %>% #drop NA columns just created
  dplyr::select(ProlificID, track_name, sad.song, artist_name, sad.song.artist, everything()) #reorder columns for manual inspection of matching


write_csv(s2_spotify_join,
          path = "data/s2_spotify_join.csv") #visual test complete, songs match 
