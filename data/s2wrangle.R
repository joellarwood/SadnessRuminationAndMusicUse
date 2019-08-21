## Join qualtrics and spotify data 


spotify2 <- read_csv("data/s2_clean.csv") %>% 
 dplyr:filter(remove == 0) %>% 
  dplyr::select(remove:key_mode, ResponseId, ProlficID)

names(spotify2)

qual2 <- read_csv("data/QualtricsRAW.csv")
names(qual2)

s2_spotify_join <- full_join(qual2, spotify2, by = "ResponseId")

names(s2_spotify_join)

s2_spotify_join %>% 
  dplyr::select(ProlificID.x,
         ProlificID.y, 
         ResponseId, 
         contains("track_name"),
         contains ("artist"),
         sad.song,
         valence) %>%
  view()
