#pbp_tracker.R
#This file breaks down the play by play data.

#library(future)
library(progressr)
#library(gamezoneR)
library(tidyverse)

library(sportsdataverse)
#library(jsonlite)
#library(cli)
#library(withr)
#library(readr)
#library(vroom)
#library(fs)



progressr::with_progress({
  mbb_pbp <-  hoopR::load_mbb_pbp()
})

seasons <- gamezoneR:::available_seasons()
seasons <- seasons 
future::plan("multisession")
season <- seasons[15]
#No turnover traking through 2011-12

for(season in seasons){
  pbp <- gamezoneR::load_gamezone_pbp(season) %>%
    as_tibble()
  #Writing for loops now, may change to purr in the future. 
  gameid <- unique(pbp$game_id)
  game <- gameid[1]
  for (game in gameid){
    df_game_pbp <- pbp %>%
      filter(game_id == game)
    if(season == "2011-12"){
      
    }
      
  }
  

}