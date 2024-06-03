#pbp_stats_import.R
#This file breaks down STATS play by play data.

library(future)
library(progressr)
library(gamezoneR)
library(tidyverse)

#library(sportsdataverse)
#library(jsonlite)
#library(cli)
#library(withr)
#library(readr)
#library(vroom)
#library(fs)
#
#future::plan("multisession")
#progressr::with_progress({
#  pbp <- gamezoneR::load_gamezone_pbp(gamezoneR:::available_seasons())
#})


#Track made basket, turnover, free throw miss/make, start of half, possession length.
seasons <- gamezoneR:::available_seasons()
future::plan("multisession")
season <- seasons[15]
season <- seasons[3] #Not good

for(season in seasons){
  df_pbp <- gamezoneR::load_gamezone_pbp(season) %>%
    as_tibble()
  #Writing for loops now, may change to purr in the future. 
  df_pbp <- df_pbp %>%
      mutate(
        new_possession = case_when(
          poss_before == poss_after ~ "no_change",
          is.na(poss_before) & is.na(poss_after) ~ "no_possession",
          is.na(poss_before) & !is.na(poss_after) ~ "half_start",
          TRUE ~ "new_possession"
        )
      ) 
  #Possession_result: turnover, missed shot, made_shot, start of half, others
  df_pbp <- df_pbp %>%
      mutate(
        possession_result = case_when(
          shot_outcome == "made" & poss_before != poss_after ~ "made_shot",
          shot_outcome == "missed" & poss_before != poss_after ~ "missed_shot",
          poss_before == poss_after ~ "no_change",
          str_detect(desc, "Start of") ~ "half_start", 
          new_possession == "new_possession" & 
            str_detect(desc, "defensive rebound") ~ "missed_shot",
          new_possession == "new_possession" & 
            str_detect(desc, "turnover") ~ "turnover",
          new_possession == "new_possession" & 
            str_detect(desc, "steal") ~ "steal",
          new_possession == "new_possession" & 
            str_detect(desc, "Offensive foul") ~ "offensive_foul",
          str_detect(desc, "Substitution:") ~ "substitution",
          shot_outcome == "missed" & new_possession == "new_possession" ~
            "missed_shot",
          str_detect(desc, "Starting Lineup") ~ "starting_lineup",
          new_possession == "new_possession" & 
            str_detect(desc, "Technical foul") ~ "tech_foul",
          new_possession == "new_possession" & 
            str_detect(lead(desc), "End of") ~ "end_of_half",
          new_possession == "new_possession" & 
            str_detect(desc, "offensive rebound") ~ "offensive_reb",
          new_possession == "new_possession" & 
            str_detect(desc, "Flagrant foul") ~ "flag_foul",
          new_possession == "new_possession"  ~ "unexpected",
          TRUE ~ "other"
          )
        )
  if (season == "2021-22"){
    df_pbp <- df_pbp %>%
    filter(possession_result != "unexpected")
  }
  df_pbp%>%
    group_by(possession_result) %>%
    tally()
  df_pbp_unexpected <- df_pbp%>%
    filter(possession_result == "unexpected")
  df_pbp_game_tmp <- df_pbp %>%
    filter(game_id == 2440510)
  write_csv(df_pbp, paste0("stats_pbp_", season, ".csv"))

  }
