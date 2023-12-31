#elo_calculation.R
#This computes the ELO rnaking for MBB using torvik data.

#library(sportsdataverse)
#library(gamezoneR)
library(tidyverse)
#library(jsonlite)
#library(cli)
#library(withr)
library(vroom)
library(fs)

season <- 2023
#Pull the team/conference combo for a given year from Torvik box scores.
mbb_conferences_torvik <- function(season){
  df_torvik <- vroom(paste0("C:/Users/ckoho/Documents/Inputs/NCAA/torvik_box_score_", 
                            season, ".csv"), altrep = FALSE)
  df_team1 <- df_torvik %>% 
    select(team1, team1_conf) %>%
    distinct() %>%
    rename("team" = "team1",
           "conf" = "team1_conf")
  df_team2 <- df_torvik %>% 
    select(team2, team2_conf) %>%
    distinct() %>%
    rename("team" = "team2",
           "conf" = "team2_conf")
  df_teams <- bind_rows(df_team1, df_team2) %>%
    distinct() %>%
    arrange(conf)
  return(df_teams)
}







mbb_conferences_torvik_ratings <- function(season){
  df_torvik_eoy <- vroom(paste0(
    "C:/Users/ckoho/Documents/Inputs/NCAA/Torvik/mbb_elo_torvik_", 
    season, ".csv"), 
                         altrep = FALSE ) %>%
    select(team, elo)
  df_conferences <- vroom(paste0(
    "C:/Users/ckoho/Documents/Inputs/NCAA/torvik_conferences_teams_",
    season, ".csv"), altrep = FALSE) 
  df_conference_ratings <- df_conferences %>%
    left_join(df_torvik_eoy, "team") %>%
    group_by(conf) %>%
    summarize(elo = mean(elo))
  return(df_conference_ratings)
}








#Pull the team/conference combo for each year.
for (season in 2008:2023){
  df_teams <- mbb_conferences_torvik(season) 
  write_csv(df_teams, paste0("../../Inputs/NCAA/torvik_conferences_teams_",
                                 season, ".csv"))
}

#Pull the conference mean rating for each year.
for (season in 2008:2022){
  df_conf_rating <- mbb_conferences_torvik_ratings(season) 
  write_csv(df_conf_rating, paste0("../../Inputs/NCAA/torvik_conferences_rating_",
                             season, ".csv"))
}

df_final <- NULL
#Combine the team/conference combo for all years.
for (season in 2008:2024){
  df_teams <- mbb_conferences_torvik(season) 
  df_teams$season <- season
  if(season == 2016){
    df_teams <- df_teams %>%
      filter(!(team == "Mercer" & conf == "Amer")) %>%
      filter(!(team == "Samford" & conf == "SB")) %>%
      filter(!(team == "Troy" & conf == "SC")) %>%
      filter(!(team == "Tulane" & conf == "SC")) 
  }
  df_final <- df_final %>%
    bind_rows(df_teams)
}


df_final1 <- df_final %>%
  pivot_wider(names_from = season, values_from = conf) %>%
  as.data.frame() %>%
  arrange(team) %>%
  filter(is.na(`2008`)) 
write_csv(df_final1, "../../Inputs/NCAA/torvik_new_d1_teams.csv")

df_final %>%
  dplyr::group_by(team, season) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)
