#data_import.R
#This file handles the importing and cleaning of data.

library(sportsdataverse)
library(gamezoneR)
library(tidyverse)
library(jsonlite)
library(cli)
library(withr)
library(readr)
library(vroom)
library(fs)

#FUNCTIONS
#Box SCore data. For time being I am using my own made script. toRvik package
#may add this functionality later. Script is called bart_expanded_box_score(). 
#Data is aailable back to 2008.
bart_expanded_box_score<- function(year = current_season()) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
          2008)) {
      cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2008!")
    } else {
      box_score_duplicates <- c(
        "fgm", "fga",  "tpm", "tpa", "ftm", "fta", "oreb", "dreb", "reb", "ast", "stl", "blk", "to", "pf", "pts")
      gf_specifc <- c(
        "result", "adj_o", "adj_d")
      gf_duplicates <-  c(
        "ppp", "efg", "to_per", "or_per", "ftr")
      gf_other <- c( 
        "game_score", "opp_conf", "season", "tempo", "game_id", "coach", "opp_coach", "avg_marg", "opp_avg_marg")
      x1_unique <- c(
        "date", "type", "team1", "team1_conf", "team2", "loc", "team1_adj_o", "team1_adj_d", "team1_game_score", "team2_conf", "season", "tempo", "game_id", "team1_coach", "team2_coach", "team1_avg_margin", "team1_opp_avg_margin")
      x2_unique <- c(
        "adj_o", "adj_d", "game_score", "game_id", "avg_margin", "opp_avg_margin")
      x <- jsonlite::fromJSON(paste0("https://barttorvik.com/getgamestats.php?year=", year))
      x <- dplyr::as_tibble(x)
      x <- as_tibble(lapply(x, function(y) {
        gsub('\"', "", y)
      }))
      x1 <- x %>%
        dplyr::filter(V22 == 1) %>%
        dplyr::select(-c(7, 22, 31)) %>%
        dplyr::rename_at(c(1:8, 19:27), ~ x1_unique) %>%
        dplyr::rename_at(9:13, ~ paste0("team1_", gf_duplicates)) %>%
        dplyr::rename_at(14:18, ~ paste0("team2_", gf_duplicates)) %>%
        tidyr:: separate("V30", c("date" , "min", "team1", "team2",  
                                  paste0("team1_", box_score_duplicates), 
                                  paste0("team2_", box_score_duplicates), 
                                  "pos"), ",") 
      x1 <- x1 %>%
        mutate(win = "",
               loss = "")

      x2 <- x %>%
        dplyr::filter(V22 == 2) %>%
        dplyr::select(c(8, 9, 20, 25, 28, 29)) %>%
        dplyr::rename_all(~paste0("team2_", x2_unique)) %>%
        dplyr::rename_at(4, ~"game_id")
      x <- x1 %>%
        dplyr::full_join(x2, by = "game_id") %>%
        dplyr::mutate(
          date = lubridate::mdy(date),
          type = dplyr::case_when(
            type == 0 ~ "nc",
            type == 1 ~ "conf",
            type == 2 ~ "conf_t",
            type == 3 ~ "post",
            TRUE ~ "nond1"
          ),
          across(c(4:16, 18:19, 23:24, 26, 29:59,62:66), as.numeric),
          loss = stringr::str_remove_all(loss, "]")
        ) %>%
        dplyr::relocate(dplyr::starts_with("team1"), .after="loss") %>%
        dplyr::relocate(dplyr::starts_with("team2"), .after="team1_pts") %>%
        dplyr::arrange(date)
      x <- x %>%
        mutate(win = if_else(team1_pts > team2_pts, team1, team2),
               loss = if_else(team1_pts > team2_pts, team2, team1),)
      
    }
  })}

#espn_mbb_lines pulls the lines for each game_id provided. Strips the 
#unnecessary information and only pulls the consensus line for simplicity.
espn_mbb_lines <- function(game_id){
  df <- NULL
  try({
    df <- hoopR::espn_mbb_team_box(game_id) %>% 
      top_n(1) %>%
      select(game_id, season, game_date, team_short_display_name, team_name, 
             team_id, team_abbreviation, opponent_id, opponent_name, 
             opponent_abbrev)
    list_lines <- hoopR::espn_mbb_betting(game_id)
    df1 <- list_lines[["pickcenter"]] %>%
      filter(provider_name == "consensus") %>% 
      select(details, over_under, spread,provider_id, provider_name,
             away_team_odds_favorite, away_team_odds_underdog, 
             away_team_odds_money_line, away_team_odds_spread_odds, 
             away_team_odds_team_id, home_team_odds_favorite, 
             home_team_odds_underdog, home_team_odds_money_line, 
             home_team_odds_spread_odds, home_team_odds_team_id
      )
    df <- df %>%
      bind_cols(df1)
  })
  return(df)
} 

for (season in 2008:2022){
  #First we get all the boxscore data for each available season.
  df <- bart_expanded_box_score(season)
  df <- df %>%
    mutate(win = str_trim(win),
           loss = str_trim(loss),
           team2 = str_trim(team2),
           team1 = str_trim(team1))
  write_csv(df, paste0("torvik_box_score_", season, ".csv"))
  
  # #ESPN betting lines. Loading each espn available game id and pulling the lines
  # #for each available game. Only available post 2013
  # df_gi <- hoopR::load_mbb_team_box(season) %>%
  #   select(game_id) %>%
  #   group_by(game_id) %>%
  #   summarise(n = n()) %>%
  #   select(game_id)
  # df_line <- NULL
  # for (i in 1:nrow(df_gi)){
  #   df_line <- df_line %>%
  #     bind_rows(try(espn_mbb_lines(df_gi[[i,1]])))
  # }
  # write_csv(df_line, paste0("espn_line_information_", season, ".csv"))
  
  # #Joins the total torvik box score and the ESPN line information into one data 
  # #frame. Torvik game_id changes in 2015 so need to join with different keys 
  # #after that. Work in progress!!1
  # if (season < 2013){
  #   #No betting lines this far back.
  #   df <- vroom(paste0("torvik_box_score_", season, ".csv"), altrep = FALSE)
  #   write_csv(df,paste0("box_score_betting_", season, ".csv"), altrep = FALSE)
  # }else if(season < 2015){
  #   df_torvik <- vroom(paste0("torvik_box_score_", season, ".csv"), 
  #                      altrep = FALS)
  #   df_espn <- vroom(paste0("espn_line_information_", season, ".csv"), 
  #                    altrep = FALSE)
  #   df <- df_torvik %>%
  #     left_join(df_espn, by = "game_id")
  #   write_csv(df,paste0("box_score_betting_", season, ".csv"))
  # }
  # else{
  #   df_torvik <- vroom(paste0("torvik_box_score_", season, ".csv"), 
  #                      altrep = FALSE)
  #   df_espn <- vroom(paste0("espn_line_information_", season, ".csv"), 
  #                    altrep = FALSE)
  # }
  
}


for (season in 2008:2022){

}

