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
#Data is av/ailable back to 2008.
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




espn_mbb_box_score <- function(game_id){
  summary_url <-
    "http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary?"
  
  ## Inputs
  ## game_id
  full_url <- paste0(summary_url,
                     "event=", game_id)
  res <- httr::RETRY("GET", full_url)
  x = httr::status_code(res)
  if(x != 200) stop("The API returned an error", call. = FALSE)
  
  
  tryCatch(
    expr = {
      resp <- res %>%
        httr::content(as = "text", encoding = "UTF-8")
      
      raw_play_df <- jsonlite::fromJSON(resp)
      season <- raw_play_df[['header']][['season']][['year']]
      season_type <- raw_play_df[['header']][['season']][['type']]
      homeAwayTeam1 = toupper(raw_play_df[['header']][['competitions']][['competitors']][[1]][['homeAway']][1])
      homeAwayTeam2 = toupper(raw_play_df[['header']][['competitions']][['competitors']][[1]][['homeAway']][2])
      homeTeamId = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['id']][1]
      awayTeamId = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['id']][2]
      homeTeamMascot = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['name']][1]
      awayTeamMascot = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['name']][2]
      homeTeamName = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['location']][1]
      awayTeamName = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['location']][2]
      
      homeTeamAbbrev = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['abbreviation']][1]
      awayTeamAbbrev = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['abbreviation']][2]
      
      homeAway1 = toupper(raw_play_df[['header']][['competitions']][['competitors']][[1]][['homeAway']][1])
      
      linescore0 = raw_play_df[["header"]][["competitions"]][["competitors"]][[1]][["linescores"]][[1]][["displayValue"]]
      firsthalf0 = raw_play_df[["header"]][["competitions"]][["competitors"]][[1]][["linescores"]][[1]][["displayValue"]][1]
      secondhalf0 = raw_play_df[["header"]][["competitions"]][["competitors"]][[1]][["linescores"]][[1]][["displayValue"]][2]
      score0 = raw_play_df[["header"]][["competitions"]][["competitors"]][[1]][["score"]][1]
      
      linescore1 = raw_play_df[["header"]][["competitions"]][["competitors"]][[1]][["linescores"]][[2]][["displayValue"]]
      firsthalf1 =  raw_play_df[["header"]][["competitions"]][["competitors"]][[1]][["linescores"]][[2]][["displayValue"]][1]
      secondhalf1 = raw_play_df[["header"]][["competitions"]][["competitors"]][[1]][["linescores"]][[2]][["displayValue"]][2]
      score1 = raw_play_df[["header"]][["competitions"]][["competitors"]][[1]][["score"]][2]
      
      detail = raw_play_df[["header"]][["competitions"]][["status"]][["type"]][["description"]]
      venue = raw_play_df[["gameInfo"]][["venue"]][["fullName"]]
      venueid = raw_play_df[["gameInfo"]][["venue"]][["id"]]
      city = raw_play_df[["gameInfo"]][["venue"]][["address"]][["city"]]
      state = raw_play_df[["gameInfo"]][["venue"]][["address"]][["state"]]
      neutralsite =  raw_play_df[["header"]][["competitions"]][["neutralSite"]]
      ref1 = raw_play_df[["gameInfo"]][["officials"]][["fullName"]][1]
      ref2 = raw_play_df[["gameInfo"]][["officials"]][["fullName"]][2]
      ref3 = raw_play_df[["gameInfo"]][["officials"]][["fullName"]][3]
      
      # if (homeAway1 == "home") {
      #
      #   homeTeamId = as.integer(raw_play_df[["header"]][["competitions"]][["competitors"]][[1]][['team']][['id']][1])
      #   homeTeamMascot = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['name']][1]
      #   homeTeamName = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['location']][1]
      #   homeTeamAbbrev = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['abbreviation']][1]
      #   homeTeamLogo = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['logos']][[1]][['href']][1]
      #   homeTeamLogoDark = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['logos']][[1]][['href']][2]
      #   homeTeamFullName = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][["displayName"]][1]
      #   homeTeamColor = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][["color"]][1]
      #   homeTeamAlternateColor = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][["alternateColor"]][1]
      #   homeTeamScore = as.integer(raw_play_df[['header']][['competitions']][['competitors']][[1]][['score']][1])
      #   homeTeamWinner = raw_play_df[['header']][['competitions']][['competitors']][[1]][['winner']][1]
      #   homeTeamRecord = raw_play_df[['header']][['competitions']][['competitors']][[1]][['record']][[1]][['summary']][1]
      #   awayTeamId = as.integer(raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['id']][2])
      #   awayTeamMascot = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['name']][2]
      #   awayTeamName = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['location']][2]
      #   awayTeamAbbrev = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['abbreviation']][2]
      #   awayTeamLogo = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['logos']][[2]][['href']][1]
      #   awayTeamLogoDark = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['logos']][[2]][['href']][2]
      #   awayTeamFullName = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][["displayName"]][2]
      #   awayTeamColor = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][["color"]][2]
      #   awayTeamAlternateColor = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][["alternateColor"]][2]
      #   awayTeamScore = as.integer(raw_play_df[['header']][['competitions']][['competitors']][[1]][['score']][2])
      #   awayTeamWinner = raw_play_df[['header']][['competitions']][['competitors']][[1]][['winner']][2]
      #   awayTeamRecord = raw_play_df[['header']][['competitions']][['competitors']][[1]][['record']][[1]][['summary']][2]
      #   id_vars <- data.frame(
      #     homeTeamId,
      #     homeTeamMascot,
      #     homeTeamName,
      #     homeTeamAbbrev,
      #     homeTeamLogo,
      #     homeTeamLogoDark,
      #     homeTeamFullName,
      #     homeTeamColor,
      #     homeTeamAlternateColor,
      #     homeTeamScore,
      #     homeTeamWinner,
      #     homeTeamRecord,
      #     awayTeamId,
      #     awayTeamMascot,
      #     awayTeamName,
      #     awayTeamAbbrev,
      #     awayTeamLogo,
      #     awayTeamLogoDark,
      #     awayTeamFullName,
      #     awayTeamColor,
      #     awayTeamAlternateColor,
      #     awayTeamScore,
      #     awayTeamWinner,
      #     awayTeamRecord
      #   )
      # } else {
      #
      #   awayTeamId = as.integer(raw_play_df[["header"]][["competitions"]][["competitors"]][[1]][['team']][['id']][1])
      #   awayTeamMascot = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['name']][1]
      #   awayTeamName = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['location']][1]
      #   awayTeamAbbrev = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['abbreviation']][1]
      #   awayTeamLogo = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['logos']][[1]][['href']][1]
      #   awayTeamLogoDark = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['logos']][[1]][['href']][2]
      #   awayTeamFullName = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][["displayName"]][1]
      #   awayTeamColor = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][["color"]][1]
      #   awayTeamAlternateColor = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][["alternateColor"]][1]
      #   awayTeamScore = as.integer(raw_play_df[['header']][['competitions']][['competitors']][[1]][['score']][1])
      #   awayTeamWinner = raw_play_df[['header']][['competitions']][['competitors']][[1]][['winner']][1]
      #   awayTeamRecord = raw_play_df[['header']][['competitions']][['competitors']][[1]][['record']][[1]][['summary']][1]
      #   homeTeamId = as.integer(raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['id']][2])
      #   homeTeamMascot = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['name']][2]
      #   homeTeamName = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['location']][2]
      #   homeTeamAbbrev = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['abbreviation']][2]
      #   homeTeamLogo = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['logos']][[2]][['href']][1]
      #   homeTeamLogoDark = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['logos']][[2]][['href']][2]
      #   homeTeamFullName = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][["displayName"]][2]
      #   homeTeamColor = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][["color"]][2]
      #   homeTeamAlternateColor = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][["alternateColor"]][2]
      #   homeTeamScore = as.integer(raw_play_df[['header']][['competitions']][['competitors']][[1]][['score']][2])
      #   homeTeamWinner = raw_play_df[['header']][['competitions']][['competitors']][[1]][['winner']][2]
      #   homeTeamRecord = raw_play_df[['header']][['competitions']][['competitors']][[1]][['record']][[1]][['summary']][2]
      #   id_vars <- data.frame(
      #     homeTeamId,
      #     homeTeamMascot,
      #     homeTeamName,
      #     homeTeamAbbrev,
      #     homeTeamLogo,
      #     homeTeamLogoDark,
      #     homeTeamFullName,
      #     homeTeamColor,
      #     homeTeamAlternateColor,
      #     homeTeamScore,
      #     homeTeamWinner,
      #     homeTeamRecord,
      #     awayTeamId,
      #     awayTeamMascot,
      #     awayTeamName,
      #     awayTeamAbbrev,
      #     awayTeamLogo,
      #     awayTeamLogoDark,
      #     awayTeamFullName,
      #     awayTeamColor,
      #     awayTeamAlternateColor,
      #     awayTeamScore,
      #     awayTeamWinner,
      #     awayTeamRecord
      #   )
      #
      # }
      
      game_date = as.Date(substr(raw_play_df[['header']][['competitions']][['date']], 0, 10))
      
      teams_box_score_df <-
        jsonlite::fromJSON(jsonlite::toJSON(raw_play_df[["boxscore"]][["teams"]]), flatten =
                             TRUE)
      
      teams_box_score_df_2 <- teams_box_score_df[[1]][[2]] %>%
        dplyr::select(
          "displayValue",
          "name") %>%
        dplyr::rename("Home" = "displayValue")
      teams_box_score_df_1 <- teams_box_score_df[[1]][[1]] %>%
        dplyr::select(
          "displayValue",
          "name") %>%
        dplyr::rename("Away" = "displayValue")
      teams2 <- data.frame(t(teams_box_score_df_2$Home))
      colnames(teams2) <- t(teams_box_score_df_2$name)
      
      teams1 <- data.frame(t(teams_box_score_df_1$Away))
      colnames(teams1) <- t(teams_box_score_df_1$name)
      teams1 <- teams1 %>%
        separate("fieldGoalsMade-fieldGoalsAttempted", c("team1_fgm", "team1_fga"))
      teams1 <- teams1 %>%
        separate("threePointFieldGoalsMade-threePointFieldGoalsAttempted", c("team1_tpm", "team1_tpa"))
              
      teams1 <- teams1 %>%
        separate("freeThrowsMade-freeThrowsAttempted", c("team1_ftm", "team1_fta"))
      teams1 <- teams1 %>%
        rename(team1_oreb = offensiveRebounds,
               team1_dreb = defensiveRebounds,
               team1_reb = totalRebounds,
               team1_ftp = freeThrowPct,
               team1_fgp = fieldGoalPct,
               team1_tpp = threePointFieldGoalPct,
               team1_ast = assists,
               team1_stl = steals,
               team1_blk = blocks,
               team1_to = turnovers,
               team1_teamto = teamTurnovers,
               team1_totalto = totalTurnovers,
               team1_tf = technicalFouls,
               team1_totaltf = totalTechnicalFouls,
               team1_flagrant = flagrantFouls,
               team1_fouls = fouls,
               team1_largestlead = largestLead,
               )
      teams2 <- teams2 %>%
        separate("fieldGoalsMade-fieldGoalsAttempted", c("team2_fgm", "team2_fga"))
      teams2 <- teams2 %>%
        separate("threePointFieldGoalsMade-threePointFieldGoalsAttempted", c("team2_tpm", "team2_tpa"))
      teams2 <- teams2 %>%
        separate("freeThrowsMade-freeThrowsAttempted", c("team2_ftm", "team2_fta"))
      teams2 <- teams2 %>%
        rename(team2_oreb = offensiveRebounds,
               team2_dreb = defensiveRebounds,
               team2_reb = totalRebounds,
               team2_ftp = freeThrowPct,
               team2_fgp = fieldGoalPct,
               team2_tpp = threePointFieldGoalPct,
               team2_ast = assists,
               team2_stl = steals,
               team2_blk = blocks,
               team2_to = turnovers,
               team2_teamto = teamTurnovers,
               team2_totalto = totalTurnovers,
               team2_tf = technicalFouls,
               team2_totaltf = totalTechnicalFouls,
               team2_flagrant = flagrantFouls,
               team2_fouls = fouls,
               team2_largestlead = largestLead,
        )      
      
      df_teams_box_score <- dplyr::bind_cols(teams1, teams2)
      df_teams_box_score <- df_teams_box_score %>%
        mutate(team1 = homeTeamName,
               team2 = awayTeamName,
               team1_home_away = homeAwayTeam1,
               team2_home_away <- homeAwayTeam2,
               team2_id = as.integer(homeTeamId),
               team2_mascot = homeTeamMascot,
               team2_abbrev = homeTeamAbbrev,
               team1_mascot = awayTeamMascot,
               team1_abbrev = awayTeamAbbrev,
               ref1 = ref1,
               ref2 = ref2,
               ref3 = ref3,
               game_id = game_id,
               season = season,
               season_type = season_type,
               game_date = game_date,
               team1_linescore = paste(linescore0,collapse=","),
               team1_firsthalf = firsthalf0,
               team1_secondhalf = secondhalf0,
               team1_score = score0,
               team2_linescore = paste(linescore1,collapse=","),
               team2_firsthalf = firsthalf1,
               team2_secondhalf = secondhalf1,
               team2_score = score1,
               detail = detail,
               venue = venue,
               venueid = venueid,
               city = city,
               state = state,
               neutralsite = neutralsite
               ) %>%
        janitor::clean_names() %>%
        dplyr::select(
          "game_id",
          "season",
          "season_type",
          "game_date",
          "neutralsite",
          starts_with("team1_"),
          starts_with("team2_"),
          tidyr::everything()) 
    },
    error = function(e) {
      message(
        glue::glue(
          "{Sys.time()}: Invalid arguments or no team box score data for {game_id} available!"
        )
      )
    },
    warning = function(w) {
      
    },
    finally = {
      
    }
  )
  return(team_box_score)
}
  
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


#Loads each available game id,
espn_mbb_game_id_season <- function(season){
df_gi <- hoopR::load_mbb_team_box(season) %>%
  select(game_id) %>%
  group_by(game_id) %>%
  summarise(n = n()) %>%
  select(game_id)
}


#Loads ESPN betting information. Only available post 2013. 




write_csv(df_line, paste0("espn_line_information_", season, ".csv"))
### End ESPN game_id



for (season in 2008:2022){
  #First we get all the boxscore data for each available season.
  df <- bart_expanded_box_score(season)
  df <- df %>%
    mutate(win = str_trim(win),
           loss = str_trim(loss),
           team2 = str_trim(team2),
           team1 = str_trim(team1))
  write_csv(df, paste0("torvik_box_score_", season, ".csv"))
  
  #ESPN betting lines. Loading each espn available game id and pulling the lines
  #for each available game. Only available post 2013
  df_gi <- hoopR::load_mbb_team_box(season) %>%
   select(game_id) %>%
     group_by(game_id) %>%
     summarise(n = n()) %>%
     select(game_id)
   df_line <- NULL
   for (i in 1:nrow(df_gi)){
     df_line <- df_line %>%
       bind_rows(try(espn_mbb_lines(df_gi[[i,1]])))
   }
   write_csv(df_line, paste0("espn_line_information_", season, ".csv"))
  
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

