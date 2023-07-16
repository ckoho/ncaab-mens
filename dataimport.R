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
#Data is available back to 2008.
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

#game_id <- 401252483
#game_id <- 273090235
espn_mbb_lines <-  function(game_id){
  print(game_id)
  summary_url <-
    "http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary?"
  
  ## Inputs
  ## game_id
  full_url <- paste0(summary_url,
                     "event=", game_id)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  x = httr::status_code(res)
  if(x != 200) return(tibble())
  df_pickcenter <- tibble()
  tryCatch(
    expr = {
      resp <- res %>%
        httr::content(as = "text", encoding = "UTF-8")
      
      raw_summary <- jsonlite::fromJSON(resp)
      if ("pickcenter" %in% names(raw_summary)) {
        df_pickcenter <-
          jsonlite::fromJSON(jsonlite::toJSON(raw_summary$pickcenter), flatten =
                               TRUE) %>%
          janitor::clean_names() %>%
          dplyr::select(-"links",
                        -"away_team_odds_win_percentage", 
                        -"away_team_odds_average_score",
                        -"away_team_odds_spread_record_wins",
                        -"away_team_odds_spread_record_losses",
                        -"away_team_odds_spread_record_pushes",
                        -"away_team_odds_spread_record_summary",
                        -"home_team_odds_win_percentage",
                        -"home_team_odds_average_score",
                        -"home_team_odds_spread_record_wins",
                        -"home_team_odds_spread_record_losses",
                        -"home_team_odds_spread_record_pushes",
                        -"home_team_odds_spread_record_summary"
          )
        
        
      }
    },
    error = function(e) {
      message(glue::glue(
        "{Sys.time()}: Invalid arguments or no betting data available!"
      ))
    },
    warning = function(w) {
      
    },
    finally = {
      
    }
    
  )
  if ("pickcenter" %in% names(raw_summary)) {
    df_name <- jsonlite::fromJSON(jsonlite::toJSON(raw_summary$boxscore$teams), flatten =
                                    TRUE)
    team1id = toupper(df_name[['team.id']][[1]])
    team2id = toupper(df_name[['team.id']][[2]])
    team1name = toupper(df_name[['team.location']][[1]])
    team2name = toupper(df_name[['team.location']][[2]])
    team1sname = toupper(df_name[['team.shortDisplayName']][[1]])
    team2sname = toupper(df_name[['team.shortDisplayName']][[2]])
    game_date = as.Date(substr(raw_summary[['header']][['competitions']][['date']], 0, 10))
    
    df_pickcenter$game_id <- game_id
    df_pickcenter$team1_id <- team1id
    df_pickcenter$team2_id <- team2id
    df_pickcenter$team1_name <- team1name
    df_pickcenter$team2_name <- team2name
    df_pickcenter$team1_sname <- team1sname
    df_pickcenter$team2_sname <- team2sname
    df_pickcenter$date <- game_date
  }
  
  return(df_pickcenter)
  
}

espn_mbb_box_score <- function(game_id){
  print(game_id)
  summary_url <-
    "http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary?"
  
  ## Inputs
  ## game_id
  full_url <- paste0(summary_url,
                     "event=", game_id)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  res <- httr::RETRY("GET", full_url)
  x = httr::status_code(res)
  if(x != 200) return(tibble())

  
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
      conferencecompetition = raw_play_df[["header"]][["competitions"]][["conferenceCompetition"]]
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
        )      
      
      df_teams_box_score <- dplyr::bind_cols(teams1, teams2)
      df_teams_box_score <- df_teams_box_score %>%
        mutate(team1 = homeTeamName,
               team2 = awayTeamName,
               team1_home_away = homeAwayTeam1,
               team2_home_away <- homeAwayTeam2,
               team1_id = as.integer(homeTeamId),
               team2_id = as.integer(awayTeamId),
               team1_mascot = homeTeamMascot,
               team1_abbrev = homeTeamAbbrev,
               team2_mascot = awayTeamMascot,
               team2_abbrev = awayTeamAbbrev,
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
               neutralsite = neutralsite,
               conferencecompetition = conferencecompetition
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
  return(df_teams_box_score)
}

#Loads each available game id,
espn_mbb_game_id_season <- function(season){
  df_gi <- hoopR::load_mbb_team_box(season) %>%
    select(game_id) %>%
    group_by(game_id) %>%
    summarise(n = n()) %>%
    select(game_id)
  return(df_gi)
}


#Loads certain season box score
espn_mbb_box_score_season <- function(season){
  df_gi <- espn_mbb_game_id_season(season) 
  df_box_score <- bind_rows(map(df_gi$game_id,espn_mbb_box_score))
  if (!("ref1" %in% colnames(df_box_score))){
    df_box_score <- df_box_score %>%
      add_column(ref1 = NA,
                 ref2 = NA,
                 ref3 = NA)
  }
  df_box_score <- df_box_score%>%
      dplyr::select(
      "game_id",
      "season",
      "season_type",
      "game_date",
      "neutralsite",
      starts_with("team1_"),
      starts_with("team2_"),
      starts_with("ref"),
      starts_with("largest_l"),
      tidyr::everything()) 
  
  write_csv(df_box_score, paste0("espn_mbb_box_score_", season, ".csv"))
  
  #write_csv(df_box_score, paste0("./../Inputs/NCAA/espn_mbb_box_score_", season, ".csv"))
  
}
#End ESPN Game ID

############################
# End of Functions
############################

#TORVIK BOX SCORE
season <- 2023
for (season in 2008:2023){
  #First we get all the boxscore data for each available season.
  df <- bart_expanded_box_score(season)
  df <- df %>%
    mutate(win = str_trim(win),
           loss = str_trim(loss),
           team2 = str_trim(team2),
           team1 = str_trim(team1))
  write_csv(df, paste0("torvik_box_score_", season, ".csv"))
}

#All betting lines
for (season in 2008:2023){
  # https://www.espn.com/mens-college-basketball/boxscore/_/gameId/253292579
  # http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary?event=253292579
  df_gi <- espn_mbb_game_id_season(season) 
  df_box_score <- bind_rows(map(df_gi$game_id,espn_mbb_lines))
  write_csv(df_box_score, paste0("../../Inputs/NCAA/espn_mbb_betting_lines_", season, ".csv"))
  
  
}




season <- 2023
#Pull box scores for each season
#for (season in 2006:2022){
for (season in 2022:2023){
  # https://www.espn.com/mens-college-basketball/boxscore/_/gameId/253292579
  # http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary?event=253292579
  espn_mbb_box_score_season(season)

}





###########################################################################
### Merge ESPN and Torvik Box score Names
###########################################################################
files <- fs::dir_ls(path = "../../Inputs/NCAA/", 
                    regexp = "espn_mbb_box.*.csv")

df_espn <- vroom(files)
df_espn_names <- df_espn %>%
  select(team1, team1_id, team2, team2_id)
df_team1 <- df_espn_names %>%
  select("team1", "team1_id") %>%
  rename("team" = "team1",
         "team_id" = "team1_id")
df_team2 <- df_espn_names %>%
  select("team2", "team2_id") %>%
  rename("team" = "team2",
         "team_id" = "team2_id")
df_unique_names <- df_team1 %>%
  bind_rows(df_team2) %>%
  count(team, team_id) %>%
  arrange(team)
write_csv(df_unique_names, "../../Inputs/NCAA/unique_names_espn.csv")


files <- fs::dir_ls(path = "../../Inputs/NCAA/", 
                    regexp = "torvik_box_score*")

df_torvik <- vroom(files)

df_torvik_names <- df_torvik %>%
  select(team1, team2)
df_team1 <- df_torvik_names %>%
  select("team1") %>%
  rename("team" = "team1")
df_team2 <- df_torvik_names %>%
  select("team2") %>%
  rename("team" = "team2")
df_unique_names <- df_team1 %>%
  bind_rows(df_team2) %>%
  count(team) %>%
  arrange(team)
write_csv(df_unique_names, "../../Inputs/NCAA/unique_names_torvik.csv")

#IPFW needs to be changed to Fort Wayne!!!!
for(season in 2008:2022){
  df_box_score <- vroom(paste0("../../Inputs/NCAA/torvik_box_score_", season, ".csv"), altrep = FALSE)
  df_box_score <- df_box_score %>%
    mutate_if(is.character, 
                  str_replace_all, 
                  pattern = "IPFW", 
                  replacement = "Fort Wayne")
write_csv(df_box_score, paste0("../../Inputs/NCAA/torvik_box_score_", season, ".csv"))
  
}



#Rename ESPN names. 
for(season in 2006:2023){
  df_box_score <- vroom(paste0("../../Inputs/NCAA/espn_mbb_box_score_", season, ".csv"), altrep = FALSE)
  df_box_score <- df_box_score %>%
    filter( team1_id)
  df_box_score <- df_box_score %>%
    dplyr::mutate(
      team1 = dplyr::case_when(
        team1_id == 2000 ~ "Abilene Christian",
        team1_id == 2005 ~ "Air Force",
        team1_id == 2006 ~ "Akron",
        team1_id == 333 ~ "Alabama",
        team1_id == 2010 ~ "Alabama A&M",
        team1_id == 2011 ~ "Alabama St.",
        team1_id == 399 ~ "Albany",
        team1_id == 2016 ~ "Alcorn St.",
        team1_id == 44 ~ "American",
        team1_id == 2026 ~ "Appalachian St.",
        team1_id == 12 ~ "Arizona",
        team1_id == 9 ~ "Arizona St.",
        team1_id == 8 ~ "Arkansas",
        team1_id == 2031 ~ "Arkansas Little Rock",
        team1_id == 2029 ~ "Arkansas Pine Bluff",
        team1_id == 2032 ~ "Arkansas St.",
        team1_id == 349 ~ "Army",
        team1_id == 2 ~ "Auburn",
        team1_id == 2046 ~ "Austin Peay",
        team1_id == 2050 ~ "Ball St.",
        team1_id == 239 ~ "Baylor",
        team1_id == 91 ~ "Bellarmine",
        team1_id == 2057 ~ "Belmont",
        team1_id == 2065 ~ "Bethune Cookman",
        team1_id == 2066 ~ "Binghamton",
        team1_id == 68 ~ "Boise St.",
        team1_id == 103 ~ "Boston College",
        team1_id == 104 ~ "Boston University",
        team1_id == 189 ~ "Bowling Green",
        team1_id == 71 ~ "Bradley",
        team1_id == 225 ~ "Brown",
        team1_id == 2803 ~ "Bryant",
        team1_id == 2083 ~ "Bucknell",
        team1_id == 2084 ~ "Buffalo",
        team1_id == 2086 ~ "Butler",
        team1_id == 252 ~ "BYU",
        team1_id == 2856 ~ "Cal Baptist",
        team1_id == 13 ~ "Cal Poly",
        team1_id == 2934 ~ "Cal St. Bakersfield",
        team1_id == 2239 ~ "Cal St. Fullerton",
        team1_id == 2463 ~ "Cal St. Northridge",
        team1_id == 25 ~ "California",
        team1_id == 2097 ~ "Campbell",
        team1_id == 2099 ~ "Canisius",
        team1_id == 2113 ~ "Centenary",
        team1_id == 2110 ~ "Central Arkansas",
        team1_id == 2115 ~ "Central Connecticut",
        team1_id == 2117 ~ "Central Michigan",
        team1_id == 2127 ~ "Charleston Southern",
        team1_id == 2429 ~ "Charlotte",
        team1_id == 236 ~ "Chattanooga",
        team1_id == 2130 ~ "Chicago St.",
        team1_id == 2132 ~ "Cincinnati",
        team1_id == 228 ~ "Clemson",
        team1_id == 325 ~ "Cleveland St.",
        team1_id == 324 ~ "Coastal Carolina",
        team1_id == 2142 ~ "Colgate",
        team1_id == 232 ~ "College of Charleston",
        team1_id == 38 ~ "Colorado",
        team1_id == 36 ~ "Colorado St.",
        team1_id == 171 ~ "Columbia",
        team1_id == 41 ~ "Connecticut",
        team1_id == 2154 ~ "Coppin St.",
        team1_id == 172 ~ "Cornell",
        team1_id == 156 ~ "Creighton",
        team1_id == 159 ~ "Dartmouth",
        team1_id == 2166 ~ "Davidson",
        team1_id == 2168 ~ "Dayton",
        team1_id == 48 ~ "Delaware",
        team1_id == 2169 ~ "Delaware St.",
        team1_id == 2172 ~ "Denver",
        team1_id == 305 ~ "DePaul",
        team1_id == 2174 ~ "Detroit",
        team1_id == 3101 ~ "Dixie St.",
        team1_id == 2181 ~ "Drake",
        team1_id == 2182 ~ "Drexel",
        team1_id == 150 ~ "Duke",
        team1_id == 2184 ~ "Duquesne",
        team1_id == 151 ~ "East Carolina",
        team1_id == 2193 ~ "East Tennessee St.",
        team1_id == 2197 ~ "Eastern Illinois",
        team1_id == 2198 ~ "Eastern Kentucky",
        team1_id == 2199 ~ "Eastern Michigan",
        team1_id == 331 ~ "Eastern Washington",
        team1_id == 2210 ~ "Elon",
        team1_id == 339 ~ "Evansville",
        team1_id == 2217 ~ "Fairfield",
        team1_id == 161 ~ "Fairleigh Dickinson",
        team1_id == 2229 ~ "FIU",
        team1_id == 57 ~ "Florida",
        team1_id == 50 ~ "Florida A&M",
        team1_id == 2226 ~ "Florida Atlantic",
        team1_id == 526 ~ "Florida Gulf Coast",
        team1_id == 52 ~ "Florida St.",
        team1_id == 2230 ~ "Fordham",
        team1_id == 2870 ~ "Fort Wayne",
        team1_id == 278 ~ "Fresno St.",
        team1_id == 231 ~ "Furman",
        team1_id == 2241 ~ "Gardner Webb",
        team1_id == 2244 ~ "George Mason",
        team1_id == 45 ~ "George Washington",
        team1_id == 46 ~ "Georgetown",
        team1_id == 61 ~ "Georgia",
        team1_id == 290 ~ "Georgia Southern",
        team1_id == 2247 ~ "Georgia St.",
        team1_id == 59 ~ "Georgia Tech",
        team1_id == 2250 ~ "Gonzaga",
        team1_id == 2755 ~ "Grambling St.",
        team1_id == 2253 ~ "Grand Canyon",
        team1_id == 2739 ~ "Green Bay",
        team1_id == 2261 ~ "Hampton",
        team1_id == 42 ~ "Hartford",
        team1_id == 108 ~ "Harvard",
        team1_id == 62 ~ "Hawaii",
        team1_id == 2272 ~ "High Point",
        team1_id == 2275 ~ "Hofstra",
        team1_id == 107 ~ "Holy Cross",
        team1_id == 248 ~ "Houston",
        team1_id == 2277 ~ "Houston Baptist",
        team1_id == 47 ~ "Howard",
        team1_id == 70 ~ "Idaho",
        team1_id == 304 ~ "Idaho St.",
        team1_id == 356 ~ "Illinois",
        team1_id == 82 ~ "Illinois Chicago",
        team1_id == 2287 ~ "Illinois St.",
        team1_id == 2916 ~ "Incarnate Word",
        team1_id == 84 ~ "Indiana",
        team1_id == 282 ~ "Indiana St.",
        team1_id == 314 ~ "Iona",
        team1_id == 2294 ~ "Iowa",
        team1_id == 66 ~ "Iowa St.",
        team1_id == 85 ~ "IUPUI",
        team1_id == 2296 ~ "Jackson St.",
        team1_id == 294 ~ "Jacksonville",
        team1_id == 55 ~ "Jacksonville St.",
        team1_id == 256 ~ "James Madison",
        team1_id == 2305 ~ "Kansas",
        team1_id == 2306 ~ "Kansas St.",
        team1_id == 338 ~ "Kennesaw St.",
        team1_id == 2309 ~ "Kent St.",
        team1_id == 96 ~ "Kentucky",
        team1_id == 2325 ~ "La Salle",
        team1_id == 322 ~ "Lafayette",
        team1_id == 2320 ~ "Lamar",
        team1_id == 2329 ~ "Lehigh",
        team1_id == 2335 ~ "Liberty",
        team1_id == 288 ~ "Lipscomb",
        team1_id == 2031 ~ "Little Rock",
        team1_id == 112358 ~ "LIU Brooklyn",
        team1_id == 299 ~ "Long Beach St.",
        team1_id == 2344 ~ "Longwood",
        team1_id == 322 ~ "Louisiana Lafayette",
        team1_id == 2433 ~ "Louisiana Monroe",
        team1_id == 2348 ~ "Louisiana Tech",
        team1_id == 97 ~ "Louisville",
        team1_id == 2350 ~ "Loyola Chicago",
        team1_id == 2351 ~ "Loyola Marymount",
        team1_id == 2352 ~ "Loyola MD",
        team1_id == 99 ~ "LSU",
        team1_id == 311 ~ "Maine",
        team1_id == 2363 ~ "Manhattan",
        team1_id == 2368 ~ "Marist",
        team1_id == 269 ~ "Marquette",
        team1_id == 276 ~ "Marshall",
        team1_id == 120 ~ "Maryland",
        team1_id == 2379 ~ "Maryland Eastern Shore",
        team1_id == 113 ~ "Massachusetts",
        team1_id == 2377 ~ "McNeese St.",
        team1_id == 235 ~ "Memphis",
        team1_id == 2382 ~ "Mercer",
        team1_id == 2771 ~ "Merrimack",
        team1_id == 2390 ~ "Miami FL",
        team1_id == 193 ~ "Miami OH",
        team1_id == 130 ~ "Michigan",
        team1_id == 127 ~ "Michigan St.",
        team1_id == 2393 ~ "Middle Tennessee",
        team1_id == 270 ~ "Milwaukee",
        team1_id == 135 ~ "Minnesota",
        team1_id == 145 ~ "Mississippi",
        team1_id == 344 ~ "Mississippi St.",
        team1_id == 2400 ~ "Mississippi Valley St.",
        team1_id == 142 ~ "Missouri",
        team1_id == 2623 ~ "Missouri St.",
        team1_id == 2405 ~ "Monmouth",
        team1_id == 149 ~ "Montana",
        team1_id == 147 ~ "Montana St.",
        team1_id == 2413 ~ "Morehead St.",
        team1_id == 2415 ~ "Morgan St.",
        team1_id == 116 ~ "Mount St. Mary's",
        team1_id == 93 ~ "Murray St.",
        team1_id == 2426 ~ "Navy",
        team1_id == 158 ~ "Nebraska",
        team1_id == 2437 ~ "Nebraska Omaha",
        team1_id == 2440 ~ "Nevada",
        team1_id == 160 ~ "New Hampshire",
        team1_id == 167 ~ "New Mexico",
        team1_id == 166 ~ "New Mexico St.",
        team1_id == 2443 ~ "New Orleans",
        team1_id == 315 ~ "Niagara",
        team1_id == 2447 ~ "Nicholls St.",
        team1_id == 2885 ~ "NJIT",
        team1_id == 2450 ~ "Norfolk St.",
        team1_id == 2453 ~ "North Alabama",
        team1_id == 153 ~ "North Carolina",
        team1_id == 2448 ~ "North Carolina A&T",
        team1_id == 2428 ~ "North Carolina Central",
        team1_id == 152 ~ "North Carolina St.",
        team1_id == 155 ~ "North Dakota",
        team1_id == 2449 ~ "North Dakota St.",
        team1_id == 2454 ~ "North Florida",
        team1_id == 249 ~ "North Texas",
        team1_id == 111 ~ "Northeastern",
        team1_id == 2464 ~ "Northern Arizona",
        team1_id == 2458 ~ "Northern Colorado",
        team1_id == 2459 ~ "Northern Illinois",
        team1_id == 2460 ~ "Northern Iowa",
        team1_id == 94 ~ "Northern Kentucky",
        team1_id == 77 ~ "Northwestern",
        team1_id == 2466 ~ "Northwestern St.",
        team1_id == 87 ~ "Notre Dame",
        team1_id == 2473 ~ "Oakland",
        team1_id == 195 ~ "Ohio",
        team1_id == 194 ~ "Ohio St.",
        team1_id == 201 ~ "Oklahoma",
        team1_id == 197 ~ "Oklahoma St.",
        team1_id == 295 ~ "Old Dominion",
        team1_id == 198 ~ "Oral Roberts",
        team1_id == 2483 ~ "Oregon",
        team1_id == 204 ~ "Oregon St.",
        team1_id == 279 ~ "Pacific",
        team1_id == 219 ~ "Penn",
        team1_id == 213 ~ "Penn St.",
        team1_id == 2492 ~ "Pepperdine",
        team1_id == 221 ~ "Pittsburgh",
        team1_id == 2501 ~ "Portland",
        team1_id == 2502 ~ "Portland St.",
        team1_id == 2504 ~ "Prairie View A&M",
        team1_id == 2506 ~ "Presbyterian",
        team1_id == 163 ~ "Princeton",
        team1_id == 2507 ~ "Providence",
        team1_id == 2509 ~ "Purdue",
        team1_id == 2514 ~ "Quinnipiac",
        team1_id == 2515 ~ "Radford",
        team1_id == 227 ~ "Rhode Island",
        team1_id == 242 ~ "Rice",
        team1_id == 257 ~ "Richmond",
        team1_id == 2520 ~ "Rider",
        team1_id == 2523 ~ "Robert Morris",
        team1_id == 164 ~ "Rutgers",
        team1_id == 16 ~ "Sacramento St.",
        team1_id == 2529 ~ "Sacred Heart",
        team1_id == 2603 ~ "Saint Joseph's",
        team1_id == 139 ~ "Saint Louis",
        team1_id == 2608 ~ "Saint Mary's",
        team1_id == 2612 ~ "Saint Peter's",
        team1_id == 2534 ~ "Sam Houston St.",
        team1_id == 2535 ~ "Samford",
        team1_id == 301 ~ "San Diego",
        team1_id == 21 ~ "San Diego St.",
        team1_id == 2539 ~ "San Francisco",
        team1_id == 23 ~ "San Jose St.",
        team1_id == 2541 ~ "Santa Clara",
        team1_id == 2542 ~ "Savannah St.",
        team1_id == 2547 ~ "Seattle",
        team1_id == 2550 ~ "Seton Hall",
        team1_id == 2561 ~ "Siena",
        team1_id == 2565 ~ "SIU Edwardsville",
        team1_id == 2567 ~ "SMU",
        team1_id == 6 ~ "South Alabama",
        team1_id == 2579 ~ "South Carolina",
        team1_id == 2569 ~ "South Carolina St.",
        team1_id == 233 ~ "South Dakota",
        team1_id == 2571 ~ "South Dakota St.",
        team1_id == 58 ~ "South Florida",
        team1_id == 2546 ~ "Southeast Missouri St.",
        team1_id == 2545 ~ "Southeastern Louisiana",
        team1_id == 2582 ~ "Southern",
        team1_id == 79 ~ "Southern Illinois",
        team1_id == 2572 ~ "Southern Miss",
        team1_id == 253 ~ "Southern Utah",
        team1_id == 179 ~ "St. Bonaventure",
        team1_id == 2597 ~ "St. Francis NY",
        team1_id == 2598 ~ "St. Francis PA",
        team1_id == 2599 ~ "St. John's",
        team1_id == 2900 ~ "St. Thomas",
        team1_id == 24 ~ "Stanford",
        team1_id == 2617 ~ "Stephen F. Austin",
        team1_id == 56 ~ "Stetson",
        team1_id == 2619 ~ "Stony Brook",
        team1_id == 183 ~ "Syracuse",
        team1_id == 2627 ~ "Tarleton St.",
        team1_id == 2628 ~ "TCU",
        team1_id == 218 ~ "Temple",
        team1_id == 2633 ~ "Tennessee",
        team1_id == 2630 ~ "Tennessee Martin",
        team1_id == 2634 ~ "Tennessee St.",
        team1_id == 2635 ~ "Tennessee Tech",
        team1_id == 251 ~ "Texas",
        team1_id == 245 ~ "Texas A&M",
        team1_id == 357 ~ "Texas A&M Corpus Chris",
        team1_id == 2640 ~ "Texas Southern",
        team1_id == 326 ~ "Texas St.",
        team1_id == 2641 ~ "Texas Tech",
        team1_id == 2643 ~ "The Citadel",
        team1_id == 2649 ~ "Toledo",
        team1_id == 119 ~ "Towson",
        team1_id == 2653 ~ "Troy",
        team1_id == 2655 ~ "Tulane",
        team1_id == 202 ~ "Tulsa",
        team1_id == 5 ~ "UAB",
        team1_id == 302 ~ "UC Davis",
        team1_id == 300 ~ "UC Irvine",
        team1_id == 27 ~ "UC Riverside",
        team1_id == 28 ~ "UC San Diego",
        team1_id == 2540 ~ "UC Santa Barbara",
        team1_id == 2116 ~ "UCF",
        team1_id == 26 ~ "UCLA",
        team1_id == 2349 ~ "UMass Lowell",
        team1_id == 2378 ~ "UMBC",
        team1_id == 140 ~ "UMKC",
        team1_id == 2427 ~ "UNC Asheville",
        team1_id == 2430 ~ "UNC Greensboro",
        team1_id == 350 ~ "UNC Wilmington",
        team1_id == 2439 ~ "UNLV",
        team1_id == 30 ~ "USC",
        team1_id == 2908 ~ "USC Upstate",
        team1_id == 250 ~ "UT Arlington",
        team1_id == 292 ~ "UT Rio Grande Valley",
        team1_id == 254 ~ "Utah",
        team1_id == 328 ~ "Utah St.",
        team1_id == 3084 ~ "Utah Valley",
        team1_id == 2638 ~ "UTEP",
        team1_id == 2636 ~ "UTSA",
        team1_id == 2674 ~ "Valparaiso",
        team1_id == 238 ~ "Vanderbilt",
        team1_id == 2670 ~ "VCU",
        team1_id == 261 ~ "Vermont",
        team1_id == 222 ~ "Villanova",
        team1_id == 258 ~ "Virginia",
        team1_id == 259 ~ "Virginia Tech",
        team1_id == 2678 ~ "VMI",
        team1_id == 2681 ~ "Wagner",
        team1_id == 154 ~ "Wake Forest",
        team1_id == 264 ~ "Washington",
        team1_id == 265 ~ "Washington St.",
        team1_id == 2692 ~ "Weber St.",
        team1_id == 277 ~ "West Virginia",
        team1_id == 2717 ~ "Western Carolina",
        team1_id == 2710 ~ "Western Illinois",
        team1_id == 98 ~ "Western Kentucky",
        team1_id == 2711 ~ "Western Michigan",
        team1_id == 2724 ~ "Wichita St.",
        team1_id == 2729 ~ "William & Mary",
        team1_id == 2736 ~ "Winston Salem St.",
        team1_id == 2737 ~ "Winthrop",
        team1_id == 275 ~ "Wisconsin",
        team1_id == 2747 ~ "Wofford",
        team1_id == 2750 ~ "Wright St.",
        team1_id == 2751 ~ "Wyoming",
        team1_id == 2752 ~ "Xavier",
        team1_id == 43 ~ "Yale",
        team1_id == 2754 ~ "Youngstown St."
      )
    )
  df_box_score <- df_box_score %>%
    dplyr::mutate(
      team2 = dplyr::case_when(
        team2_id == 2000 ~ "Abilene Christian",
        team2_id == 2005 ~ "Air Force",
        team2_id == 2006 ~ "Akron",
        team2_id == 333 ~ "Alabama",
        team2_id == 2010 ~ "Alabama A&M",
        team2_id == 2011 ~ "Alabama St.",
        team2_id == 399 ~ "Albany",
        team2_id == 2016 ~ "Alcorn St.",
        team2_id == 44 ~ "American",
        team2_id == 2026 ~ "Appalachian St.",
        team2_id == 12 ~ "Arizona",
        team2_id == 9 ~ "Arizona St.",
        team2_id == 8 ~ "Arkansas",
        team2_id == 2031 ~ "Arkansas Little Rock",
        team2_id == 2029 ~ "Arkansas Pine Bluff",
        team2_id == 2032 ~ "Arkansas St.",
        team2_id == 349 ~ "Army",
        team2_id == 2 ~ "Auburn",
        team2_id == 2046 ~ "Austin Peay",
        team2_id == 2050 ~ "Ball St.",
        team2_id == 239 ~ "Baylor",
        team2_id == 91 ~ "Bellarmine",
        team2_id == 2057 ~ "Belmont",
        team2_id == 2065 ~ "Bethune Cookman",
        team2_id == 2066 ~ "Binghamton",
        team2_id == 68 ~ "Boise St.",
        team2_id == 103 ~ "Boston College",
        team2_id == 104 ~ "Boston University",
        team2_id == 189 ~ "Bowling Green",
        team2_id == 71 ~ "Bradley",
        team2_id == 225 ~ "Brown",
        team2_id == 2803 ~ "Bryant",
        team2_id == 2083 ~ "Bucknell",
        team2_id == 2084 ~ "Buffalo",
        team2_id == 2086 ~ "Butler",
        team2_id == 252 ~ "BYU",
        team2_id == 2856 ~ "Cal Baptist",
        team2_id == 13 ~ "Cal Poly",
        team2_id == 2934 ~ "Cal St. Bakersfield",
        team2_id == 2239 ~ "Cal St. Fullerton",
        team2_id == 2463 ~ "Cal St. Northridge",
        team2_id == 25 ~ "California",
        team2_id == 2097 ~ "Campbell",
        team2_id == 2099 ~ "Canisius",
        team2_id == 2113 ~ "Centenary",
        team2_id == 2110 ~ "Central Arkansas",
        team2_id == 2115 ~ "Central Connecticut",
        team2_id == 2117 ~ "Central Michigan",
        team2_id == 2127 ~ "Charleston Southern",
        team2_id == 2429 ~ "Charlotte",
        team2_id == 236 ~ "Chattanooga",
        team2_id == 2130 ~ "Chicago St.",
        team2_id == 2132 ~ "Cincinnati",
        team2_id == 228 ~ "Clemson",
        team2_id == 325 ~ "Cleveland St.",
        team2_id == 324 ~ "Coastal Carolina",
        team2_id == 2142 ~ "Colgate",
        team2_id == 232 ~ "College of Charleston",
        team2_id == 38 ~ "Colorado",
        team2_id == 36 ~ "Colorado St.",
        team2_id == 171 ~ "Columbia",
        team2_id == 41 ~ "Connecticut",
        team2_id == 2154 ~ "Coppin St.",
        team2_id == 172 ~ "Cornell",
        team2_id == 156 ~ "Creighton",
        team2_id == 159 ~ "Dartmouth",
        team2_id == 2166 ~ "Davidson",
        team2_id == 2168 ~ "Dayton",
        team2_id == 48 ~ "Delaware",
        team2_id == 2169 ~ "Delaware St.",
        team2_id == 2172 ~ "Denver",
        team2_id == 305 ~ "DePaul",
        team2_id == 2174 ~ "Detroit",
        team2_id == 3101 ~ "Dixie St.",
        team2_id == 2181 ~ "Drake",
        team2_id == 2182 ~ "Drexel",
        team2_id == 150 ~ "Duke",
        team2_id == 2184 ~ "Duquesne",
        team2_id == 151 ~ "East Carolina",
        team2_id == 2193 ~ "East Tennessee St.",
        team2_id == 2197 ~ "Eastern Illinois",
        team2_id == 2198 ~ "Eastern Kentucky",
        team2_id == 2199 ~ "Eastern Michigan",
        team2_id == 331 ~ "Eastern Washington",
        team2_id == 2210 ~ "Elon",
        team2_id == 339 ~ "Evansville",
        team2_id == 2217 ~ "Fairfield",
        team2_id == 161 ~ "Fairleigh Dickinson",
        team2_id == 2229 ~ "FIU",
        team2_id == 57 ~ "Florida",
        team2_id == 50 ~ "Florida A&M",
        team2_id == 2226 ~ "Florida Atlantic",
        team2_id == 526 ~ "Florida Gulf Coast",
        team2_id == 52 ~ "Florida St.",
        team2_id == 2230 ~ "Fordham",
        team2_id == 2870 ~ "Fort Wayne",
        team2_id == 278 ~ "Fresno St.",
        team2_id == 231 ~ "Furman",
        team2_id == 2241 ~ "Gardner Webb",
        team2_id == 2244 ~ "George Mason",
        team2_id == 45 ~ "George Washington",
        team2_id == 46 ~ "Georgetown",
        team2_id == 61 ~ "Georgia",
        team2_id == 290 ~ "Georgia Southern",
        team2_id == 2247 ~ "Georgia St.",
        team2_id == 59 ~ "Georgia Tech",
        team2_id == 2250 ~ "Gonzaga",
        team2_id == 2755 ~ "Grambling St.",
        team2_id == 2253 ~ "Grand Canyon",
        team2_id == 2739 ~ "Green Bay",
        team2_id == 2261 ~ "Hampton",
        team2_id == 42 ~ "Hartford",
        team2_id == 108 ~ "Harvard",
        team2_id == 62 ~ "Hawaii",
        team2_id == 2272 ~ "High Point",
        team2_id == 2275 ~ "Hofstra",
        team2_id == 107 ~ "Holy Cross",
        team2_id == 248 ~ "Houston",
        team2_id == 2277 ~ "Houston Baptist",
        team2_id == 47 ~ "Howard",
        team2_id == 70 ~ "Idaho",
        team2_id == 304 ~ "Idaho St.",
        team2_id == 356 ~ "Illinois",
        team2_id == 82 ~ "Illinois Chicago",
        team2_id == 2287 ~ "Illinois St.",
        team2_id == 2916 ~ "Incarnate Word",
        team2_id == 84 ~ "Indiana",
        team2_id == 282 ~ "Indiana St.",
        team2_id == 314 ~ "Iona",
        team2_id == 2294 ~ "Iowa",
        team2_id == 66 ~ "Iowa St.",
        team2_id == 85 ~ "IUPUI",
        team2_id == 2296 ~ "Jackson St.",
        team2_id == 294 ~ "Jacksonville",
        team2_id == 55 ~ "Jacksonville St.",
        team2_id == 256 ~ "James Madison",
        team2_id == 2305 ~ "Kansas",
        team2_id == 2306 ~ "Kansas St.",
        team2_id == 338 ~ "Kennesaw St.",
        team2_id == 2309 ~ "Kent St.",
        team2_id == 96 ~ "Kentucky",
        team2_id == 2325 ~ "La Salle",
        team2_id == 322 ~ "Lafayette",
        team2_id == 2320 ~ "Lamar",
        team2_id == 2329 ~ "Lehigh",
        team2_id == 2335 ~ "Liberty",
        team2_id == 288 ~ "Lipscomb",
        team2_id == 2031 ~ "Little Rock",
        team2_id == 112358 ~ "LIU Brooklyn",
        team2_id == 299 ~ "Long Beach St.",
        team2_id == 2344 ~ "Longwood",
        team2_id == 322 ~ "Louisiana Lafayette",
        team2_id == 2433 ~ "Louisiana Monroe",
        team2_id == 2348 ~ "Louisiana Tech",
        team2_id == 97 ~ "Louisville",
        team2_id == 2350 ~ "Loyola Chicago",
        team2_id == 2351 ~ "Loyola Marymount",
        team2_id == 2352 ~ "Loyola MD",
        team2_id == 99 ~ "LSU",
        team2_id == 311 ~ "Maine",
        team2_id == 2363 ~ "Manhattan",
        team2_id == 2368 ~ "Marist",
        team2_id == 269 ~ "Marquette",
        team2_id == 276 ~ "Marshall",
        team2_id == 120 ~ "Maryland",
        team2_id == 2379 ~ "Maryland Eastern Shore",
        team2_id == 113 ~ "Massachusetts",
        team2_id == 2377 ~ "McNeese St.",
        team2_id == 235 ~ "Memphis",
        team2_id == 2382 ~ "Mercer",
        team2_id == 2771 ~ "Merrimack",
        team2_id == 2390 ~ "Miami FL",
        team2_id == 193 ~ "Miami OH",
        team2_id == 130 ~ "Michigan",
        team2_id == 127 ~ "Michigan St.",
        team2_id == 2393 ~ "Middle Tennessee",
        team2_id == 270 ~ "Milwaukee",
        team2_id == 135 ~ "Minnesota",
        team2_id == 145 ~ "Mississippi",
        team2_id == 344 ~ "Mississippi St.",
        team2_id == 2400 ~ "Mississippi Valley St.",
        team2_id == 142 ~ "Missouri",
        team2_id == 2623 ~ "Missouri St.",
        team2_id == 2405 ~ "Monmouth",
        team2_id == 149 ~ "Montana",
        team2_id == 147 ~ "Montana St.",
        team2_id == 2413 ~ "Morehead St.",
        team2_id == 2415 ~ "Morgan St.",
        team2_id == 116 ~ "Mount St. Mary's",
        team2_id == 93 ~ "Murray St.",
        team2_id == 2426 ~ "Navy",
        team2_id == 158 ~ "Nebraska",
        team2_id == 2437 ~ "Nebraska Omaha",
        team2_id == 2440 ~ "Nevada",
        team2_id == 160 ~ "New Hampshire",
        team2_id == 167 ~ "New Mexico",
        team2_id == 166 ~ "New Mexico St.",
        team2_id == 2443 ~ "New Orleans",
        team2_id == 315 ~ "Niagara",
        team2_id == 2447 ~ "Nicholls St.",
        team2_id == 2885 ~ "NJIT",
        team2_id == 2450 ~ "Norfolk St.",
        team2_id == 2453 ~ "North Alabama",
        team2_id == 153 ~ "North Carolina",
        team2_id == 2448 ~ "North Carolina A&T",
        team2_id == 2428 ~ "North Carolina Central",
        team2_id == 152 ~ "North Carolina St.",
        team2_id == 155 ~ "North Dakota",
        team2_id == 2449 ~ "North Dakota St.",
        team2_id == 2454 ~ "North Florida",
        team2_id == 249 ~ "North Texas",
        team2_id == 111 ~ "Northeastern",
        team2_id == 2464 ~ "Northern Arizona",
        team2_id == 2458 ~ "Northern Colorado",
        team2_id == 2459 ~ "Northern Illinois",
        team2_id == 2460 ~ "Northern Iowa",
        team2_id == 94 ~ "Northern Kentucky",
        team2_id == 77 ~ "Northwestern",
        team2_id == 2466 ~ "Northwestern St.",
        team2_id == 87 ~ "Notre Dame",
        team2_id == 2473 ~ "Oakland",
        team2_id == 195 ~ "Ohio",
        team2_id == 194 ~ "Ohio St.",
        team2_id == 201 ~ "Oklahoma",
        team2_id == 197 ~ "Oklahoma St.",
        team2_id == 295 ~ "Old Dominion",
        team2_id == 198 ~ "Oral Roberts",
        team2_id == 2483 ~ "Oregon",
        team2_id == 204 ~ "Oregon St.",
        team2_id == 279 ~ "Pacific",
        team2_id == 219 ~ "Penn",
        team2_id == 213 ~ "Penn St.",
        team2_id == 2492 ~ "Pepperdine",
        team2_id == 221 ~ "Pittsburgh",
        team2_id == 2501 ~ "Portland",
        team2_id == 2502 ~ "Portland St.",
        team2_id == 2504 ~ "Prairie View A&M",
        team2_id == 2506 ~ "Presbyterian",
        team2_id == 163 ~ "Princeton",
        team2_id == 2507 ~ "Providence",
        team2_id == 2509 ~ "Purdue",
        team2_id == 2514 ~ "Quinnipiac",
        team2_id == 2515 ~ "Radford",
        team2_id == 227 ~ "Rhode Island",
        team2_id == 242 ~ "Rice",
        team2_id == 257 ~ "Richmond",
        team2_id == 2520 ~ "Rider",
        team2_id == 2523 ~ "Robert Morris",
        team2_id == 164 ~ "Rutgers",
        team2_id == 16 ~ "Sacramento St.",
        team2_id == 2529 ~ "Sacred Heart",
        team2_id == 2603 ~ "Saint Joseph's",
        team2_id == 139 ~ "Saint Louis",
        team2_id == 2608 ~ "Saint Mary's",
        team2_id == 2612 ~ "Saint Peter's",
        team2_id == 2534 ~ "Sam Houston St.",
        team2_id == 2535 ~ "Samford",
        team2_id == 301 ~ "San Diego",
        team2_id == 21 ~ "San Diego St.",
        team2_id == 2539 ~ "San Francisco",
        team2_id == 23 ~ "San Jose St.",
        team2_id == 2541 ~ "Santa Clara",
        team2_id == 2542 ~ "Savannah St.",
        team2_id == 2547 ~ "Seattle",
        team2_id == 2550 ~ "Seton Hall",
        team2_id == 2561 ~ "Siena",
        team2_id == 2565 ~ "SIU Edwardsville",
        team2_id == 2567 ~ "SMU",
        team2_id == 6 ~ "South Alabama",
        team2_id == 2579 ~ "South Carolina",
        team2_id == 2569 ~ "South Carolina St.",
        team2_id == 233 ~ "South Dakota",
        team2_id == 2571 ~ "South Dakota St.",
        team2_id == 58 ~ "South Florida",
        team2_id == 2546 ~ "Southeast Missouri St.",
        team2_id == 2545 ~ "Southeastern Louisiana",
        team2_id == 2582 ~ "Southern",
        team2_id == 79 ~ "Southern Illinois",
        team2_id == 2572 ~ "Southern Miss",
        team2_id == 253 ~ "Southern Utah",
        team2_id == 179 ~ "St. Bonaventure",
        team2_id == 2597 ~ "St. Francis NY",
        team2_id == 2598 ~ "St. Francis PA",
        team2_id == 2599 ~ "St. John's",
        team2_id == 2900 ~ "St. Thomas",
        team2_id == 24 ~ "Stanford",
        team2_id == 2617 ~ "Stephen F. Austin",
        team2_id == 56 ~ "Stetson",
        team2_id == 2619 ~ "Stony Brook",
        team2_id == 183 ~ "Syracuse",
        team2_id == 2627 ~ "Tarleton St.",
        team2_id == 2628 ~ "TCU",
        team2_id == 218 ~ "Temple",
        team2_id == 2633 ~ "Tennessee",
        team2_id == 2630 ~ "Tennessee Martin",
        team2_id == 2634 ~ "Tennessee St.",
        team2_id == 2635 ~ "Tennessee Tech",
        team2_id == 251 ~ "Texas",
        team2_id == 245 ~ "Texas A&M",
        team2_id == 357 ~ "Texas A&M Corpus Chris",
        team2_id == 2640 ~ "Texas Southern",
        team2_id == 326 ~ "Texas St.",
        team2_id == 2641 ~ "Texas Tech",
        team2_id == 2643 ~ "The Citadel",
        team2_id == 2649 ~ "Toledo",
        team2_id == 119 ~ "Towson",
        team2_id == 2653 ~ "Troy",
        team2_id == 2655 ~ "Tulane",
        team2_id == 202 ~ "Tulsa",
        team2_id == 5 ~ "UAB",
        team2_id == 302 ~ "UC Davis",
        team2_id == 300 ~ "UC Irvine",
        team2_id == 27 ~ "UC Riverside",
        team2_id == 28 ~ "UC San Diego",
        team2_id == 2540 ~ "UC Santa Barbara",
        team2_id == 2116 ~ "UCF",
        team2_id == 26 ~ "UCLA",
        team2_id == 2349 ~ "UMass Lowell",
        team2_id == 2378 ~ "UMBC",
        team2_id == 140 ~ "UMKC",
        team2_id == 2427 ~ "UNC Asheville",
        team2_id == 2430 ~ "UNC Greensboro",
        team2_id == 350 ~ "UNC Wilmington",
        team2_id == 2439 ~ "UNLV",
        team2_id == 30 ~ "USC",
        team2_id == 2908 ~ "USC Upstate",
        team2_id == 250 ~ "UT Arlington",
        team2_id == 292 ~ "UT Rio Grande Valley",
        team2_id == 254 ~ "Utah",
        team2_id == 328 ~ "Utah St.",
        team2_id == 3084 ~ "Utah Valley",
        team2_id == 2638 ~ "UTEP",
        team2_id == 2636 ~ "UTSA",
        team2_id == 2674 ~ "Valparaiso",
        team2_id == 238 ~ "Vanderbilt",
        team2_id == 2670 ~ "VCU",
        team2_id == 261 ~ "Vermont",
        team2_id == 222 ~ "Villanova",
        team2_id == 258 ~ "Virginia",
        team2_id == 259 ~ "Virginia Tech",
        team2_id == 2678 ~ "VMI",
        team2_id == 2681 ~ "Wagner",
        team2_id == 154 ~ "Wake Forest",
        team2_id == 264 ~ "Washington",
        team2_id == 265 ~ "Washington St.",
        team2_id == 2692 ~ "Weber St.",
        team2_id == 277 ~ "West Virginia",
        team2_id == 2717 ~ "Western Carolina",
        team2_id == 2710 ~ "Western Illinois",
        team2_id == 98 ~ "Western Kentucky",
        team2_id == 2711 ~ "Western Michigan",
        team2_id == 2724 ~ "Wichita St.",
        team2_id == 2729 ~ "William & Mary",
        team2_id == 2736 ~ "Winston Salem St.",
        team2_id == 2737 ~ "Winthrop",
        team2_id == 275 ~ "Wisconsin",
        team2_id == 2747 ~ "Wofford",
        team2_id == 2750 ~ "Wright St.",
        team2_id == 2751 ~ "Wyoming",
        team2_id == 2752 ~ "Xavier",
        team2_id == 43 ~ "Yale",
        team2_id == 2754 ~ "Youngstown St.",
      )
    )
  write_csv(df_box_score, paste0("../../Inputs/NCAA/espn_mbb_box_score_", season, ".csv"))
  
}
  
  
  
  
#Rename ESPN names. Cut to D1 teams only and save in new file name! IN PROGRESS!
for(season in 2006:2023){
  df_box_score <- vroom(paste0("../../Inputs/NCAA/espn_mbb_box_score_", season, ".csv"), altrep = FALSE)
  df_box_score <- df_box_score %>%
    dplyr::mutate(
      status1 = dplyr::case_when(
        team1_id == 2000 ~ "Keep",
        team1_id == 2005 ~ "Keep",
        team1_id == 2006 ~ "Keep",
        team1_id == 333 ~ "Keep",
        team1_id == 2010 ~ "Keep",
        team1_id == 2011 ~ "Keep",
        team1_id == 399 ~ "Keep",
        team1_id == 2016 ~ "Keep",
        team1_id == 44 ~ "Keep",
        team1_id == 2026 ~ "Keep",
        team1_id == 12 ~ "Keep",
        team1_id == 9 ~ "Keep",
        team1_id == 8 ~ "Keep",
        team1_id == 2031 ~ "Keep",
        team1_id == 2029 ~ "Keep",
        team1_id == 2032 ~ "Keep",
        team1_id == 349 ~ "Keep",
        team1_id == 2 ~ "Keep",
        team1_id == 2046 ~ "Keep",
        team1_id == 2050 ~ "Keep",
        team1_id == 239 ~ "Keep",
        team1_id == 91 ~ "Keep",
        team1_id == 2057 ~ "Keep",
        team1_id == 2065 ~ "Keep",
        team1_id == 2066 ~ "Keep",
        team1_id == 68 ~ "Keep",
        team1_id == 103 ~ "Keep",
        team1_id == 104 ~ "Keep",
        team1_id == 189 ~ "Keep",
        team1_id == 71 ~ "Keep",
        team1_id == 225 ~ "Keep",
        team1_id == 2803 ~ "Keep",
        team1_id == 2083 ~ "Keep",
        team1_id == 2084 ~ "Keep",
        team1_id == 2086 ~ "Keep",
        team1_id == 252 ~ "Keep",
        team1_id == 2856 ~ "Keep",
        team1_id == 13 ~ "Keep",
        team1_id == 2934 ~ "Keep",
        team1_id == 2239 ~ "Keep",
        team1_id == 2463 ~ "Keep",
        team1_id == 25 ~ "Keep",
        team1_id == 2097 ~ "Keep",
        team1_id == 2099 ~ "Keep",
        team1_id == 2113 ~ "Keep",
        team1_id == 2110 ~ "Keep",
        team1_id == 2115 ~ "Keep",
        team1_id == 2117 ~ "Keep",
        team1_id == 2127 ~ "Keep",
        team1_id == 2429 ~ "Keep",
        team1_id == 236 ~ "Keep",
        team1_id == 2130 ~ "Keep",
        team1_id == 2132 ~ "Keep",
        team1_id == 228 ~ "Keep",
        team1_id == 325 ~ "Keep",
        team1_id == 324 ~ "Keep",
        team1_id == 2142 ~ "Keep",
        team1_id == 232 ~ "Keep",
        team1_id == 38 ~ "Keep",
        team1_id == 36 ~ "Keep",
        team1_id == 171 ~ "Keep",
        team1_id == 41 ~ "Keep",
        team1_id == 2154 ~ "Keep",
        team1_id == 172 ~ "Keep",
        team1_id == 156 ~ "Keep",
        team1_id == 159 ~ "Keep",
        team1_id == 2166 ~ "Keep",
        team1_id == 2168 ~ "Keep",
        team1_id == 48 ~ "Keep",
        team1_id == 2169 ~ "Keep",
        team1_id == 2172 ~ "Keep",
        team1_id == 305 ~ "Keep",
        team1_id == 2174 ~ "Keep",
        team1_id == 3101 ~ "Keep",
        team1_id == 2181 ~ "Keep",
        team1_id == 2182 ~ "Keep",
        team1_id == 150 ~ "Keep",
        team1_id == 2184 ~ "Keep",
        team1_id == 151 ~ "Keep",
        team1_id == 2193 ~ "Keep",
        team1_id == 2197 ~ "Keep",
        team1_id == 2198 ~ "Keep",
        team1_id == 2199 ~ "Keep",
        team1_id == 331 ~ "Keep",
        team1_id == 2210 ~ "Keep",
        team1_id == 339 ~ "Keep",
        team1_id == 2217 ~ "Keep",
        team1_id == 161 ~ "Keep",
        team1_id == 2229 ~ "Keep",
        team1_id == 57 ~ "Keep",
        team1_id == 50 ~ "Keep",
        team1_id == 2226 ~ "Keep",
        team1_id == 526 ~ "Keep",
        team1_id == 52 ~ "Keep",
        team1_id == 2230 ~ "Keep",
        team1_id == 2870 ~ "Keep",
        team1_id == 278 ~ "Keep",
        team1_id == 231 ~ "Keep",
        team1_id == 2241 ~ "Keep",
        team1_id == 2244 ~ "Keep",
        team1_id == 45 ~ "Keep",
        team1_id == 46 ~ "Keep",
        team1_id == 61 ~ "Keep",
        team1_id == 290 ~ "Keep",
        team1_id == 2247 ~ "Keep",
        team1_id == 59 ~ "Keep",
        team1_id == 2250 ~ "Keep",
        team1_id == 2755 ~ "Keep",
        team1_id == 2253 ~ "Keep",
        team1_id == 2739 ~ "Keep",
        team1_id == 2261 ~ "Keep",
        team1_id == 42 ~ "Keep",
        team1_id == 108 ~ "Keep",
        team1_id == 62 ~ "Keep",
        team1_id == 2272 ~ "Keep",
        team1_id == 2275 ~ "Keep",
        team1_id == 107 ~ "Keep",
        team1_id == 248 ~ "Keep",
        team1_id == 2277 ~ "Keep",
        team1_id == 47 ~ "Keep",
        team1_id == 70 ~ "Keep",
        team1_id == 304 ~ "Keep",
        team1_id == 356 ~ "Keep",
        team1_id == 82 ~ "Keep",
        team1_id == 2287 ~ "Keep",
        team1_id == 2916 ~ "Keep",
        team1_id == 84 ~ "Keep",
        team1_id == 282 ~ "Keep",
        team1_id == 314 ~ "Keep",
        team1_id == 2294 ~ "Keep",
        team1_id == 66 ~ "Keep",
        team1_id == 85 ~ "Keep",
        team1_id == 2296 ~ "Keep",
        team1_id == 294 ~ "Keep",
        team1_id == 55 ~ "Keep",
        team1_id == 256 ~ "Keep",
        team1_id == 2305 ~ "Keep",
        team1_id == 2306 ~ "Keep",
        team1_id == 338 ~ "Keep",
        team1_id == 2309 ~ "Keep",
        team1_id == 96 ~ "Keep",
        team1_id == 2325 ~ "Keep",
        team1_id == 322 ~ "Keep",
        team1_id == 2320 ~ "Keep",
        team1_id == 2329 ~ "Keep",
        team1_id == 2335 ~ "Keep",
        team1_id == 288 ~ "Keep",
        team1_id == 2031 ~ "Keep",
        team1_id == 112358 ~ "Keep",
        team1_id == 299 ~ "Keep",
        team1_id == 2344 ~ "Keep",
        team1_id == 322 ~ "Keep",
        team1_id == 2433 ~ "Keep",
        team1_id == 2348 ~ "Keep",
        team1_id == 97 ~ "Keep",
        team1_id == 2350 ~ "Keep",
        team1_id == 2351 ~ "Keep",
        team1_id == 2352 ~ "Keep",
        team1_id == 99 ~ "Keep",
        team1_id == 311 ~ "Keep",
        team1_id == 2363 ~ "Keep",
        team1_id == 2368 ~ "Keep",
        team1_id == 269 ~ "Keep",
        team1_id == 276 ~ "Keep",
        team1_id == 120 ~ "Keep",
        team1_id == 2379 ~ "Keep",
        team1_id == 113 ~ "Keep",
        team1_id == 2377 ~ "Keep",
        team1_id == 235 ~ "Keep",
        team1_id == 2382 ~ "Keep",
        team1_id == 2771 ~ "Keep",
        team1_id == 2390 ~ "Keep",
        team1_id == 193 ~ "Keep",
        team1_id == 130 ~ "Keep",
        team1_id == 127 ~ "Keep",
        team1_id == 2393 ~ "Keep",
        team1_id == 270 ~ "Keep",
        team1_id == 135 ~ "Keep",
        team1_id == 145 ~ "Keep",
        team1_id == 344 ~ "Keep",
        team1_id == 2400 ~ "Keep",
        team1_id == 142 ~ "Keep",
        team1_id == 2623 ~ "Keep",
        team1_id == 2405 ~ "Keep",
        team1_id == 149 ~ "Keep",
        team1_id == 147 ~ "Keep",
        team1_id == 2413 ~ "Keep",
        team1_id == 2415 ~ "Keep",
        team1_id == 116 ~ "Keep",
        team1_id == 93 ~ "Keep",
        team1_id == 2426 ~ "Keep",
        team1_id == 158 ~ "Keep",
        team1_id == 2437 ~ "Keep",
        team1_id == 2440 ~ "Keep",
        team1_id == 160 ~ "Keep",
        team1_id == 167 ~ "Keep",
        team1_id == 166 ~ "Keep",
        team1_id == 2443 ~ "Keep",
        team1_id == 315 ~ "Keep",
        team1_id == 2447 ~ "Keep",
        team1_id == 2885 ~ "Keep",
        team1_id == 2450 ~ "Keep",
        team1_id == 2453 ~ "Keep",
        team1_id == 153 ~ "Keep",
        team1_id == 2448 ~ "Keep",
        team1_id == 2428 ~ "Keep",
        team1_id == 152 ~ "Keep",
        team1_id == 155 ~ "Keep",
        team1_id == 2449 ~ "Keep",
        team1_id == 2454 ~ "Keep",
        team1_id == 249 ~ "Keep",
        team1_id == 111 ~ "Keep",
        team1_id == 2464 ~ "Keep",
        team1_id == 2458 ~ "Keep",
        team1_id == 2459 ~ "Keep",
        team1_id == 2460 ~ "Keep",
        team1_id == 94 ~ "Keep",
        team1_id == 77 ~ "Keep",
        team1_id == 2466 ~ "Keep",
        team1_id == 87 ~ "Keep",
        team1_id == 2473 ~ "Keep",
        team1_id == 195 ~ "Keep",
        team1_id == 194 ~ "Keep",
        team1_id == 201 ~ "Keep",
        team1_id == 197 ~ "Keep",
        team1_id == 295 ~ "Keep",
        team1_id == 198 ~ "Keep",
        team1_id == 2483 ~ "Keep",
        team1_id == 204 ~ "Keep",
        team1_id == 279 ~ "Keep",
        team1_id == 219 ~ "Keep",
        team1_id == 213 ~ "Keep",
        team1_id == 2492 ~ "Keep",
        team1_id == 221 ~ "Keep",
        team1_id == 2501 ~ "Keep",
        team1_id == 2502 ~ "Keep",
        team1_id == 2504 ~ "Keep",
        team1_id == 2506 ~ "Keep",
        team1_id == 163 ~ "Keep",
        team1_id == 2507 ~ "Keep",
        team1_id == 2509 ~ "Keep",
        team1_id == 2514 ~ "Keep",
        team1_id == 2515 ~ "Keep",
        team1_id == 227 ~ "Keep",
        team1_id == 242 ~ "Keep",
        team1_id == 257 ~ "Keep",
        team1_id == 2520 ~ "Keep",
        team1_id == 2523 ~ "Keep",
        team1_id == 164 ~ "Keep",
        team1_id == 16 ~ "Keep",
        team1_id == 2529 ~ "Keep",
        team1_id == 2603 ~ "Keep",
        team1_id == 139 ~ "Keep",
        team1_id == 2608 ~ "Keep",
        team1_id == 2612 ~ "Keep",
        team1_id == 2534 ~ "Keep",
        team1_id == 2535 ~ "Keep",
        team1_id == 301 ~ "Keep",
        team1_id == 21 ~ "Keep",
        team1_id == 2539 ~ "Keep",
        team1_id == 23 ~ "Keep",
        team1_id == 2541 ~ "Keep",
        team1_id == 2542 ~ "Keep",
        team1_id == 2547 ~ "Keep",
        team1_id == 2550 ~ "Keep",
        team1_id == 2561 ~ "Keep",
        team1_id == 2565 ~ "Keep",
        team1_id == 2567 ~ "Keep",
        team1_id == 6 ~ "Keep",
        team1_id == 2579 ~ "Keep",
        team1_id == 2569 ~ "Keep",
        team1_id == 233 ~ "Keep",
        team1_id == 2571 ~ "Keep",
        team1_id == 58 ~ "Keep",
        team1_id == 2546 ~ "Keep",
        team1_id == 2545 ~ "Keep",
        team1_id == 2582 ~ "Keep",
        team1_id == 79 ~ "Keep",
        team1_id == 2572 ~ "Keep",
        team1_id == 253 ~ "Keep",
        team1_id == 179 ~ "Keep",
        team1_id == 2597 ~ "Keep",
        team1_id == 2598 ~ "Keep",
        team1_id == 2599 ~ "Keep",
        team1_id == 2900 ~ "Keep",
        team1_id == 24 ~ "Keep",
        team1_id == 2617 ~ "Keep",
        team1_id == 56 ~ "Keep",
        team1_id == 2619 ~ "Keep",
        team1_id == 183 ~ "Keep",
        team1_id == 2627 ~ "Keep",
        team1_id == 2628 ~ "Keep",
        team1_id == 218 ~ "Keep",
        team1_id == 2633 ~ "Keep",
        team1_id == 2630 ~ "Keep",
        team1_id == 2634 ~ "Keep",
        team1_id == 2635 ~ "Keep",
        team1_id == 251 ~ "Keep",
        team1_id == 245 ~ "Keep",
        team1_id == 357 ~ "Keep",
        team1_id == 2640 ~ "Keep",
        team1_id == 326 ~ "Keep",
        team1_id == 2641 ~ "Keep",
        team1_id == 2643 ~ "Keep",
        team1_id == 2649 ~ "Keep",
        team1_id == 119 ~ "Keep",
        team1_id == 2653 ~ "Keep",
        team1_id == 2655 ~ "Keep",
        team1_id == 202 ~ "Keep",
        team1_id == 5 ~ "Keep",
        team1_id == 302 ~ "Keep",
        team1_id == 300 ~ "Keep",
        team1_id == 27 ~ "Keep",
        team1_id == 28 ~ "Keep",
        team1_id == 2540 ~ "Keep",
        team1_id == 2116 ~ "Keep",
        team1_id == 26 ~ "Keep",
        team1_id == 2349 ~ "Keep",
        team1_id == 2378 ~ "Keep",
        team1_id == 140 ~ "Keep",
        team1_id == 2427 ~ "Keep",
        team1_id == 2430 ~ "Keep",
        team1_id == 350 ~ "Keep",
        team1_id == 2439 ~ "Keep",
        team1_id == 30 ~ "Keep",
        team1_id == 2908 ~ "Keep",
        team1_id == 250 ~ "Keep",
        team1_id == 292 ~ "Keep",
        team1_id == 254 ~ "Keep",
        team1_id == 328 ~ "Keep",
        team1_id == 3084 ~ "Keep",
        team1_id == 2638 ~ "Keep",
        team1_id == 2636 ~ "Keep",
        team1_id == 2674 ~ "Keep",
        team1_id == 238 ~ "Keep",
        team1_id == 2670 ~ "Keep",
        team1_id == 261 ~ "Keep",
        team1_id == 222 ~ "Keep",
        team1_id == 258 ~ "Keep",
        team1_id == 259 ~ "Keep",
        team1_id == 2678 ~ "Keep",
        team1_id == 2681 ~ "Keep",
        team1_id == 154 ~ "Keep",
        team1_id == 264 ~ "Keep",
        team1_id == 265 ~ "Keep",
        team1_id == 2692 ~ "Keep",
        team1_id == 277 ~ "Keep",
        team1_id == 2717 ~ "Keep",
        team1_id == 2710 ~ "Keep",
        team1_id == 98 ~ "Keep",
        team1_id == 2711 ~ "Keep",
        team1_id == 2724 ~ "Keep",
        team1_id == 2729 ~ "Keep",
        team1_id == 2736 ~ "Keep",
        team1_id == 2737 ~ "Keep",
        team1_id == 275 ~ "Keep",
        team1_id == 2747 ~ "Keep",
        team1_id == 2750 ~ "Keep",
        team1_id == 2751 ~ "Keep",
        team1_id == 2752 ~ "Keep",
        team1_id == 43 ~ "Keep",
        team1_id == 2754 ~ "Keep"
        )
    )
  df_box_score <- df_box_score %>%
    dplyr::mutate(
      status2 = dplyr::case_when(
        team2_id == 2000 ~ "Keep",
        team2_id == 2005 ~ "Keep",
        team2_id == 2006 ~ "Keep",
        team2_id == 333 ~ "Keep",
        team2_id == 2010 ~ "Keep",
        team2_id == 2011 ~ "Keep",
        team2_id == 399 ~ "Keep",
        team2_id == 2016 ~ "Keep",
        team2_id == 44 ~ "Keep",
        team2_id == 2026 ~ "Keep",
        team2_id == 12 ~ "Keep",
        team2_id == 9 ~ "Keep",
        team2_id == 8 ~ "Keep",
        team2_id == 2031 ~ "Keep",
        team2_id == 2029 ~ "Keep",
        team2_id == 2032 ~ "Keep",
        team2_id == 349 ~ "Keep",
        team2_id == 2 ~ "Keep",
        team2_id == 2046 ~ "Keep",
        team2_id == 2050 ~ "Keep",
        team2_id == 239 ~ "Keep",
        team2_id == 91 ~ "Keep",
        team2_id == 2057 ~ "Keep",
        team2_id == 2065 ~ "Keep",
        team2_id == 2066 ~ "Keep",
        team2_id == 68 ~ "Keep",
        team2_id == 103 ~ "Keep",
        team2_id == 104 ~ "Keep",
        team2_id == 189 ~ "Keep",
        team2_id == 71 ~ "Keep",
        team2_id == 225 ~ "Keep",
        team2_id == 2803 ~ "Keep",
        team2_id == 2083 ~ "Keep",
        team2_id == 2084 ~ "Keep",
        team2_id == 2086 ~ "Keep",
        team2_id == 252 ~ "Keep",
        team2_id == 2856 ~ "Keep",
        team2_id == 13 ~ "Keep",
        team2_id == 2934 ~ "Keep",
        team2_id == 2239 ~ "Keep",
        team2_id == 2463 ~ "Keep",
        team2_id == 25 ~ "Keep",
        team2_id == 2097 ~ "Keep",
        team2_id == 2099 ~ "Keep",
        team2_id == 2113 ~ "Keep",
        team2_id == 2110 ~ "Keep",
        team2_id == 2115 ~ "Keep",
        team2_id == 2117 ~ "Keep",
        team2_id == 2127 ~ "Keep",
        team2_id == 2429 ~ "Keep",
        team2_id == 236 ~ "Keep",
        team2_id == 2130 ~ "Keep",
        team2_id == 2132 ~ "Keep",
        team2_id == 228 ~ "Keep",
        team2_id == 325 ~ "Keep",
        team2_id == 324 ~ "Keep",
        team2_id == 2142 ~ "Keep",
        team2_id == 232 ~ "Keep",
        team2_id == 38 ~ "Keep",
        team2_id == 36 ~ "Keep",
        team2_id == 171 ~ "Keep",
        team2_id == 41 ~ "Keep",
        team2_id == 2154 ~ "Keep",
        team2_id == 172 ~ "Keep",
        team2_id == 156 ~ "Keep",
        team2_id == 159 ~ "Keep",
        team2_id == 2166 ~ "Keep",
        team2_id == 2168 ~ "Keep",
        team2_id == 48 ~ "Keep",
        team2_id == 2169 ~ "Keep",
        team2_id == 2172 ~ "Keep",
        team2_id == 305 ~ "Keep",
        team2_id == 2174 ~ "Keep",
        team2_id == 3101 ~ "Keep",
        team2_id == 2181 ~ "Keep",
        team2_id == 2182 ~ "Keep",
        team2_id == 150 ~ "Keep",
        team2_id == 2184 ~ "Keep",
        team2_id == 151 ~ "Keep",
        team2_id == 2193 ~ "Keep",
        team2_id == 2197 ~ "Keep",
        team2_id == 2198 ~ "Keep",
        team2_id == 2199 ~ "Keep",
        team2_id == 331 ~ "Keep",
        team2_id == 2210 ~ "Keep",
        team2_id == 339 ~ "Keep",
        team2_id == 2217 ~ "Keep",
        team2_id == 161 ~ "Keep",
        team2_id == 2229 ~ "Keep",
        team2_id == 57 ~ "Keep",
        team2_id == 50 ~ "Keep",
        team2_id == 2226 ~ "Keep",
        team2_id == 526 ~ "Keep",
        team2_id == 52 ~ "Keep",
        team2_id == 2230 ~ "Keep",
        team2_id == 2870 ~ "Keep",
        team2_id == 278 ~ "Keep",
        team2_id == 231 ~ "Keep",
        team2_id == 2241 ~ "Keep",
        team2_id == 2244 ~ "Keep",
        team2_id == 45 ~ "Keep",
        team2_id == 46 ~ "Keep",
        team2_id == 61 ~ "Keep",
        team2_id == 290 ~ "Keep",
        team2_id == 2247 ~ "Keep",
        team2_id == 59 ~ "Keep",
        team2_id == 2250 ~ "Keep",
        team2_id == 2755 ~ "Keep",
        team2_id == 2253 ~ "Keep",
        team2_id == 2739 ~ "Keep",
        team2_id == 2261 ~ "Keep",
        team2_id == 42 ~ "Keep",
        team2_id == 108 ~ "Keep",
        team2_id == 62 ~ "Keep",
        team2_id == 2272 ~ "Keep",
        team2_id == 2275 ~ "Keep",
        team2_id == 107 ~ "Keep",
        team2_id == 248 ~ "Keep",
        team2_id == 2277 ~ "Keep",
        team2_id == 47 ~ "Keep",
        team2_id == 70 ~ "Keep",
        team2_id == 304 ~ "Keep",
        team2_id == 356 ~ "Keep",
        team2_id == 82 ~ "Keep",
        team2_id == 2287 ~ "Keep",
        team2_id == 2916 ~ "Keep",
        team2_id == 84 ~ "Keep",
        team2_id == 282 ~ "Keep",
        team2_id == 314 ~ "Keep",
        team2_id == 2294 ~ "Keep",
        team2_id == 66 ~ "Keep",
        team2_id == 85 ~ "Keep",
        team2_id == 2296 ~ "Keep",
        team2_id == 294 ~ "Keep",
        team2_id == 55 ~ "Keep",
        team2_id == 256 ~ "Keep",
        team2_id == 2305 ~ "Keep",
        team2_id == 2306 ~ "Keep",
        team2_id == 338 ~ "Keep",
        team2_id == 2309 ~ "Keep",
        team2_id == 96 ~ "Keep",
        team2_id == 2325 ~ "Keep",
        team2_id == 322 ~ "Keep",
        team2_id == 2320 ~ "Keep",
        team2_id == 2329 ~ "Keep",
        team2_id == 2335 ~ "Keep",
        team2_id == 288 ~ "Keep",
        team2_id == 2031 ~ "Keep",
        team2_id == 112358 ~ "Keep",
        team2_id == 299 ~ "Keep",
        team2_id == 2344 ~ "Keep",
        team2_id == 322 ~ "Keep",
        team2_id == 2433 ~ "Keep",
        team2_id == 2348 ~ "Keep",
        team2_id == 97 ~ "Keep",
        team2_id == 2350 ~ "Keep",
        team2_id == 2351 ~ "Keep",
        team2_id == 2352 ~ "Keep",
        team2_id == 99 ~ "Keep",
        team2_id == 311 ~ "Keep",
        team2_id == 2363 ~ "Keep",
        team2_id == 2368 ~ "Keep",
        team2_id == 269 ~ "Keep",
        team2_id == 276 ~ "Keep",
        team2_id == 120 ~ "Keep",
        team2_id == 2379 ~ "Keep",
        team2_id == 113 ~ "Keep",
        team2_id == 2377 ~ "Keep",
        team2_id == 235 ~ "Keep",
        team2_id == 2382 ~ "Keep",
        team2_id == 2771 ~ "Keep",
        team2_id == 2390 ~ "Keep",
        team2_id == 193 ~ "Keep",
        team2_id == 130 ~ "Keep",
        team2_id == 127 ~ "Keep",
        team2_id == 2393 ~ "Keep",
        team2_id == 270 ~ "Keep",
        team2_id == 135 ~ "Keep",
        team2_id == 145 ~ "Keep",
        team2_id == 344 ~ "Keep",
        team2_id == 2400 ~ "Keep",
        team2_id == 142 ~ "Keep",
        team2_id == 2623 ~ "Keep",
        team2_id == 2405 ~ "Keep",
        team2_id == 149 ~ "Keep",
        team2_id == 147 ~ "Keep",
        team2_id == 2413 ~ "Keep",
        team2_id == 2415 ~ "Keep",
        team2_id == 116 ~ "Keep",
        team2_id == 93 ~ "Keep",
        team2_id == 2426 ~ "Keep",
        team2_id == 158 ~ "Keep",
        team2_id == 2437 ~ "Keep",
        team2_id == 2440 ~ "Keep",
        team2_id == 160 ~ "Keep",
        team2_id == 167 ~ "Keep",
        team2_id == 166 ~ "Keep",
        team2_id == 2443 ~ "Keep",
        team2_id == 315 ~ "Keep",
        team2_id == 2447 ~ "Keep",
        team2_id == 2885 ~ "Keep",
        team2_id == 2450 ~ "Keep",
        team2_id == 2453 ~ "Keep",
        team2_id == 153 ~ "Keep",
        team2_id == 2448 ~ "Keep",
        team2_id == 2428 ~ "Keep",
        team2_id == 152 ~ "Keep",
        team2_id == 155 ~ "Keep",
        team2_id == 2449 ~ "Keep",
        team2_id == 2454 ~ "Keep",
        team2_id == 249 ~ "Keep",
        team2_id == 111 ~ "Keep",
        team2_id == 2464 ~ "Keep",
        team2_id == 2458 ~ "Keep",
        team2_id == 2459 ~ "Keep",
        team2_id == 2460 ~ "Keep",
        team2_id == 94 ~ "Keep",
        team2_id == 77 ~ "Keep",
        team2_id == 2466 ~ "Keep",
        team2_id == 87 ~ "Keep",
        team2_id == 2473 ~ "Keep",
        team2_id == 195 ~ "Keep",
        team2_id == 194 ~ "Keep",
        team2_id == 201 ~ "Keep",
        team2_id == 197 ~ "Keep",
        team2_id == 295 ~ "Keep",
        team2_id == 198 ~ "Keep",
        team2_id == 2483 ~ "Keep",
        team2_id == 204 ~ "Keep",
        team2_id == 279 ~ "Keep",
        team2_id == 219 ~ "Keep",
        team2_id == 213 ~ "Keep",
        team2_id == 2492 ~ "Keep",
        team2_id == 221 ~ "Keep",
        team2_id == 2501 ~ "Keep",
        team2_id == 2502 ~ "Keep",
        team2_id == 2504 ~ "Keep",
        team2_id == 2506 ~ "Keep",
        team2_id == 163 ~ "Keep",
        team2_id == 2507 ~ "Keep",
        team2_id == 2509 ~ "Keep",
        team2_id == 2514 ~ "Keep",
        team2_id == 2515 ~ "Keep",
        team2_id == 227 ~ "Keep",
        team2_id == 242 ~ "Keep",
        team2_id == 257 ~ "Keep",
        team2_id == 2520 ~ "Keep",
        team2_id == 2523 ~ "Keep",
        team2_id == 164 ~ "Keep",
        team2_id == 16 ~ "Keep",
        team2_id == 2529 ~ "Keep",
        team2_id == 2603 ~ "Keep",
        team2_id == 139 ~ "Keep",
        team2_id == 2608 ~ "Keep",
        team2_id == 2612 ~ "Keep",
        team2_id == 2534 ~ "Keep",
        team2_id == 2535 ~ "Keep",
        team2_id == 301 ~ "Keep",
        team2_id == 21 ~ "Keep",
        team2_id == 2539 ~ "Keep",
        team2_id == 23 ~ "Keep",
        team2_id == 2541 ~ "Keep",
        team2_id == 2542 ~ "Keep",
        team2_id == 2547 ~ "Keep",
        team2_id == 2550 ~ "Keep",
        team2_id == 2561 ~ "Keep",
        team2_id == 2565 ~ "Keep",
        team2_id == 2567 ~ "Keep",
        team2_id == 6 ~ "Keep",
        team2_id == 2579 ~ "Keep",
        team2_id == 2569 ~ "Keep",
        team2_id == 233 ~ "Keep",
        team2_id == 2571 ~ "Keep",
        team2_id == 58 ~ "Keep",
        team2_id == 2546 ~ "Keep",
        team2_id == 2545 ~ "Keep",
        team2_id == 2582 ~ "Keep",
        team2_id == 79 ~ "Keep",
        team2_id == 2572 ~ "Keep",
        team2_id == 253 ~ "Keep",
        team2_id == 179 ~ "Keep",
        team2_id == 2597 ~ "Keep",
        team2_id == 2598 ~ "Keep",
        team2_id == 2599 ~ "Keep",
        team2_id == 2900 ~ "Keep",
        team2_id == 24 ~ "Keep",
        team2_id == 2617 ~ "Keep",
        team2_id == 56 ~ "Keep",
        team2_id == 2619 ~ "Keep",
        team2_id == 183 ~ "Keep",
        team2_id == 2627 ~ "Keep",
        team2_id == 2628 ~ "Keep",
        team2_id == 218 ~ "Keep",
        team2_id == 2633 ~ "Keep",
        team2_id == 2630 ~ "Keep",
        team2_id == 2634 ~ "Keep",
        team2_id == 2635 ~ "Keep",
        team2_id == 251 ~ "Keep",
        team2_id == 245 ~ "Keep",
        team2_id == 357 ~ "Keep",
        team2_id == 2640 ~ "Keep",
        team2_id == 326 ~ "Keep",
        team2_id == 2641 ~ "Keep",
        team2_id == 2643 ~ "Keep",
        team2_id == 2649 ~ "Keep",
        team2_id == 119 ~ "Keep",
        team2_id == 2653 ~ "Keep",
        team2_id == 2655 ~ "Keep",
        team2_id == 202 ~ "Keep",
        team2_id == 5 ~ "Keep",
        team2_id == 302 ~ "Keep",
        team2_id == 300 ~ "Keep",
        team2_id == 27 ~ "Keep",
        team2_id == 28 ~ "Keep",
        team2_id == 2540 ~ "Keep",
        team2_id == 2116 ~ "Keep",
        team2_id == 26 ~ "Keep",
        team2_id == 2349 ~ "Keep",
        team2_id == 2378 ~ "Keep",
        team2_id == 140 ~ "Keep",
        team2_id == 2427 ~ "Keep",
        team2_id == 2430 ~ "Keep",
        team2_id == 350 ~ "Keep",
        team2_id == 2439 ~ "Keep",
        team2_id == 30 ~ "Keep",
        team2_id == 2908 ~ "Keep",
        team2_id == 250 ~ "Keep",
        team2_id == 292 ~ "Keep",
        team2_id == 254 ~ "Keep",
        team2_id == 328 ~ "Keep",
        team2_id == 3084 ~ "Keep",
        team2_id == 2638 ~ "Keep",
        team2_id == 2636 ~ "Keep",
        team2_id == 2674 ~ "Keep",
        team2_id == 238 ~ "Keep",
        team2_id == 2670 ~ "Keep",
        team2_id == 261 ~ "Keep",
        team2_id == 222 ~ "Keep",
        team2_id == 258 ~ "Keep",
        team2_id == 259 ~ "Keep",
        team2_id == 2678 ~ "Keep",
        team2_id == 2681 ~ "Keep",
        team2_id == 154 ~ "Keep",
        team2_id == 264 ~ "Keep",
        team2_id == 265 ~ "Keep",
        team2_id == 2692 ~ "Keep",
        team2_id == 277 ~ "Keep",
        team2_id == 2717 ~ "Keep",
        team2_id == 2710 ~ "Keep",
        team2_id == 98 ~ "Keep",
        team2_id == 2711 ~ "Keep",
        team2_id == 2724 ~ "Keep",
        team2_id == 2729 ~ "Keep",
        team2_id == 2736 ~ "Keep",
        team2_id == 2737 ~ "Keep",
        team2_id == 275 ~ "Keep",
        team2_id == 2747 ~ "Keep",
        team2_id == 2750 ~ "Keep",
        team2_id == 2751 ~ "Keep",
        team2_id == 2752 ~ "Keep",
        team2_id == 43 ~ "Keep",
        team2_id == 2754 ~ "Keep"
        )
    )
  df_d1_only <- df_box_score%>%
    filter(status1 == "Keep") %>%
    filter(status2 == "Keep") %>%
    select(-status1, -status2)
  write_csv(df_d1_only, paste0("../../Inputs/NCAA/espn_mbb_d1_box_score_", season, ".csv"))
  

}





###################
# NCAA play-by-play data
#################################
progressr::with_progress({
  mbb_pbp <-  hoopR::load_mbb_pbp(2008)
})

future::plan("multisession")
gamezoneR:::available_seasons()
progressr::with_progress({
  pbp <- gamezoneR::load_gamezone_pbp(gamezoneR:::available_seasons())
})
length(unique(pbp$game_id))