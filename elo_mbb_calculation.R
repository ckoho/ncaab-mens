#elo_mbb_calculation.R
#This computes the ELO ranking for MBB using torvik data.

library(tidyverse)
library(readr)
library(vroom)
library(fs)

#df_year_box_score, df_ratings, home_court, k, l, m
df <- df_year_box_score
df1 <- df_ratings
j <- 1
year_elo_ratings <- function(df, df1, home_court, k, l, m){
  for (j in 1:nrow(df)) {
    #Get each team name and rating .
    team1 = df[[j, "team1"]]
    team2 = df[[j, "team2"]]
    
    #######
    # ELO_adjusted part.
    #######
    team2_rating = df1[[which(df1 == team2, arr.ind=TRUE)[1], "elo_adjusted"]]
    team1_rating = df1[[which(df1 == team1, arr.ind=TRUE)[1], "elo_adjusted"]]
    
    #Calculating home court advantage into win percentage. Rest of calculation 
    #is the same. Line calculation also needs to factor in home court. Taking 
    #rating delta time 0.046455 to get line.
    if (df[[j, "loc"]] == "A") {
      team2_win_per = 1 / (1 + 10 ^ ((team1_rating - 
                                        (team2_rating + home_court))/400))
      df[[j, "elo_adjusted_line"]] <- 
        (team1_rating - (team2_rating + home_court)) * 0.046455
      
    } else if (df[[j, "loc"]] == "N") {
      team2_win_per = 1 / (1 + 10 ^ ((team1_rating - (team2_rating))/400))
      df[[j, "elo_adjusted_line"]] <- (team1_rating - (team2_rating)) * 0.046455
    } else if (df[[j, "loc"]] == "H") {
      team2_win_per = 1 / (1 + 10 ^ (((team1_rating + home_court)- 
                                        (team2_rating))/400))
      df[[j, "elo_adjusted_line"]] <- ((team1_rating + home_court) - 
                            (team2_rating)) * 0.046455
    }
    
    
    #Put pre rating and odds into box score data frame.
    df[[j, "team1_rating_elo_adjusted"]] <- team1_rating
    df[[j, "team2_rating_elo_adjusted"]] <- team2_rating
    df[[j, "team1_odds_elo_adjusted"]] <- 1 - team2_win_per
    df[[j, "team2_odds_elo_adjusted"]] <- team2_win_per
    
    #Update elo rating based on game result.
    if (df[[j, "win"]] == team2) {
      #1 means team2 (home) win, 0 means team1 (road) win
      #>.5 is team2 (home) is favorite, <.5 is team1 (road) favorite
      df[[j, "result"]] <- 1
      
      #See if they cover the line.
      if(df[[j, "team1_pts"]] - df[[j, "team2_pts"]] < 
         df[[j, "elo_adjusted_line"]]){
        adjust <- round((1 + l) * 
                          log(df[[j, "team2_pts"]] - df[[j, "team1_pts"]] +
                                        1) * k * (1 - team2_win_per), 0)
      } else {
        adjust <- round((1 - l) * 
                          log(df[[j, "team2_pts"]] - df[[j, "team1_pts"]] +
                                        1) * k * (1 - team2_win_per), 0)
      }
      
      df1[[which(df1 == team1, arr.ind=TRUE)[1], "elo_adjusted"]] <- 
        df1[[which(df1 == team1, arr.ind=TRUE)[1], "elo_adjusted"]] - adjust
      df1[[which(df1 == team2, arr.ind=TRUE)[1], "elo_adjusted"]] <- adjust +
        df1[[which(df1 == team2, arr.ind=TRUE)[1], "elo_adjusted"]]
    } else {
      #See if they cover the line.
      if(df[[j, "team1_pts"]] - df[[j, "team2_pts"]] > 
         df[[j, "elo_adjusted_line"]]){
        adjust <- round((1 + l) * 
                          log(df[[j, "team1_pts"]] - df[[j, "team2_pts"]] +
                                        1) * k * (team2_win_per), 0)
      } else {
        adjust <- round((1 - l) * 
                          log(df[[j, "team1_pts"]] - df[[j, "team2_pts"]] +
                                        1) * k * (team2_win_per), 0)
      }
      
      df1[[which(df1 == team1, arr.ind=TRUE)[1], "elo_adjusted"]] <- adjust +
        df1[[which(df1 == team1, arr.ind=TRUE)[1], "elo_adjusted"]]
      df1[[which(df1 == team2, arr.ind=TRUE)[1], "elo_adjusted"]] <- 
        df1[[which(df1 == team2, arr.ind=TRUE)[1], "elo_adjusted"]] - adjust
      
    }
    #######
    # ELO_line part.
    #######
    team1_rating = df1[[which(df1 == team1, arr.ind=TRUE)[1], "elo_line"]]
    team2_rating = df1[[which(df1 == team2, arr.ind=TRUE)[1], "elo_line"]]
    
    #Calculating home court advantage into win percentage. Rest of calculation 
    #is the same.
    if (df[[j, "loc"]] == "A") {
      team2_win_per = 1 / (1 + 10 ^ ((team1_rating - 
                                        (team2_rating + home_court))/400))
      df[[j, "elo_line_line"]] <- (team1_rating - (team2_rating + home_court)) * 0.046455
      
    } else if (df[[j, "loc"]] == "N") {
      team2_win_per = 1 / (1 + 10 ^ ((team1_rating - (team2_rating))/400))
      df[[j, "elo_line_line"]] <- (team1_rating - (team2_rating)) * 0.046455
      
    } else if (df[[j, "loc"]] == "H") {
      team2_win_per = 1 / (1 + 10 ^ (((team1_rating + home_court) - 
                                        (team2_rating))/400))
      df[[j, "elo_line_line"]] <- ((team1_rating + home_court) - 
                            (team2_rating)) * 0.046455
      
    }
    
    df[[j, "team1_rating_elo_line"]] <- team1_rating
    df[[j, "team2_rating_elo_line"]] <- team2_rating
    df[[j, "team1_odds_elo_line"]] <- 1 - team2_win_per
    df[[j, "team2_odds_elo_line"]] <- team2_win_per
    
    if (df[[j, "elo_line_line"]] > 
        (df[[j, "team1_pts"]] - df[[j, "team2_pts"]])) {
      #Win per should always be 50/50 for this method.
      delta <- df[[j, "elo_line_line"]] - (df[[j, "team1_pts"]] - df[[j, "team2_pts"]])
      adjust <- round(log(delta + 1) * m * (.5), 0)
      #adjust <- round(k * (.5), 0)
      df1[[which(df1 == team1, arr.ind=TRUE)[1], "elo_line"]] <- 
        df1[[which(df1 == team1, arr.ind=TRUE)[1], "elo_line"]] - adjust
      df1[[which(df1 == team2, arr.ind=TRUE)[1], "elo_line"]] <- adjust +
        df1[[which(df1 == team2, arr.ind=TRUE)[1], "elo_line"]]
    } else {
      #Win per should always be 50/50 for this method.
      delta <- abs(df[[j, "elo_line_line"]] - (df[[j, "team1_pts"]] - 
                                        df[[j, "team2_pts"]]))
      
      adjust <- round(log(delta + 1) * m * (.5), 0)
      df1[[which(df1 == team1, arr.ind=TRUE)[1], "elo_line"]] <- adjust +
        df1[[which(df1 == team1, arr.ind=TRUE)[1], "elo_line"]]
      df1[[which(df1 == team2, arr.ind=TRUE)[1], "elo_line"]] <- 
        df1[[which(df1 == team2, arr.ind=TRUE)[1], "elo_line"]] - adjust
      
    }
    

    #######
    # ELO_combined part.
    #######
    df[[j, "team1_rating_elo_combined"]] <- (df[[j, "team1_rating_elo_line"]] + 
      df[[j, "team1_rating_elo_adjusted"]])/2  
    df[[j, "team2_rating_elo_combined"]] <- 
      (df[[j, "team2_rating_elo_line"]] + 
         df[[j, "team2_rating_elo_adjusted"]])/2 
    df[[j, "team1_odds_elo_combined"]] <- 
      (df[[j, "team1_odds_elo_adjusted"]] + df[[j, "team1_odds_elo_line"]] )/2
    df[[j, "team2_odds_elo_combined"]] <- 
      (df[[j, "team2_odds_elo_adjusted"]] + df[[j, "team2_odds_elo_line"]] )/2
    
    df[[j, "elo_combined_line"]] <- 
      (df[[j, "elo_line_line"]] + df[[j, "elo_adjusted_line"]] )/2
    df[[j, "team2_odds_elo_combined"]] <- 
      (df[[j, "team2_odds_elo_adjusted"]] + df[[j, "team2_odds_elo_line"]] )/2
    
    df1[[which(df1 == team2, arr.ind=TRUE)[1], "elo_combined"]] <- 
      (df1[[which(df1 == team2, arr.ind=TRUE)[1], "elo_adjusted"]] + 
      df1[[which(df1 == team2, arr.ind=TRUE)[1], "elo_line"]]) /2
    
    df1[[which(df1 == team1, arr.ind=TRUE)[1], "elo_combined"]] <- 
      (df1[[which(df1 == team1, arr.ind=TRUE)[1], "elo_adjusted"]] + 
         df1[[which(df1 == team1, arr.ind=TRUE)[1], "elo_line"]]) /2
    
    
    #######
    # Standard record keeping updates.
    #######
    
    #Adjust records (games played, W/L, conf W/L, and elo rating through gp.)
    if(df[[j, "win"]] == team2 ){
      if (df[[j, "type"]] == "conf"){
        df1[[which(df1 == team2, arr.ind=TRUE)[1], "conf_w"]] <- 
          df1[[which(df1 == team2, arr.ind=TRUE)[1], "conf_w"]] + 1
        df1[[which(df1 == team1, arr.ind=TRUE)[1], "conf_l"]] <- 
          df1[[which(df1 == team1, arr.ind=TRUE)[1], "conf_l"]] + 1
      }
      df1[[which(df1 == team2, arr.ind=TRUE)[1], "w"]] <- 
        df1[[which(df1 == team2, arr.ind=TRUE)[1], "w"]] + 1
      df1[[which(df1 == team1, arr.ind=TRUE)[1], "l"]] <- 
        df1[[which(df1 == team1, arr.ind=TRUE)[1], "l"]] + 1
    }else{
      if (df[[j, "type"]] == "conf"){
        df1[[which(df1 == team2, arr.ind=TRUE)[1], "conf_l"]] <- 
          df1[[which(df1 == team2, arr.ind=TRUE)[1], "conf_l"]] + 1
        df1[[which(df1 == team1, arr.ind=TRUE)[1], "conf_w"]] <- 
          df1[[which(df1 == team1, arr.ind=TRUE)[1], "conf_w"]] + 1
      }
      df1[[which(df1 == team2, arr.ind=TRUE)[1], "l"]] <- 
        df1[[which(df1 == team2, arr.ind=TRUE)[1], "l"]] + 1
      df1[[which(df1 == team1, arr.ind=TRUE)[1], "w"]] <- 
        df1[[which(df1 == team1, arr.ind=TRUE)[1], "w"]] + 1
    }
    
    df1[[which(df1 == team1, arr.ind=TRUE)[1], "gp"]] <-
      df1[[which(df1 == team1, arr.ind=TRUE)[1], "gp"]] + 1
    df1[[which(df1 == team2, arr.ind=TRUE)[1], "gp"]] <-
      df1[[which(df1 == team2, arr.ind=TRUE)[1], "gp"]] + 1
    
  }

  #Save df and return df1
  #Select less values from df before saving.
  df <- df %>%
    select(type, loc, season, date, win, loss, team1_conf, team1, team1_pts, 
           team2_conf, team2, team2_pts,
           result,
           year,
           elo_line_line,
           elo_combined_line,
           elo_adjusted_line,
           team1_rating_elo_adjusted,
           team2_rating_elo_adjusted,
           team1_odds_elo_adjusted,
           team2_odds_elo_adjusted,
           team1_rating_elo_line,
           team2_rating_elo_line,
           team1_odds_elo_line,
           team2_odds_elo_line,
           team1_rating_elo_combined,
           team2_rating_elo_combined,
           team2_odds_elo_combined,
           team1_odds_elo_combined)
  
  write_csv(df, paste0("elo_results_eoy_", season, "_mbb_box_score.csv"))
  return(df1)
}

####################################################
### Starts Here                                  ###
####################################################
df_ratings <- vroom(
  "C:/Users/ckoho/Documents/Inputs/NCAA/mbb_elo_torvik_default.csv", 
  altrep = FALSE)
df_ratings <- vroom(
  "C:/Users/ckoho/Documents/Inputs/NCAA/mbb_elo_torvik.csv", 
  altrep = FALSE)

season <- 2008
k <- 17
l <- .4
m <- 19
#season <- 2025
for (season in 2019:2024) {
if (season > 2021 ) {
  home_court <- 49
} else{
  home_court <- 70
}
df_year_box_score <-
  vroom(
    paste0(
      "C:/Users/ckoho/Documents/Inputs/NCAA/Torvik/torvik_box_score_",
      season,
      ".csv"
    )
  )
#Get the team name and conference for the year
df_team_conf <- df_year_box_score %>%
  select(team1, team1_conf, team2, team2_conf) %>%
  pivot_longer(cols = c(team1, team2), 
               names_to = "team_name", 
               values_to = "team") %>%
  pivot_longer(cols = c(team1_conf, team2_conf), 
               names_to = "team_conf_name", 
               values_to = "team_conf") %>%
  filter(substr(team_name, 5, 5) == 
           substr(team_conf_name, 5, 5)) %>% 
  select(team, team_conf) %>%
  distinct()

if (season == "2009") {
  #Update ind conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "ind") %>%
    filter(team != "Bryant") %>%
    filter(team != "Houston Christian") %>%
    filter(team != "SIU Edwardsville") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  
  #Set Bryant to ind conference rating
  df_ratings[[which(df_ratings == "Bryant", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "Bryant", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "Bryant", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
  
  #Set Houston Christian to ind conference rating
  df_ratings[[which(df_ratings == "Houston Christian", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "Houston Christian", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "Houston Christian", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
  
  #Set SIU to ind conference rating
  df_ratings[[which(df_ratings == "SIU Edwardsville", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "SIU Edwardsville", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "SIU Edwardsville", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
  
  #Update Sum conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "Sum") %>%
    filter(team != "IPFW") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  
  #Set IPFW to Summit conference rating
  df_ratings[[which(df_ratings == "IPFW", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "IPFW", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "IPFW", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
} else if (season == "2010") {
  #Update GWC conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "GWC") %>%
    filter(team != "North Dakota") %>%
    filter(team != "South Dakota") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  
  #Set North Dakota to GWC conference rating
  df_ratings[[which(df_ratings == "North Dakota", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "North Dakota", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "North Dakota", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
  #Set South Dakota to GWC conference rating
  df_ratings[[which(df_ratings == "South Dakota", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "South Dakota", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "South Dakota", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
  
  #Update ind conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "ind") %>%
    filter(team != "Seattle") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  #Set Seattle to ind conference rating
  df_ratings[[which(df_ratings == "Seattle", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "Seattle", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "Seattle", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
} else if (season == "2012") {
  #Update ind conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "ind") %>%
    filter(team != "Nebraska Omaha") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  
  #Set Nebraska Omaha to ind conference rating
  df_ratings[[which(df_ratings == "Nebraska Omaha", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "Nebraska Omaha", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "Nebraska Omaha", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
} else if (season == "2013") {
  #Update ASun conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "ASun") %>%
    filter(team != "Northern Kentucky") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  
  #Set Northern Kentucky to ASun conference rating
  df_ratings[[which(df_ratings == "Northern Kentucky", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "Northern Kentucky", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "Northern Kentucky", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
} else if (season == "2014") {
  #Update ASun conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "Slnd") %>%
    filter(team != "Abilene Christian") %>%
    filter(team != "Incarnate Word") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  
  #Set Abilene Christian to Slnd conference rating
  df_ratings[[which(df_ratings == "Abilene Christian", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "Abilene Christian", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "Abilene Christian", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
  #Set Incarnate Word to Slnd conference rating
  df_ratings[[which(df_ratings == "Incarnate Word", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "Incarnate Word", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "Incarnate Word", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
  
  #Update WAC conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "WAC") %>%
    filter(team != "Grand Canyon") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  
  #Set Grand Canyon to WAC conference rating
  df_ratings[[which(df_ratings == "Grand Canyon", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "Grand Canyon", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "Grand Canyon", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
  
  #Update AE conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "AE") %>%
    filter(team != "UMass Lowell") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  #Set UMass Lowell to AE conference rating
  df_ratings[[which(df_ratings == "UMass Lowell", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "UMass Lowell", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "UMass Lowell", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
} else if (season == "2017") {
  #Set Fort Wayne to IPFW rating.
  #TODO Need to clean up how this is handled later.
  #TODO Maybe just celan up team names???
  df_conference_teams <- df_team_conf %>%
    filter(team != "UMass Lowell") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team == "IPFW" ) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  
  df_ratings[[which(df_ratings == "Fort Wayne", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "Fort Wayne", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "Fort Wayne", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
} else if (season == "2019") {
  #Update WAC conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "WAC") %>%
    filter(team != "Cal Baptist") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  #Set Cal Baptist to WAC conference rating
  df_ratings[[which(df_ratings == "Cal Baptist", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "Cal Baptist", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "Cal Baptist", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
  
  #Update ASun conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "ASun") %>%
    filter(team != "North Alabama") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  #Set North Alabama to ASun conference rating
  df_ratings[[which(df_ratings == "North Alabama", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "North Alabama", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "North Alabama", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
} else if (season == "2020") {
  #Update NEC conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "NEC") %>%
    filter(team != "Merrimack") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  #Set Merrimack to NEC conference rating
  df_ratings[[which(df_ratings == "Merrimack", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "Merrimack", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "Merrimack", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
} else if (season == "2021") {
  #Update WAC conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "WAC") %>%
    filter(team != "Tarleton St.") %>%
    filter(team != "Utah Tech") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  #Set Tarleton St. to WAC conference rating
  df_ratings[[which(df_ratings == "Tarleton St.", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "Tarleton St.", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "Tarleton St.", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
  
  #Set Utah Valley to WAC conference rating
  df_ratings[[which(df_ratings == "Utah Valley", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "Utah Valley", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "Utah Valley", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
  
  #Update BW conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "BW") %>%
    filter(team != "UC San Diego") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  #Set UC San Diego to BW conference rating
  df_ratings[[which(df_ratings == "UC San Diego", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "UC San Diego", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "UC San Diego", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
  
  #Update ASun conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "ASun") %>%
    filter(team != "Bellarmine") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  #Set Bellarmine to ASun conference rating
  df_ratings[[which(df_ratings == "Bellarmine", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "Bellarmine", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "Bellarmine", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
} else if (season == "2022") {
  #Update Sum conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "Sum") %>%
    filter(team != "St. Thomas") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  #Set St. Thomas to Sum conference rating
  df_ratings[[which(df_ratings == "St. Thomas", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "St. Thomas", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "St. Thomas", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
} else if (season == "2023") {
  #Update ASun conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "ASun") %>%
    filter(team != "Queens") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  #Set Queens to ASun conference rating
  df_ratings[[which(df_ratings == "Queens", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "Queens", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "Queens", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
  
  #Update OVC conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "OVC") %>%
    filter(team != "Southern Indiana") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  #Set Southern Indiana to OVC conference rating
  df_ratings[[which(df_ratings == "Southern Indiana", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "Southern Indiana", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "Southern Indiana", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
  
  #Update Slnd conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "Slnd") %>%
    filter(team != "Texas A&M Commerce") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  #Set Texas A&M Commerce to Slnd conference rating
  df_ratings[[which(df_ratings == "Texas A&M Commerce", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "Texas A&M Commerce", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "Texas A&M Commerce", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
  
  #Update NEC conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "NEC") %>%
    filter(team != "Stonehill") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  #Set Stonehill to NEC conference rating
  df_ratings[[which(df_ratings == "Stonehill", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "Stonehill", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "Stonehill", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
} else if (season == "2024") {
  #Update NEC conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "NEC") %>%
    filter(team != "Le Moyne") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  #Set Le Moyne to NEC conference rating
  df_ratings[[which(df_ratings == "Le Moyne", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "Le Moyne", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "Le Moyne", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
}else if (season == "2025") {
  #Update ASun conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "ASun") %>%
    filter(team != "West Georgia") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  #Set West Georgia to ASun conference rating
  df_ratings[[which(df_ratings == "West Georgia", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "West Georgia", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "West Georgia", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
  #Update NEC conference ratings
  df_conference_teams <- df_team_conf %>%
    filter(team_conf == "NEC") %>%
    filter(team != "Mercyhurst") %>%
    select(team)
  df_conference_average <- df_ratings %>%
    filter(team %in% df_conference_teams$team) %>%
    select(elo_adjusted, elo_line, elo_combined) %>%
    summarise(elo_adjusted_avg = mean(elo_adjusted),
              elo_line_avg = mean(elo_line),
              elo_combined_avg = mean(elo_combined))
  #Set West Georgia to NEC conference rating
  df_ratings[[which(df_ratings == "Mercyhurst", arr.ind=TRUE)[1], 
              "elo_adjusted"]] <- df_conference_average$elo_adjusted_avg
  df_ratings[[which(df_ratings == "Mercyhurst", arr.ind=TRUE)[1], 
              "elo_line"]] <- df_conference_average$elo_line_avg
  df_ratings[[which(df_ratings == "Mercyhurst", arr.ind=TRUE)[1], 
              "elo_combined"]] <- df_conference_average$elo_combined_avg
}
df_year_box_score <- df_year_box_score %>%
  mutate(team1_elo_adjusted_odds = 0,
         team2_elo_adjusted_odds = 0,
         elo_adjusted_line = 0,
         elo_line_line = 0,
         elo_combined_line = 0,
         result = 0,
         year = season
  )
df_ratings <- year_elo_ratings(df_year_box_score, df_ratings, 
                               home_court, k, l, m)
year1 <- strtoi(season) - 1

#Set end of year ratings to year
df_ratings <- df_ratings %>%
  mutate(!!paste0("elo_adjusted_", season) := elo_adjusted,
         !!paste0("elo_line_", season) := elo_line,
         !!paste0("elo_combined_", season) := elo_combined
  )
write_csv(df_ratings,
          paste("mbb_elo_torvik_ratings_",season, ".csv", sep = ""))

#Regress to baseline for next year.
#Regression calculation = 50% end of year, 30% n - 1, and 20% mean (1500)
df_ratings["elo_adjusted"] <- 
  ((.5 * df_ratings[[paste0("elo_adjusted_", season)]]) + 
     (.2 * 1500) +
     (.3 * df_ratings[[paste0("elo_adjusted_", year1)]]))
df_ratings["elo_line"] <- 
  ((.5 * df_ratings[[paste0("elo_line_", season)]]) + 
     (.2 * 1500) +
     (.3 * df_ratings[[paste0("elo_line_", year1)]]))
df_ratings["elo_combined"] <- 
  ((.5 * df_ratings[[paste0("elo_combined_", season)]]) + 
     (.2 * 1500) +
     (.3 * df_ratings[[paste0("elo_combined_", year1)]]))
df_ratings <- df_ratings %>%
  mutate(
    w = 0,
    l = 0,
    conf_w = 0,
    conf_l = 0,
    gp = 0
  )
#write_csv(df_ratings,
#          "C:/Users/ckoho/Documents/Inputs/NCAA/mbb_elo_torvik.csv")
}
