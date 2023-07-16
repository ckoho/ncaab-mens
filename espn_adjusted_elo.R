#espn_adjusted_elo.R
#Uses ESPN box scorea to calculate adjusted elo rankings. 

##library(sportsdataverse)
##library(gamezoneR)
library(tidyverse)
##library(jsonlite)
##library(cli)
##library(withr)
library(readr)
library(vroom)
library(fs)
library(magrittr)

#j <- 1
#j <- 311
#df <- df_year_box_score
#df1 <- df_ratings
year_elo_ratings <- function(df, df1, home_court, k, l){
  for (j in 1:nrow(df)) {
    #Get each team name and rating .
    print(j)
    team1 = df[[j, "team1"]]
    team1_rating = df1[[which(df1 == team1, arr.ind = TRUE)[1], "elo"]]
    team2 = df[[j, "team2"]]
    team2_rating = df1[[which(df1 == team2, arr.ind = TRUE)[1], "elo"]]
    
    #Calculating home court advantage into win percentage. Rest of calculation
    #is the same. Line calculation also needs to factor in home court. Taking
    #rating delta time .0815 to get line.
    if (df[[j, "neutralsite"]] == "FALSE") {
      team1_win_per = 1 / (1 + 10 ^ ((
        team2_rating -
          (team1_rating + home_court)
      ) / 400))
      df[[j, "line"]] <-
        (team2_rating - (team1_rating + home_court)) * .0815
      
    } else if (df[[j, "neutralsite"]] == "TRUE") {
      team1_win_per = 1 / (1 + 10 ^ ((team2_rating - (team1_rating)) / 400))
      df[[j, "line"]] <- (team2_rating - (team1_rating)) * .0815
    }
    #Put pre rating and odds into box score data frame.
    df[[j, "team1_rating"]] <- team1_rating
    df[[j, "team2_rating"]] <- team2_rating
    df[[j, "team2_odds"]] <- 1 - team1_win_per
    df[[j, "team1_odds"]] <- team1_win_per
    
    #Update elo rating based on game result.
    if (df[[j, "team1_score"]] > df[[j, "team2_score"]]) {
      #1 means team1 (home) win, 0 means team2 (road) win
      #>.5 is team1 (home) is favorite, <.5 is team2 (road) favorite
      df[[j, "result"]] <- 1
      #See if they cover the line.
      if (df[[j, "team2_score"]] - df[[j, "team1_score"]] < df[[j, "line"]]) {
        adjust <-
          round((1 + l) * log(df[[j, "team1_score"]] - df[[j, "team2_score"]] +
                                1) * k * (1 - team1_win_per), 0)
      } else {
        adjust <-
          round((1 - l) * log(df[[j, "team1_score"]] - df[[j, "team2_score"]] +
                                1) * k * (1 - team1_win_per), 0)
      }
      
      df1[[which(df1 == team1, arr.ind = TRUE)[1], "elo"]] <-
        df1[[which(df1 == team1, arr.ind = TRUE)[1], "elo"]] + adjust
      df1[[which(df1 == team2, arr.ind = TRUE)[1], "elo"]] <-
        df1[[which(df1 == team2, arr.ind = TRUE)[1], "elo"]] - 
        adjust
    } else {
      #See if they cover the line.
      #CK HERE NEED TO CHECK THIS ONE.
      if (df[[j, "team1_score"]] - df[[j, "team2_score"]] > df[[j, "line"]]) {
        adjust <-
          round((1 + l) * log(df[[j, "team2_score"]] - df[[j, "team1_score"]] +
                                1) * k * (team1_win_per), 0)
      } else {
        adjust <-
          round((1 - l) * log(df[[j, "team2_score"]] - df[[j, "team1_score"]] +
                                1) * k * (team1_win_per), 0)
      }
      #adjust <-
      #  round(log(df[[j, "team2_score"]] - df[[j, "team1_score"]] +
      #              1) * k * (team1_win_per), 0)
      df1[[which(df1 == team1, arr.ind = TRUE)[1], "elo"]] <-
        df1[[which(df1 == team1, arr.ind = TRUE)[1], "elo"]] - 
        adjust
      df1[[which(df1 == team2, arr.ind = TRUE)[1], "elo"]] <-
        df1[[which(df1 == team2, arr.ind = TRUE)[1], "elo"]] + adjust
      
    }
    df[[j, "adjust"]] <- adjust
    
    #Adjust records (games played, W/L, conf W/L, and elo rating through gp.)
    if (df[[j, "team2_score"]] > df[[j, "team1_score"]]) {
      if (df[[j, "conferencecompetition"]] == "TRUE") {
        df1[[which(df1 == team2, arr.ind = TRUE)[1], "confw"]] <-
          df1[[which(df1 == team2, arr.ind = TRUE)[1], "confw"]] + 1
        df1[[which(df1 == team1, arr.ind = TRUE)[1], "confl"]] <-
          df1[[which(df1 == team1, arr.ind = TRUE)[1], "confl"]] + 1
      }
      df1[[which(df1 == team2, arr.ind = TRUE)[1], "w"]] <-
        df1[[which(df1 == team2, arr.ind = TRUE)[1], "w"]] + 1
      df1[[which(df1 == team1, arr.ind = TRUE)[1], "l"]] <-
        df1[[which(df1 == team1, arr.ind = TRUE)[1], "l"]] + 1
    } else{
      if (df[[j, "conferencecompetition"]] == "TRUE") {
        df1[[which(df1 == team2, arr.ind = TRUE)[1], "confl"]] <-
          df1[[which(df1 == team2, arr.ind = TRUE)[1], "confl"]] + 1
        df1[[which(df1 == team1, arr.ind = TRUE)[1], "confw"]] <-
          df1[[which(df1 == team1, arr.ind = TRUE)[1], "confw"]] + 1
      }
      df1[[which(df1 == team2, arr.ind = TRUE)[1], "l"]] <-
        df1[[which(df1 == team2, arr.ind = TRUE)[1], "l"]] + 1
      df1[[which(df1 == team1, arr.ind = TRUE)[1], "w"]] <-
        df1[[which(df1 == team1, arr.ind = TRUE)[1], "w"]] + 1
    }
    
    df1[[which(df1 == team1, arr.ind = TRUE)[1], "gp"]] <-
      df1[[which(df1 == team1, arr.ind = TRUE)[1], "gp"]] + 1
    df1[[which(df1 == team2, arr.ind = TRUE)[1], "gp"]] <-
      df1[[which(df1 == team2, arr.ind = TRUE)[1], "gp"]] + 1
    #Set elo ranking for that specific game played. Used to trend over time.
    df1[[which(df1 == team2, arr.ind = TRUE)[1],
         paste0("game", df1[[which(df1 == team2, arr.ind = TRUE)[1], "gp"]])]] <-
      df1[[which(df1 == team2, arr.ind = TRUE)[1], "elo"]]
    df1[[which(df1 == team1, arr.ind = TRUE)[1],
         paste0("game", df1[[which(df1 == team1, arr.ind = TRUE)[1], "gp"]])]] <-
      df1[[which(df1 == team1, arr.ind = TRUE)[1], "elo"]]
    
    
  }
  
  #Save df and return df1
  #Select less values from df before saving.
  df <- df %>%
    select(
      game_id,
      season,
      season_type,
      game_date,
      neutralsite,
      team1_home_away,
      team1_id,
      team1_mascot,
      team1_abbrev,
      team1_score,
      team2_home_away_home_away_team2,
      team2_id,
      team2_mascot,
      team2_abbrev,
      team2_score,
      team1,
      team2,
      detail,
      venue,
      venueid,
      city,
      state,
      conferencecompetition,
      team1_odds,
      team2_odds,
      result,
      line,
      adjust,
      team1_rating,
      team2_rating,
      year
    )
  
  #write_csv(df, paste0("results_eoy_", l, "_", season, "_mbb_box_score.csv"))
  write_csv(df, paste0("results_espn_eoy_", season, "_mbb_box_score.csv"))
  return(df1)
}


#Debug
team1 = df_year_box_score[[j, "team1"]]
team2 = df_year_box_score[[j, "team2"]]

season <- 2006
k <- 27
l <- .25

df_ratings <- vroom("C:/Users/ckoho/Documents/Inputs/NCAA/mbb_elo_espn_default.csv", 
                    altrep = FALSE)
for (season in 2006:2023){
  #Need to check 2022 h\\ome court advantage.
  df_ratings[, 9:69]  <- NA
  if (season == "2021"  | season == "2022" | season == "2023"){
    home_court <- 49
  }else{
    home_court <- 70
  }
  df_year_box_score <- vroom(paste0( "C:/Users/ckoho/Documents/Inputs/NCAA/espn_mbb_d1_box_score_", season, ".csv"))
  #df_year_box_score <- vroom(paste0( "torvik_box_score_", season, ".csv"))
  df_year_box_score$team1_odds <- 0
  df_year_box_score$team2_odds <- 0
  df_year_box_score$result <- 0
  df_year_box_score$line <- 0
  df_year_box_score$adjust <- 0
  df_year_box_score$team1_rating <- 0
  df_year_box_score$team2_rating <- 0
  df_year_box_score$year <- season
  df_ratings <- year_elo_ratings(df_year_box_score, df_ratings, 
                                 home_court, k, l)
  elo_year <- paste( "elo", season, sep = "_")
  year1 <- strtoi(season) - 1
  elo_year1 <- paste("elo", year1, sep = "_")
  
  #Set end of year ratings to year
  df_ratings <- df_ratings %>%
    mutate( !!elo_year := elo)
  write_csv(df_ratings, paste("mbb_elo_espn_", season, ".csv", sep = ""))
  #write_csv(df_ratings, paste("mbb_elo_", l, "_", 
  #                            season, ".csv", sep = ""))
  
  #Regress to baseline for next year.
  #Regression calculation = 50% end of year, 30% n -1, and 20% mean (1500)
  df_ratings["elo"] <- ((.5 * df_ratings[elo_year]) + (.2 * 1500) + 
                          (.3 * df_ratings[elo_year1]))
  df_ratings <- df_ratings %>%
    mutate(w = 0,
           l = 0,
           confw = 0,
           confl = 0,
           gp = 0
    )
  write_csv(df_ratings, "mbb_espn_elo.csv")
}
