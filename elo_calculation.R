#elo_calculation.R
#This file handles the importing and cleaning of data.

#library(sportsdataverse)
#library(gamezoneR)
library(tidyverse)
#library(jsonlite)
#library(cli)
#library(withr)
library(readr)
library(vroom)
library(fs)
#Want to use k=27

#j <- 1
#df <- df_year_box_score
#df1 <- df_ratings
year_elo_ratings <- function(df, df1, home_court, k){
  for (j in 1:nrow(df)) {
    #Get each team name and rating .
    team1 = df[[j, "team1"]]
    team1_rating = df1[[which(df1 == team1, arr.ind=TRUE)[1], "elo"]]
    team2 = df[[j, "team2"]]
    team2_rating = df1[[which(df1 == team2, arr.ind=TRUE)[1], "elo"]]
    #Calculating home court advantage into win percentage. Rest of calculation 
    #is the same. Line calculation also needs to factor in home court. Taking 
    #rating delta time .0815 to get line.
    if (df[[j, "loc"]] == "A") {
      team2_win_per = 1 / (1 + 10 ^ ((team1_rating - 
                                        (team2_rating + home_court))/400))
      df[[j, "line"]] <- (team1_rating - (team2_rating + home_court)) * .0815
      
    } else if (df[[j, "loc"]] == "N") {
      team2_win_per = 1 / (1 + 10 ^ ((team1_rating - (team2_rating))/400))
      df[[j, "line"]] <- (team1_rating - (team2_rating)) * .0815
    } else if (df[[j, "loc"]] == "H") {
      team2_win_per = 1 / (1 + 10 ^ (((team1_rating + home_court)- 
                                        (team2_rating))/400))
      df[[j, "line"]] <- ((team1_rating + home_court) - 
                            (team2_rating)) * .0815
      
    }
    #Put pre rating and odds into box score data frame.
    df[[j, "team1_rating"]] <- team1_rating
    df[[j, "team2_rating"]] <- team2_rating
    df[[j, "team1_odds"]] <- 1 - team2_win_per
    df[[j, "team2_odds"]] <- team2_win_per

    #Update elo rating based on game result.
    if (df[[j, "win"]] == team2) {
      df[[j, "result"]] <- 1
      #1 means team2 (home) win, 0 means team1 (road) win
      #>.5 is team2 (home) is favorite, <.5 is team1 (road) favorite
      adjust <- round(log(df[[j, "team2_pts"]] - df[[j, "team1_pts"]] +
                            1) * k * (1 - team2_win_per), 0)
      df1[[which(df1 == team1, arr.ind=TRUE)[1], "elo"]] <- 
        df1[[which(df1 == team1, arr.ind=TRUE)[1], "elo"]] - adjust
      df1[[which(df1 == team2, arr.ind=TRUE)[1], "elo"]] <- adjust +
        df1[[which(df1 == team2, arr.ind=TRUE)[1], "elo"]]
    } else {
      adjust <- round(log(df[[j, "team1_pts"]] - df[[j, "team2_pts"]] +
                            1) * k * (team2_win_per), 0)
      df1[[which(df1 == team1, arr.ind=TRUE)[1], "elo"]] <- adjust +
        df1[[which(df1 == team1, arr.ind=TRUE)[1], "elo"]]
      df1[[which(df1 == team2, arr.ind=TRUE)[1], "elo"]] <- 
        df1[[which(df1 == team2, arr.ind=TRUE)[1], "elo"]] - adjust

    }
    df[[j, "adjust"]] <- adjust

    #Adjust records (games played, W/L, conf W/L, and elo rating through gp.)
    if(df[[j, "win"]] == team2 ){
      if (df[[j, "type"]] == "conf"){
        df1[[which(df1 == team2, arr.ind=TRUE)[1], "confw"]] <- 
          df1[[which(df1 == team2, arr.ind=TRUE)[1], "confw"]] + 1
        df1[[which(df1 == team1, arr.ind=TRUE)[1], "confl"]] <- 
          df1[[which(df1 == team1, arr.ind=TRUE)[1], "confl"]] + 1
      }
      df1[[which(df1 == team2, arr.ind=TRUE)[1], "w"]] <- 
        df1[[which(df1 == team2, arr.ind=TRUE)[1], "w"]] + 1
      df1[[which(df1 == team1, arr.ind=TRUE)[1], "l"]] <- 
        df1[[which(df1 == team1, arr.ind=TRUE)[1], "l"]] + 1
    }else{
      if (df[[j, "type"]] == "conf"){
        df1[[which(df1 == team2, arr.ind=TRUE)[1], "confl"]] <- 
          df1[[which(df1 == team2, arr.ind=TRUE)[1], "confl"]] + 1
        df1[[which(df1 == team1, arr.ind=TRUE)[1], "confw"]] <- 
          df1[[which(df1 == team1, arr.ind=TRUE)[1], "confw"]] + 1
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
    #Set elo ranking for that specific game played. Used to trend over time.
    df1[[which(df1 == team2, arr.ind=TRUE)[1], 
         paste0("game", df1[[which(df1 == team2, arr.ind=TRUE)[1], "gp"]])]] <-
      df1[[which(df1 == team2, arr.ind=TRUE)[1], "elo"]]
    df1[[which(df1 == team1, arr.ind=TRUE)[1], 
         paste0("game", df1[[which(df1 == team1, arr.ind=TRUE)[1], "gp"]])]] <-
      df1[[which(df1 == team1, arr.ind=TRUE)[1], "elo"]]
  
  }
  
  #Save df and return df1
  #Select less values from df before saving.
  df <- df %>%
    select(type, loc, season, date, win, loss, team1_conf, team1, team1_pts, 
           team2_conf, team2, team2_pts, team1_odds, team2_odds, result, line,
           year, adjust, team2_rating, team1_rating)
  
  #write_csv(df, paste0("results_eoy_", k, "_", season, "_mbb_box_score.csv"))
  write_csv(df, paste0("results_eoy_", season, "_mbb_box_score.csv"))
  return(df1)
}


#DEBUG
k_loop <- c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45)
k_loop <- c(11, 12, 13, 14, 16, 17, 18, 19)
k_loop <- c(1, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 25, 30, 40)
k_loop <- c(6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 25, 30, 40)
k_loop <- c(22, 23, 24, 26, 27, 28, 29, 31, 32, 33)
#k_loop <- c(34)
k <- 27
#season <- 2008
#This k loop is to be removed once values are chosen.
for (k in k_loop){
  df_ratings <- vroom("mbb_elo_default.csv", altrep = FALSE)
  #for (season in 2008:2022){
  for (season in 2008:2021){
    #Need to check 2022 home court advantage.
    if (season == "2021" ){
      home_court <- 49
    }else{
      home_court <- 70
    }
    df_year_box_score <- vroom(paste0( "C:/Users/ckoho/Documents/Inputs/NCAA/torvik_box_score_", season, ".csv"))
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
                                   home_court, k)
    #HERE CK 6/3. Need to finish regression to mean.
    elo_year <- paste( "elo", season, sep = "_")
    year1 <- strtoi(season) - 1
    elo_year1 <- paste("elo", year1, sep = "_")
    
    #Set end of year ratings to year
    df_ratings <- df_ratings %>%
      mutate( !!elo_year := elo)
    write_csv(df_ratings, paste("mbb_elo_", season, ".csv", sep = ""))
    #write_csv(df_ratings, paste("mbb_elo_", k, "_", 
    #                            season, ".csv", sep = ""))
    
    #Regress to baseline for next year.
    #Regression calculation = 50% end of year, 3%0 n -1, and 20% mean (1500)
    df_ratings["elo"] <- ((.5 * df_ratings[elo_year]) + (.2 * 1500) + 
                            (.3 * df_ratings[elo_year1]))
    df_ratings <- df_ratings %>%
      mutate(w = 0,
             l = 0,
             confw = 0,
             confl = 0,
             gp = 0
      )
    
    write_csv(df_ratings, "mbb_elo.csv")
  }
}
