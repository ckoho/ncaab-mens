#elo_calculation_adjust.R
#This is elo calculation factoring in a line.
#Work in progress.
library(tidyverse)
library(readr)
library(vroom)
library(fs)


#j <- 1
#df <- df_year_box_score
#df1 <- df_ratings
year_elo_ratings <- function(df, df1, home_court, k){
  for (j in 1:nrow(df)) {
    team1 = df[[j, "team1"]]
    team1_rating = df1[[which(df1 == team1, arr.ind=TRUE)[1], "aelo"]]
    team2 = df[[j, "team2"]]
    team2_rating = df1[[which(df1 == team2, arr.ind=TRUE)[1], "aelo"]]
    #Calculating home court advantage into win percentage. Rest of calculation 
    #is the same.
    if (df[[j, "loc"]] == "A") {
      team2_win_per = 1 / (1 + 10 ^ ((team1_rating - 
                                        (team2_rating + home_court))/400))
      df[[j, "line"]] <- (team1_rating - (team2_rating + home_court)) * .0815
      
    } else if (df[[j, "loc"]] == "N") {
      team2_win_per = 1 / (1 + 10 ^ ((team1_rating - (team2_rating))/400))
      df[[j, "line"]] <- (team1_rating - (team2_rating)) * .0815
      
    } else if (df[[j, "loc"]] == "H") {
      team2_win_per = 1 / (1 + 10 ^ (((team1_rating + home_court) - 
                                        (team2_rating))/400))
      df[[j, "line"]] <- ((team1_rating + home_court) - 
                            (team2_rating)) * .0815
      
    }
    
    df[[j, "team1_rating"]] <- team1_rating
    df[[j, "team2_rating"]] <- team2_rating
    df[[j, "team1_odds"]] <- 1 - team2_win_per
    df[[j, "team2_odds"]] <- team2_win_per
    
    #Compare result to line (expected result).
    if (df[[j, "line"]] > (df[[j, "team1_pts"]] - df[[j, "team2_pts"]])) {
      #Win per should always be 50/50 for this method.
      delta <- df[[j, "line"]] - (df[[j, "team1_pts"]] - df[[j, "team2_pts"]])
      adjust <- round(log(delta + 1) * k * (.5), 0)
      #adjust <- round(k * (.5), 0)
      df1[[which(df1 == team1, arr.ind=TRUE)[1], "aelo"]] <- 
        df1[[which(df1 == team1, arr.ind=TRUE)[1], "aelo"]] - adjust
      df1[[which(df1 == team2, arr.ind=TRUE)[1], "aelo"]] <- adjust +
        df1[[which(df1 == team2, arr.ind=TRUE)[1], "aelo"]]
    } else {
      #Win per should always be 50/50 for this method.
      delta <- abs(df[[j, "line"]] - (df[[j, "team1_pts"]] - 
                                        df[[j, "team2_pts"]]))
      
      adjust <- round(log(delta + 1) * k * (.5), 0)
      #adjust <- round(k * (.5), 0)
      df1[[which(df1 == team1, arr.ind=TRUE)[1], "aelo"]] <- adjust +
        df1[[which(df1 == team1, arr.ind=TRUE)[1], "aelo"]]
      df1[[which(df1 == team2, arr.ind=TRUE)[1], "aelo"]] <- 
        df1[[which(df1 == team2, arr.ind=TRUE)[1], "aelo"]] - adjust
      
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
      df1[[which(df1 == team2, arr.ind=TRUE)[1], "aelo"]]
    df1[[which(df1 == team1, arr.ind=TRUE)[1], 
         paste0("game", df1[[which(df1 == team1, arr.ind=TRUE)[1], "gp"]])]] <-
      df1[[which(df1 == team1, arr.ind=TRUE)[1], "aelo"]]
    
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

#k_loop <- c(34)
k <- 27
season <- 2008
df_ratings <- vroom("mbb_elo_default.csv", altrep = FALSE)
#df1 <- vroom("mbb_elo_default.csv", altrep = FALSE)
#for (season in 2008:2022){
for (season in 2008:2021) {
  #Need to check 2022 home court advantage.
  if (season == "2021") {
    home_court <- 49
  } else{
    home_court <- 70
  }
  
  df_year_box_score <-
    vroom(
      paste0(
        "C:/Users/ckoho/Documents/Inputs/NCAA/torvik_box_score_",
        season,
        ".csv"
      )
    )
  #df_year_box_score <- vroom(paste0( "torvik_box_score_", season, ".csv"))
  df_year_box_score$team1_odds <- 0
  df_year_box_score$team2_odds <- 0
  df_year_box_score$result <- 0
  df_year_box_score$line <- 0
  df_year_box_score$adjust <- 0
  df_year_box_score$team1_rating <- 0
  df_year_box_score$team2_rating <- 0
  df_year_box_score$year <- season
  
  #df <- df_year_box_score
  
  df_ratings <- year_elo_ratings(df_year_box_score, df_ratings,
                                 home_court, k)
  elo_year <- paste("aelo", season, sep = "_")
  year1 <- strtoi(season) - 1
  elo_year1 <- paste("aelo", year1, sep = "_")
  
  #Set end of year ratings to year
  df_ratings <- df_ratings %>%
    mutate(!!elo_year := aelo)
  write_csv(df_ratings, paste("mbb_elo_", season, ".csv", sep = ""))
  #write_csv(df_ratings, paste("mbb_elo_", k, "_",
  #                            season, ".csv", sep = ""))
  
  #Regress to baseline for next year.
  #Regression calculation = 50% end of year, 3%0 n -1, and 20% mean (1500)
  df_ratings["aelo"] <-
    ((.5 * df_ratings[elo_year]) + (.2 * 1500) +
       (.3 * df_ratings[elo_year1]))
  df_ratings <- df_ratings %>%
    mutate(
      w = 0,
      l = 0,
      confw = 0,
      confl = 0,
      gp = 0
    )
  
  write_csv(df_ratings, "mbb_elo.csv")
}
