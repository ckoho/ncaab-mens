#elo_calculation.R
#This computes the ELO ranking for MBB using torvik data.

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
#j <- 5
#df <- df_year_box_score
#df1 <- df_ratings
year_elo_ratings <- function(df, df1, home_court, k, l){
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
      #1 means team2 (home) win, 0 means team1 (road) win
      #>.5 is team2 (home) is favorite, <.5 is team1 (road) favorite
      df[[j, "result"]] <- 1
      
      #See if they cover the line.
      if(df[[j, "team1_pts"]] - df[[j, "team2_pts"]] < df[[j, "line"]]){
        adjust <- round((1 + l) * log(df[[j, "team2_pts"]] - df[[j, "team1_pts"]] +
                                        1) * k * (1 - team2_win_per), 0)
      } else {
        adjust <- round((1 - l) * log(df[[j, "team2_pts"]] - df[[j, "team1_pts"]] +
                                        1) * k * (1 - team2_win_per), 0)
      }
      
      df1[[which(df1 == team1, arr.ind=TRUE)[1], "elo"]] <- 
        df1[[which(df1 == team1, arr.ind=TRUE)[1], "elo"]] - adjust
      df1[[which(df1 == team2, arr.ind=TRUE)[1], "elo"]] <- adjust +
        df1[[which(df1 == team2, arr.ind=TRUE)[1], "elo"]]
    } else {
      #See if they cover the line.
      if(df[[j, "team1_pts"]] - df[[j, "team2_pts"]] > df[[j, "line"]]){
        adjust <- round((1 + l) * log(df[[j, "team1_pts"]] - df[[j, "team2_pts"]] +
                                        1) * k * (team2_win_per), 0)
      } else {
        adjust <- round((1 - l) * log(df[[j, "team1_pts"]] - df[[j, "team2_pts"]] +
                                        1) * k * (team2_win_per), 0)
      }
      
      #adjust <- round(log(df[[j, "team1_pts"]] - df[[j, "team2_pts"]] +
      #                      1) * k * (team2_win_per), 0)
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
  
  #write_csv(df, paste0("results_eoy_", l, "_", season, "_mbb_box_score.csv"))
  write_csv(df, paste0("results_eoy_torvik_", season, "_mbb_box_score.csv"))
  return(df1)
}


#DEBUG
k <- 27
#l_loop <- c(.01, .05, .1, .15, .2, .25)
#l_loop <- c(.3, .4, .5, .75)
l <- .25
#season <- 2024

df_ratings <- vroom(
  "C:/Users/ckoho/Documents/Inputs/NCAA/mbb_elo_torvik_default.csv", 
  altrep = FALSE)
#season <- 2023
#df_ratings <- vroom(
#  "C:/Users/ckoho/Documents/Inputs/NCAA/Torvik/mbb_elo_torvik_2023.csv", 
#  altrep = FALSE)

for (season in 2008:2024){
  #Need to check 2022 home court advantage.
  df_ratings[, 9:69]  <- NA
  if (season == "2021" | season == "2022" | season == "2023" | season == "2024"
      ){
    home_court <- 49
  }else{
    home_court <- 70
  }
  if (season == "2009"){
    #Set Bryant to ind conference rating
    df_ratings[46,2] <- 1286
    #Set Houston Christain to ind conference rating
    df_ratings[29,2] <- 1286
    #Set SIU to ind conference rating
    df_ratings[23,2] <- 1286
    #Set IPFW to Summit conference rating
    df_ratings[14,2] <- 1459
  }else if (season == "2010"){
    #Set North Dakota to GWC conference rating
    df_ratings[26,2] <- 1209
    #Set South Dakota to GWC conference rating
    df_ratings[25,2] <- 1209
    #Set Seattle to ind conference rating
    df_ratings[24,2] <- 1256
  }else if (season == "2012"){
    #Set Nebraska Omaha to ind conference rating
    df_ratings[20,2] <- 1239
  }else if (season == "2013"){
    #Set Northern Kentucky to ASun conference rating
    df_ratings[19,2] <- 1449
  }else if (season == "2014"){
    #Set Abilene Christian to Slnd conference rating
    df_ratings[15,2] <- 1359
    #Set Grand Canyon to WAC conference rating
    df_ratings[18,2] <- 1493
    #Set Incarnate Word to Slnd conference rating
    df_ratings[13,2] <- 1359
    #Set UMass Lowell to AE conference rating
    df_ratings[17,2] <- 1392
  }else if (season == "2017"){
    #Set Fort Wayne to IPFW rating.
    #TODO Need to clean up how this is handled later.
    df_ratings[11,2] <- df_ratings[14,2]
  }else if (season == "2019"){
    #Set Cal Baptist to Sum conference rating
    df_ratings[9,2] <- 1437
    #Set North Alabama to ASun conference rating
    df_ratings[8,2] <- 1330
  }else if (season == "2020"){
    #Set Merrimack to NEC conference rating
    df_ratings[6,2] <- 1318
  }else if (season == "2021"){
    #Set Tarleton St to WAC conference rating
    df_ratings[3,2] <- 1398
    #Set UC San Diego to BW conference rating
    df_ratings[2,2] <- 1385
    #Set Utah Tech to WAC conference rating
    df_ratings[4,2] <- 1398
    #Set Bellarmine to ASun conference rating
    df_ratings[5,2] <- 1364
  }else if (season == "2022"){
    #Set St Thomas to Sum conference rating
    df_ratings[1,2] <- 1389
  }else if (season == "2023"){
    #Set Queens to ASun conference rating
    df_ratings[365,2] <- 1393
    #Set Southern Indiana to OVC conference rating
    df_ratings[366,2] <- 1369
    #Set Stonehill to NEC conference rating
    df_ratings[367,2] <- 1334
    #Set Texas A&M Commerce to OVC conference rating
    df_ratings[368,2] <- 1254
  }else if (season == "2024"){
    #Set Le Moyne to NEC conference rating
    df_ratings[369,2] <- 1279
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
                                 home_court, k, l)
  elo_year <- paste( "elo", season, sep = "_")
  year1 <- strtoi(season) - 1
  elo_year1 <- paste("elo", year1, sep = "_")
  
  #Set end of year ratings to year
  df_ratings <- df_ratings %>%
    mutate( !!elo_year := elo)
  write_csv(df_ratings, paste("mbb_elo_torvik_", season, ".csv", sep = ""))
  
  
  
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
  write_csv(df_ratings, "C:/Users/ckoho/Documents/Inputs/NCAA/mbb_elo_torvik.csv")
}
