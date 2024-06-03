#line_to_ranking.R
#This uses the betting lines to create a power ranking. 
#This is a top down approach.
 
#library(sportsdataverse)
#library(gamezoneR)
library(tidyverse)
#library(jsonlite)
#library(cli)
#library(withr)
library(readr)
library(vroom)
library(fs)

#Pull in the lines.
df_lines <-  vroom(
  "C:/Users/ckoho/Documents/Inputs/NCAA/line_pr_debug_tmp.csv", 
  altrep = FALSE)
df_teams <-  vroom(
  "C:/Users/ckoho/Documents/Inputs/NCAA/teams_pr_debug_tmp.csv", 
  altrep = FALSE)
#NFL Files
df_lines <-  vroom(
#  "C:/Users/ckoho/Documents/Inputs/NCAA/line_nfl_2.csv", 
#  "C:/Users/ckoho/Documents/Inputs/NCAA/line_nfl_3.csv", 
  "C:/Users/ckoho/Documents/Inputs/NCAA/line_nfl_4.csv", 
  altrep = FALSE)
df_teams <-  vroom(
  "C:/Users/ckoho/Documents/Inputs/NCAA/teams_nfl.csv", 
  altrep = FALSE)

#home court advantage NFL
#https://sportsbook.draftkings.com/nfl-odds-week-3
hca <- 1.5
#hca <- 2.5

j <- 1

for (j in 1:100) {
  for (i in 1:nrow(df_lines)) {
    team1 = df_lines[[i, "team1"]]
    team1_rating = df_teams[[which(df_teams == team1, arr.ind = TRUE)[1], "Rating"]]
    team2 = df_lines[[i, "team2"]]
    team2_rating = df_teams[[which(df_teams == team2, arr.ind = TRUE)[1], "Rating"]]
    spread <- df_lines[[i, "Spread"]]
    line_delta <- team2_rating + spread - team1_rating + hca
    df_lines[[i, "team_delta"]] <- team2_rating - team1_rating
    df_lines[[i, "adjusted_spread"]] <- spread + hca
    df_lines[[i,"line_delta"]] <- line_delta
    df_lines[[i,"team1_rating"]] <- team1_rating
    df_lines[[i,"team2_rating"]] <- team2_rating
    if (line_delta > .2) {
      adjust <- line_delta / 2
      df_teams[[which(df_teams == team1, arr.ind = TRUE)[1], "Rating"]] <-
        df_teams[[which(df_teams == team1, arr.ind = TRUE)[1], "Rating"]] + adjust
      df_teams[[which(df_teams == team2, arr.ind = TRUE)[1], "Rating"]] <-
        df_teams[[which(df_teams == team2, arr.ind = TRUE)[1], "Rating"]] - adjust
    } else if (line_delta < -.2) {
      adjust <- line_delta / 2
      df_teams[[which(df_teams == team1, arr.ind = TRUE)[1], "Rating"]] <-
        df_teams[[which(df_teams == team1, arr.ind = TRUE)[1], "Rating"]] + adjust
      df_teams[[which(df_teams == team2, arr.ind = TRUE)[1], "Rating"]] <-
        df_teams[[which(df_teams == team2, arr.ind = TRUE)[1], "Rating"]] - adjust
    } else{
      adjust <- 0
    }
    df_lines[[i,"adjust"]] <- adjust
    df_lines[[i,"abs_adjust"]] <- abs(adjust)
    
    df_lines[[i,"team1_rating_n"]] <- 
      df_teams[[which(df_teams == team1, arr.ind = TRUE)[1], "Rating"]]
    df_lines[[i,"team2_rating_n"]] <- 
      df_teams[[which(df_teams == team2, arr.ind = TRUE)[1], "Rating"]]
    
  }
  
  #normalizing_factor <- mean(df_teams$Rating)
  #df_teams <- df_teams %>%
  #  mutate(Rating = Rating - normalizing_factor)
  df_teams$Rating
  write_csv(df_lines, paste0(
            "C:/Users/ckoho/Documents/Inputs/NCAA/", j, "_nfl_ratings_debug.csv"
            ))
  
  write_csv(
    df_teams,
    paste0(
      "C:/Users/ckoho/Documents/Inputs/NCAA/NFL_lines/",
      j,
#      "_nfl_week2_ratings.csv"
#      "_nfl_week3_ratings.csv"
      "_nfl_week4_ratings.csv"
    )
  )
}

j <- 4
df_compare <- NULL
for (j in 2:4){
  df_games_to_predict <- vroom(
    "C:/Users/ckoho/Documents/Inputs/NCAA/line_nfl_5.csv", 
    altrep = FALSE)
  df_teams <- vroom(
    paste0(
      "C:/Users/ckoho/Documents/Inputs/NCAA/NFL_lines/100_nfl_week", 
      j, "_ratings.csv"), 
    altrep = FALSE)
  df_games_to_predict$prediction <- 0
  for (i in 1:nrow(df_games_to_predict)){
    team1 = df_games_to_predict[[i, "team1"]]
    team1_rating = df_teams[[which(df_teams == team1, 
                                   arr.ind = TRUE)[1], "Rating"]]
    team2 = df_games_to_predict[[i, "team2"]]
    team2_rating = df_teams[[which(df_teams == team2, 
                                   arr.ind = TRUE)[1], "Rating"]]
    pred_spread <- team1_rating - hca - team2_rating
    df_games_to_predict[[i, "prediction"]] <- pred_spread
    if (team1 == "NYJ"){
      team1
      team1_rating
      team2
      team2_rating
      pred_spread
    }
    
  }
  df_games_to_predict <- df_games_to_predict %>%
    mutate(weeks = j,
           delta = Spread - prediction,
           abs_delta = abs(Spread - prediction))
  write_csv(df_games_to_predict, paste0(
            "C:/Users/ckoho/Documents/Inputs/NCAA/NFL_lines/week", 
            j, "_prediction.csv"))
  df_compare <- df_compare %>%
    bind_rows(df_games_to_predict)
  
}




df_compare %>%
  group_by(weeks) %>%
  summarize(error = mean(abs_delta),
            max = max(abs_delta))


for (j in 1:100){
  df_games_to_predict <- vroom(
    "C:/Users/ckoho/Documents/Inputs/NCAA/line_nfl_5.csv", 
    altrep = FALSE)
  df_teams <- vroom(paste0(
    "C:/Users/ckoho/Documents/Inputs/NCAA/", j , "_nfl_week2_ratings.csv"), 
  altrep = FALSE)
df_games_to_predict$prediction <- 0
for (i in 1:nrow(df_games_to_predict)){
  team1 = df_games_to_predict[[i, "team1"]]
  team1_rating = df_teams[[which(df_teams == team1, 
                                 arr.ind = TRUE)[1], "Rating"]]
  team2 = df_games_to_predict[[i, "team2"]]
  team2_rating = df_teams[[which(df_teams == team2, 
                                 arr.ind = TRUE)[1], "Rating"]]
  pred_spread <- team1_rating - hca - team2_rating
  df_games_to_predict[[i, "prediction"]] <- pred_spread
  
}
df_games_to_predict <- df_games_to_predict %>%
  mutate(error = abs(Spread - prediction))
write_csv(df_games_to_predict, paste0(
          "C:/Users/ckoho/Documents/Inputs/NCAA/NFL_lines/", 
          j , 
          "_nfl_predictions_2weeks.csv"))

}

df_compare <- NULL
for (i in 1:100){
  df_read <-  vroom(paste0(
    "C:/Users/ckoho/Documents/Inputs/NCAA/", i, "_nfl_predictions_2weeks.csv"), 
    altrep = FALSE) %>%
    #select(prediction, error) %>%
    mutate(week = i)
  df_compare <- df_compare %>%
    bind_rows(df_read)

}
df_summary <- df_compare %>%
  group_by(week) %>%
  summarize(mean = mean(error),
            sd = sd(error),
            max = max(error))


#Week 5 prediction
df_games_to_predict <- vroom(
  "C:/Users/ckoho/Documents/Inputs/NCAA/line_nfl_5.csv", 
  altrep = FALSE)
df_teams <- vroom(
  "C:/Users/ckoho/Documents/Inputs/NCAA/20_nfl_week4_ratings.csv", 
  altrep = FALSE)

df_games_to_predict$prediction <- 0
for (i in 1:nrow(df_games_to_predict)){
  team1 = df_games_to_predict[[i, "team1"]]
  team1_rating = df_teams[[which(df_teams == team1, 
                                            arr.ind = TRUE)[1], "Rating"]]
  team2 = df_games_to_predict[[i, "team2"]]
  team2_rating = df_teams[[which(df_teams == team2, 
                                            arr.ind = TRUE)[1], "Rating"]]
  pred_spread <- team1_rating - hca - team2_rating
  df_games_to_predict[[i, "prediction"]] <- pred_spread
  
}
df_games_to_predict <- df_games_to_predict %>%
  mutate(error = abs(Spread - prediction))
write_csv(df_games_to_predict, 
          "C:/Users/ckoho/Documents/Inputs/NCAA/nfl_predictions_4weeks.csv")




df_read <-  vroom(
  "C:/Users/ckoho/Documents/Inputs/NCAA/nfl_predictions_4weeks.csv", 
  altrep = FALSE) %>%
  #select(prediction, error) %>%
  mutate(week = 4)
df_compare <- NULL
df_compare <- df_compare %>%
  bind_rows(df_read)

df_read <-  vroom(
  "C:/Users/ckoho/Documents/Inputs/NCAA/nfl_predictions_3weeks.csv", 
  altrep = FALSE) %>%
  #select(prediction, error) %>%
  mutate(week = 3)
df_compare <- df_compare %>%
  bind_rows(df_read)

df_read <-  vroom(
  "C:/Users/ckoho/Documents/Inputs/NCAA/nfl_predictions_2weeks.csv", 
  altrep = FALSE) %>%
  #select(prediction, error) %>%
  mutate(week = 2)
df_compare <- df_compare %>%
  bind_rows(df_read)

df_compare %>%
  group_by(week) %>%
  summarize(mean = mean(error),
           sd = sd(error),
           max = max(error))
