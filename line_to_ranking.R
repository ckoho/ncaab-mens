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

################################################################################
### Below here is all NFL testing data to debug the code. Will put the NCAA/ ###
### NBA/WNBA data in different file with cleaner functions.                  ###
### TO DO: Create basketball specific one.                                   ###
################################################################################

#############################
###NFL Files  HCA = 1.5   ###
#############################
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




df_weekly_summary <- df_compare %>%
  group_by(weeks) %>%
  summarize(error = mean(abs_delta),
            max = max(abs_delta))

write_csv(df_weekly_summary, 
          "C:/Users/ckoho/Documents/Inputs/NCAA/NFL_lines/num_weeks_comparison_summary.csv")

#Get data on how many iterations are needed to get accurate predictions.
df_compare <- NULL
for (j in 1:100){
  df_games_to_predict <- vroom(
    "C:/Users/ckoho/Documents/Inputs/NCAA/line_nfl_5.csv", 
    altrep = FALSE)
  df_teams <- vroom(paste0(
    "C:/Users/ckoho/Documents/Inputs/NCAA/NFL_lines/", j , "_nfl_week4_ratings.csv"), 
  altrep = FALSE)
  df_games_to_predict$prediction <- 0
  for (i in 1:nrow(df_games_to_predict)) {
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
    mutate(error = abs(Spread - prediction),
           iteration = j)
  df_compare <- df_compare %>%
    bind_rows(df_games_to_predict)
  
}
df_summary <- df_compare %>%
  group_by(iteration) %>%
  summarize(mean = mean(error),
            max = max(error)) %>%
  filter(iteration < 26)


ggplot(df_summary, aes(iteration, mean)) + geom_point()
write_csv(df_summary, 
          "C:/Users/ckoho/Documents/Inputs/NCAA/NFL_lines/4weeks_iteration_summary.csv")


# Look at 2 weeks with preset (bayes) values.
# Not a whole lot better.
df_lines <-  vroom("C:/Users/ckoho/Documents/Inputs/NCAA/line_nfl_2.csv", 
  altrep = FALSE)
df_teams <-  vroom(
  "C:/Users/ckoho/Documents/Inputs/NCAA/teams_nfl_bayes.csv", 
  altrep = FALSE)

hca <- 1.5

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
  write_csv(
    df_teams,
    paste0(
      "C:/Users/ckoho/Documents/Inputs/NCAA/NFL_lines/bayes_",
      j, "_nfl_week2_ratings.csv"
    )
  )
}

df_compare <- NULL
for (j in 1:100){
  df_games_to_predict <- vroom(
    "C:/Users/ckoho/Documents/Inputs/NCAA/line_nfl_5.csv", 
    altrep = FALSE)
  df_teams <- vroom(paste0(
    "C:/Users/ckoho/Documents/Inputs/NCAA/NFL_lines/bayes_", j , "_nfl_week2_ratings.csv"), 
    altrep = FALSE)
  df_games_to_predict$prediction <- 0
  for (i in 1:nrow(df_games_to_predict)) {
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
    mutate(error = abs(Spread - prediction),
           iteration = j)
  df_compare <- df_compare %>%
    bind_rows(df_games_to_predict)
  
}
df_summary <- df_compare %>%
  group_by(iteration) %>%
  summarize(mean = mean(error),
            max = max(error))


ggplot(df_summary, aes(iteration, mean)) + geom_point()
write_csv(df_summary, 
          "C:/Users/ckoho/Documents/Inputs/NCAA/NFL_lines/2weeks_bayes_iteration_summary.csv")

j <- 2
df_compare <- NULL
for (j in 2){
  df_games_to_predict <- vroom(
    "C:/Users/ckoho/Documents/Inputs/NCAA/line_nfl_5.csv", 
    altrep = FALSE)
  df_teams <- vroom(
    paste0(
      "C:/Users/ckoho/Documents/Inputs/NCAA/NFL_lines/bayes_100_nfl_week", 
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
  }
  df_games_to_predict <- df_games_to_predict %>%
    mutate(weeks = j,
           delta = Spread - prediction,
           abs_delta = abs(Spread - prediction))
  write_csv(df_games_to_predict, paste0(
    "C:/Users/ckoho/Documents/Inputs/NCAA/NFL_lines/bayes_week", 
    j, "_prediction.csv"))
  df_compare <- df_compare %>%
    bind_rows(df_games_to_predict)
  
}




df_compare %>%
  group_by(weeks) %>%
  summarize(error = mean(abs_delta),
            max = max(abs_delta))



#############################
### NFL Files  HCA =2    ###
#############################
df_lines <-  vroom(
  #  "C:/Users/ckoho/Documents/Inputs/NCAA/line_nfl_2.csv", 
  #  "C:/Users/ckoho/Documents/Inputs/NCAA/line_nfl_3.csv", 
  "C:/Users/ckoho/Documents/Inputs/NCAA/line_nfl_4_hca2.csv", 
  altrep = FALSE)
df_teams <-  vroom(
  "C:/Users/ckoho/Documents/Inputs/NCAA/teams_nfl.csv", 
  altrep = FALSE)

#home court advantage NFL
#https://sportsbook.draftkings.com/nfl-odds-week-3
hca <- 2
#hca <- 2.5

for (j in 1:25) {
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
      "_nfl_week4_ratings_hca2.csv"
    )
  )
}
j <- 4
df_compare <- NULL
for (j in 2:4){
  df_games_to_predict <- vroom(
    "C:/Users/ckoho/Documents/Inputs/NCAA/line_nfl_5_hca2.csv", 
    altrep = FALSE)
  df_teams <- vroom(
    paste0(
      "C:/Users/ckoho/Documents/Inputs/NCAA/NFL_lines/25_nfl_week", 
      j, "_ratings_hca2.csv"), 
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
    mutate(weeks = j,
           delta = Spread - prediction,
           abs_delta = abs(Spread - prediction))
  write_csv(df_games_to_predict, paste0(
    "C:/Users/ckoho/Documents/Inputs/NCAA/NFL_lines/week", 
    j, "_prediction_hca2.csv"))
  df_compare <- df_compare %>%
    bind_rows(df_games_to_predict)
  
}


df_hca_2_summary<- df_compare %>%
  group_by(weeks) %>%
  summarize(error = mean(abs_delta),
            max = max(abs_delta))

write_csv(df_lines, paste0(
  "C:/Users/ckoho/Documents/Inputs/NCAA/NFL_summary_hca2.csv"
  ))



