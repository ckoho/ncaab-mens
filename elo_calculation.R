#elo_calculation.R
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




k <- 25



#DEBUG
#k_loop <- c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45)
#k_loop <- c(26, 27, 28, 29)
#k_loop <- c(34)
for (season in 2008:2022){
df_ratings <- vroom("
                    ", altrep = FALSE)
#ncaa_season <- years[1]
for (ncaa_season in years){
  if (ncaa_season == "2020_2021"){
    home_court <- 49
  }else{
    home_court <- 70
  }
  df_year_box_score <- vroom(paste(ncaa_season, 
                                   "_cleaned_mbb_box_score.csv", sep = ""))
  df_year_box_score$odds <- 0
  df_year_box_score$result <- 0
  df_year_box_score$line <- 0
  df_year_box_score$year <- ncaa_season
  df_ratings <- year_elo_ratings(df_year_box_score, df_ratings, 
                                 home_court, k)
  
  year <- substr(ncaa_season,6,10)
  elo_year <- paste(year, "elo", sep = "_")
  year1 <- strtoi(year) - 1
  elo_year1 <- paste(year1, "elo", sep = "_")
  
  #Set end of year ratings to year
  df_ratings <- df_ratings %>%
    mutate( !!elo_year := elo)
  #write_csv(df_ratings, paste("mbb_elo_", ncaa_season, ".csv", sep = ""))
  write_csv(df_ratings, paste("mbb_elo_", k, "_", 
                              ncaa_season, ".csv", sep = ""))
  
  #Regress to baseline for next year.
  #Regression calculation = 50% end of year, 3%0 n -1, and 20% mean (1500)
  df_ratings["elo"] <- ((.5 * df_ratings[elo_year]) + (.2 * 1500) + 
                          (.3 * df_ratings[elo_year1]))
  df_ratings <- df_ratings %>%
    mutate(W = 0,
           L = 0,
           ConfW = 0,
           ConfL = 0
    )
  
  write_csv(df_ratings, "mbb_elo.csv")
}
}
