# if (!requireNamespace('pacman', quietly = TRUE)){
#   install.packages('pacman')
# }
# pacman::p_load_current_gh("saiemgilani/sportsdataverse-R")
#devtools::install_github(repo = "JackLich10/gamezoneR")
library(sportsdataverse)
library(gamezoneR)
library(tidyverse)
library(jsonlite)
library(cli)
library(withr)
library(readr)
library(fs)
library(vroom)

###############################
# Score Distribution
###############################
#Pull all games and the absolute score delta.
df_scores <- NULL
for (season in 2008:2022){
  #First we get all the boxscore data for each available season.
  df <- bart_expanded_box_score(season)
  df <- df %>%
    mutate(win = str_trim(win),
           loss = str_trim(loss),
           team2 = str_trim(team2),
           team1 = str_trim(team1))
  df_scores <- df_scores %>%
    bind_rows(df)
}  
df_scores <- df_scores %>%
  mutate(delta = team1_pts - team2_pts,
         abs_delta = abs(team1_pts - team2_pts))



###############################
# Plotting Score Distribution
###############################
ggplot(df_scores, aes(x=abs_delta)) +
  geom_histogram(binwidth=1) +
  xlim(0,50) #+
  #facet_wrap(~type) #+ 
  #facet_wrap(~season) #+ 


#############################
# Gambling Lines 
##############################
