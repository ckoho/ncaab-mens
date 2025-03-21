#bayes_pace_boxscore.R
#This documents the process of predicting the pace of a game using a 
#bayesian model.

#library(sportsdataverse)
#library(gamezoneR)
library(tidyverse)
#library(jsonlite)
#library(cli)
#library(withr)
library(readr)
library(vroom)
library(fs)
library(brms)

library(bayesplot)
#Explanation of how pace is calculated
#Need to figure out how Torvik package calculates.
#Show distribution of games by each year
i <- 2009
df_pace <- NULL
df_preodds <- NULL

#This for loop pulls in all games to evaluate.
for (i in 2008:2024){
  df_year <- vroom(paste0("C:/Users/ckoho/Documents/Inputs/NCAA/torvik_box_score_", i, ".csv"))
  df_year <- df_year %>% 
    select(type, season, tempo, date, loc, team1, team1_pts, team2, team2_pts)
  df_pace <- rbind(df_pace, df_year)
  df_year <- vroom(paste0("C:/Users/ckoho/Documents/Inputs/NCAA/Torvik/results_eoy_torvik_", i, "_mbb_box_score.csv"))
  #Want to make a unique code for each game.
  df_year <- df_year %>%
    select(team1_odds, team2_odds
           ) %>%
    mutate(adj_odds = pmax(team1_odds, team2_odds))
  df_preodds <- rbind(df_preodds, df_year)
  #Once unique code, use that ID to join.  
  df_pace_odds <- df_pace %>%
    cbind(df_preodds)
    #left_join(df_preodds, by = c("date", "win", "loss", "team1", 
    #"team2", "season"))
}
#Round to make possessions continuous
#Calculate days since season started.
df_pace_odds <- df_pace_odds %>%
  mutate(
    tempo = round(tempo)
    )
df_pace_odds <- df_pace_odds %>%
  group_by(season) %>%
  mutate(
    days_since_start = date - min(date)
  )

#Building a Bayesian model for pace.
#Option 1 One hot encoding for game type.
df_pace_onehot <- df_pace_odds %>%
  mutate(
    type_nc = ifelse(type == 'nc', 1, 0),
    type_conf = ifelse(type == 'conf', 1, 0),
    type_conf_t = ifelse(type == 'conf_t', 1, 0),
    type_post = ifelse(type == 'post', 1, 0)
     )



#############################################################
### WORK TO DO                                            ###
#############################################################


#
# Option 2 model.matrix
#
#
# # Convert 'type' into dummy variables automatically
# type_dummies <- model.matrix(~ type - 1, data = df)
# 
# # Bind the dummy variables back to the original DataFrame
# df <- cbind(df, type_dummies)
#
#
# Option 3. Avoiding colinearity. Drop one of the dummy columns.
# df <- df %>%
#   mutate(
#     type_conf = ifelse(type == 'conf', 1, 0),
#     type_conf_t = ifelse(type == 'conf_t', 1, 0),
#     type_tournament = ifelse(type == 'tournament', 1, 0)
#   )
# # Drop 'nc' as baseline category

#############################################################
### WORK TO DO                                            ###
#############################################################
#Need to pull in average team pace, just to start training
#11/12 Goal
#df_pace_onehot[5091,4] <- lubridate::as_date("2008-03-14")

df_pace_avg <- df_pace_onehot %>%
  select(season, date, team1, team2, tempo) %>%
  pivot_longer(cols = starts_with("team")) %>% 
  arrange(date)


df_pace_avg <- df_pace_avg %>%
  group_by(season, value) %>%
  mutate(team_tempo = lag(cummean(tempo), default = NA)) %>%  # Cumulative avg pace for team1 up to current game
  ungroup()  %>%
  mutate(team_tempo = replace_na(team_tempo, 68)) %>%
  select(-tempo, -name)

df_pace_onehot <- df_pace_onehot %>%
  left_join(df_pace_avg, by = c("season", "date", "team1" = "value")) %>%
  rename(team1_pace = team_tempo) %>%
  left_join(df_pace_avg, by = c("season", "date", "team2" = "value")) %>%
    rename(team2_pace = team_tempo)
  


####### Train/Test breakdown
df_pace_onehot_train <- df_pace_onehot %>%
  filter(season < 2018)

df_pace_onehot_test <- df_pace_onehot %>%
  filter(season > 2017)






################
#Random slopes #
################
# Define priors
priors <- c(
  set_prior("normal(68, 6)", class = "Intercept"),  # Prior for intercept
  set_prior("normal(0, 2)", class = "b")             # Priors for fixed effects
)

# Fit the Bayesian Poisson model with random slopes for team pace.
model <- brm(
  tempo ~ (1 + team1_pace | team1) + (1 + team2_pace | team2) +  # Random slopes for pace
    type + # Other predictors
    days_since_start + adj_odds,                    # Other predictors
  data = df_pace_onehot_train,        # Training data
  family = poisson(),                 # Poisson distribution for count data
  prior = priors,                     # Defined priors
  chains = 4,                         # Number of Markov chains
  iter = 2000,                        # Iterations per chain
  control = list(adapt_delta = 0.95)  # To help with convergence
)


# Fit the Bayesian Poisson model with random slopes for team pace.
model_onehot <- brm(
  tempo ~ (1 + team1_pace | team1) + (1 + team2_pace | team2) +  # Random slopes for pace
    type_nc + type_conf + type_conf_t + type_post + # Other predictors
    days_since_start + adj_odds,                    # Other predictors
  data = df_pace_onehot_train,        # Training data
  family = poisson(),                 # Poisson distribution for count data
  prior = priors,                     # Defined priors
  chains = 4,                         # Number of Markov chains
  iter = 2000,                        # Iterations per chain
  control = list(adapt_delta = 0.95)  # To help with convergence
)
# Summarize the model
summary(model_onehot)

mcmc_plot(model_onehot, type = "trace")
