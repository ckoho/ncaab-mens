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

#Explanation of how pace is calculated
#Need to figure out how Torvik package calculates.
#Show distribution of games by each year
i <- 2009
df_pace <- NULL
df_preodds <- NULL

#This for loop pulls in all games to evaluate.
for (i in 2008:2024){
  df_year <- vroom(paste0("C:/Users/ckoho/Documents//Inputs/NCAA/torvik_box_score_", i, ".csv"))
  df_year <- df_year %>% 
    select(type, season, tempo, date, loc, team1_pts, team2_pts)
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

#Create normal distributions by year.
ggplot(df_pace, aes(tempo, color = factor(season))) + 
  scale_y_continuous(breaks = NULL) + geom_density()
#ggsave("tempo_year_distribution.png")
df_summary <- df_pace %>%
  group_by(season) %>%
  summarize(mean(tempo))
#write_csv(df_summary, "tempo_year_mean.csv")


#Smoothed data over the year
#Does it matter if the game is in conference?
#Does it matter if the game is a conference tournament?
df_season_2008 <- df_pace %>%
  filter(season == 2008)
ggplot(df_season_2008, aes(x = date, y = tempo)) + 
  geom_smooth()
ggplot(df_season_2008, aes(x = date, y = tempo)) + 
  geom_smooth() +
  geom_jitter()
ggplot(df_season_2008, aes(x = date, y = tempo, color = type)) + 
  geom_smooth() +
  geom_jitter()
ggplot(df_season_2008, aes(x = date, y = tempo, color = type)) +
  geom_smooth()

df_pace %>%
  filter(season == 2008) %>%
  summarize(mean(tempo))

df_season_2023 <- df_pace %>%
  filter(season == 2023)
ggplot(df_season_2023, aes(x = date, y = tempo)) + geom_smooth()
ggplot(df_season_2023, aes(x = date, y = tempo)) + geom_smooth() +
  geom_jitter()
ggplot(df_season_2023, aes(x = date, y = tempo, color = type)) + 
  geom_smooth() +
  geom_jitter()
ggplot(df_season_2023, aes(x = date, y = tempo, color = type)) + 
  geom_smooth()

df_pace %>%
  filter(season == 2023) %>%
  summarize(mean(tempo))

df_pace %>%
  filter(season == 2023) %>%
  group_by(type) %>%
  summarize(mean(tempo))

df_pace %>%
  filter(season == 2008) %>%
  group_by(type) %>%
  summarize(mean(tempo))

df_pace %>%
  filter(season == 2008) %>%
  summarize(mean(tempo))

#Calculate number of days into the season the day is.
df_pace_days <- df_pace %>%
  mutate(daymonth = format(as.Date(date), "%m-%d")) %>%
  group_by(season) %>%
  mutate(days = date - min(date))

#Plot all years in terms of days into the season.
ggplot(df_pace_days, aes(x = days, y = tempo)) + 
  geom_smooth()
ggplot(df_pace_days, aes(x = days, y = tempo)) + 
  geom_smooth() + 
  geom_jitter()
ggplot(df_pace_days, aes(x = days, y = tempo, color = type)) + 
  geom_smooth() +
  geom_jitter()
ggplot(df_pace_days, aes(x = days, y = tempo, color = type)) + 
  geom_smooth()

#Calculate number of days into the season the day is and how the pace compares to
#the season mean.
df_pace_days <- df_pace %>%
  mutate(daymonth = format(as.Date(date), "%m-%d")) %>%
  group_by(season) %>%
  mutate(days = date - min(date),
         tempo_ratio= tempo / mean(tempo))

#Plot all years in terms of days into the season and pace normalized to mean.
ggplot(df_pace_days, aes(x = days, y = tempo, color = type)) + 
  geom_smooth()
ggplot(df_pace_days, aes(x = days, 
                         y = tempo_ratio, 
                         color = type)) + 
  geom_smooth()

ggplot(df_pace_days, aes(x = days, 
                         y = tempo_ratio, 
                         color = factor(season))) + 
  geom_smooth()

df_pace_days_conf <- df_pace_days %>%
  filter(type == "conf")
df_pace_days_conf_t <- df_pace_days %>%
  filter(type == "conf_t")
df_pace_days_nc <- df_pace_days %>%
  filter(type == "nc")
df_pace_days_post <- df_pace_days %>%
  filter(type == "post")

ggplot(df_pace_days_conf, aes(x = days, 
                              y = tempo_ratio, 
                              color = factor(season))) + 
  geom_smooth()
ggplot(df_pace_days_conf_t, aes(x = days, 
                                y = tempo_ratio, 
                                color = factor(season))) + 
  geom_smooth()
ggplot(df_pace_days_nc, aes(x = days, 
                            y = tempo_ratio, 
                            color = factor(season))) + 
  geom_smooth()
ggplot(df_pace_days_post, aes(x = days, 
                              y = tempo_ratio, 
                              color = factor(season))) + 
  geom_smooth()

ggplot(df_pace_days, aes(x = days, 
                         y = tempo_ratio, 
                         color = factor(season))) + 
  geom_smooth() +
  xlim(-.1,50)

ggplot(df_pace_days, aes(x = days, 
                         y = tempo_ratio, 
                         color = factor(season))) + 
  geom_smooth(span = .3, 
              se = FALSE) +
  xlim(-.1,50)

ggplot(df_pace_days, aes(x = days, 
                         y = tempo_ratio, 
                         color = factor(season))) + 
  geom_smooth(se = FALSE)


#Does neutral site matter???
#Data seems to say no.

ggplot(df_pace_days_nc, aes(x = days, 
                            y = tempo_ratio, 
                            color = factor(loc))) + 
  geom_smooth()
#ggsave("nc_tempo_days_site_smooth.png")
ggplot(df_pace_days_conf_t, aes(x = days, 
                                y = tempo_ratio, 
                                color = factor(loc))) + 
  geom_smooth()



#Does MOV matter? Look at non conference games and final score. Not perfect 
#Yes score does matter.
#proxy for how close a game is, so adding in pre game line could help.
#Team 2 is home team.
df_pace_days_nc <- df_pace_days_nc %>%
  mutate(mov = abs(team2_pts - team1_pts),
         mov_group = case_when(mov < 6 ~ "1_close",
                               mov < 11 ~ "2_med",
                               .default = "3_large"))

ggplot(df_pace_days_nc, aes(x = days, 
                            y = tempo_ratio, 
                            color = factor(mov_group))) + 
  geom_smooth()
ggsave("mov_days_pace.png")




