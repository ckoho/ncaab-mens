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
library(ggpmisc)
library(gt)
library(gtExtras)

#sportsdataverse_packages()
#usethis::use_git_config(user.name = "ckohoutek", user.email = "ckohoutek@gmail.com")
#credentials::set_github_pat("YourPAT")
hoopR::load_mbb_team_box()

#gamezoneR
pbp <- gamezoneR::load_gamezone_pbp(gamezoneR:::available_seasons())    
length(unique(pbp$game_id))
pbp %>% dplyr::filter(!is.na(loc_x)) %>% nrow()

#torvik_bs <- toRvik::bart_game_box(2008)
#torvik_gf <- toRvik::bart_game_factors(2008)
mbb_pbp <-  hoopR::load_mbb_pbp(2006:2021)
#mbb_bs <- hoopR::load_mbb_team_box(2013)
#mbb_filter <  hoopR::espn_mbb_betting(401253836)
#pbp <- hoopR::espn_mbb_pbp(401253836)
#hoopR::kp_box(458, 2013)
#getgamestats.php?year=2008&tvalue=Memphis
year <- 2022

season <- 2008
for (season in 2008:2022){
  df <- bart_expanded_box_score(season)
  write_csv(df, paste0("torvik_box_score_", season, ".csv"))
}




season <- 2008
df_torvik <- vroom(paste0("torvik_box_score_", season, ".csv"))
df_espn <- vroom(paste0("espn_line_information_", season, ".csv"))













####NEEDS REVIEW!!!!
#####################################################################
### Checking what K value is best on train data set.              ###
#####################################################################

#Exclude first two years to allow stabilization
k_loop <- c(25, 26, 27, 28, 29, 30, 31, 32, 33, 34)
k_loop <- c(1, 5, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 25, 26, 27, 28, 29, 30, 31, 32, 35, 40, 45)
k_loop <- c(26, 27, 28, 29, 31, 32)

df_box_score_all <- tibble()
for (k in k_loop){
  for (year in 2009:2014){
    df <- vroom(paste("results_eoy_", k, "_", year, "_mbb_box_score.csv", 
                      sep = ""))
    df$k <- k
    df$year <- year
    df <- df %>%
      mutate(rounded = round(team2_odds, 2))
    df_box_score_all <- df_box_score_all %>%
      bind_rows(df)
  }
}
df_summary_year <- df_box_score_all %>%
  group_by(k, year, rounded) %>%
  summarize(mean_line = mean(team2_odds),
            mean_result = mean(result),
            median_result = median(result),
            n = n()) %>%
  filter(k == 27)

df_summary_all <- df_box_score_all %>%
  group_by(k, rounded) %>%
  summarize(mean_line = mean(team2_odds),
            mean_result = mean(result),
            median_result = median(result),
            n = n()) %>%
  #filter(k > 20,
  #       k < 35)
  filter(k == 27)

ggplot(df_summary_all, aes(x=rounded, y=mean_result)) + geom_point(aes(color=as.factor(k))) + 
  xlim(0,1) + ylim(0,1) + geom_abline(intercept = 0, slope = 1)

ggplot(df_summary_year, aes(x=rounded, y=mean_result)) + geom_point(aes(color=as_factor(year))) + 
  xlim(0,1) + ylim(0,1) + geom_abline(intercept = 0, slope = 1)
ggsave("prediction_accuracy_k27_plot.png")

##########################
###Log loss calculation
#########################
k_loop <- c(1, 5, 10, 15, 20, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 35)
k_loop <- c(1, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 25, 30, 40)
k_loop <- c(1, 5, 10, 15, 20, 25, 30, 35)
df_box_score_all <- tibble()
for (k in k_loop){
  for (year in 2009:2014){
    df <- vroom(paste("results_eoy_", k, "_", year, "_mbb_box_score.csv", 
                      sep = ""))
    df$year <- year
    df$k <- k
    #print(k)
    #print(year)
    df <- df %>%
      mutate(eq1 = result * log(team2_odds),
             eq2 = (1 - result) * log(1-team2_odds),
             logloss = -(eq1 + eq2)
      )
    df <- df %>%
      mutate(rounded = round(team2_odds, 2))
    df_box_score_all <- df_box_score_all %>%
      bind_rows(df)
  }
}
df_summary_all <- df_box_score_all %>%
  group_by(k) %>%
  summarize(avg_logloss = mean(logloss),
            median_logloss = median(logloss),
            n = n())

df_summary_year <- df_box_score_all %>%
  group_by(k, year) %>%
  summarize(avg_logloss = mean(logloss),
            median_logloss = median(logloss),
            n = n())

df_summary_year_reduced <- df_box_score_all %>%
  group_by(k, year) %>%
  summarize(avg_logloss = mean(logloss),
            median_logloss = median(logloss),
            n = n()) %>%
  filter(k>25,
         k < 32)

ggplot(df_summary_all, aes(x = k, y = avg_logloss)) + geom_point()
ggplot(df_summary_year, aes(x = k, y = avg_logloss)) + 
  geom_point(aes(color = factor(year))) 
ggplot(df_summary_year, aes(x = k, y = avg_logloss)) + 
  geom_point(aes(color = factor(year)))  + ylim(.51,.54)
ggsave("k_evaluation_logloss.png")

df_summary_year <- df_summary_year %>% 
  filter(k < 40) %>%
  filter(k > 20) %>%
  mutate(k = as.character(k))
ggplot(df_summary_year, aes(x = year, y = avg_logloss, color = k)) + 
  geom_jitter()
ggsave("k_evaluation_year_zoom.png")

df_box_score_all <- df_box_score_all %>%
  select(date, team_a, team_h, conference_a, conference_h, 
         odds, result, k, year, eq1, eq2, logloss, rounded)


df <- vroom("k_large_analysis.csv")
df_summary_all <- df %>%
  #  group_by(k, year) %>%
  group_by(k) %>%
  summarize(avg_logloss = mean(logloss),
            median_logloss = median(logloss),
            n = n())








####################
###Checking the percentiles over time.
####################
k_loop <- c(1, 5, 10, 15, 20, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 35)
k <- 1
df_eoy_percentile <- tibble()
for (k in k_loop){
  #Read in master file that has all previous year ELO ranking. Should be just 
  #the latest end of year.
  df <- vroom(paste0("mbb_elo_", k, "_2014.csv")) %>%
    #Select only the years and elo columns of interest.
    select(team, elo_2008, elo_2009, elo_2010, elo_2011, elo_2012, elo_2013, 
           elo_2014)
  #pivot all elo's into one dateframe.
  df <- df %>%
    pivot_longer(!team, names_to = "year", values_to = "elo") %>%
    group_by(year) %>%
    #Calculate the percentiles for each year.
    summarise(enframe(quantile(elo, c(.01, .1, 0.25, 0.5, 0.75, .9, .99)), 
                      "quantile", "elo")) %>%
    #Remove string "elo_" and cast year to int.
    mutate(year = as.integer(str_replace_all(year, "elo_", "")),
           k = k)
  df_eoy_percentile <- df_eoy_percentile %>%
    bind_rows(df)
  
}

#Filter to 99% 
df_eoy_99 <- df_eoy_percentile %>%
  filter(quantile == "99%",
         k > 20)
ggplot(df_eoy_99, aes(x = year, y = elo, color = factor(k))) + 
  geom_line() +geom_point()


#Filter to 90% 
df_eoy_90 <- df_eoy_percentile %>%
  filter(quantile == "90%",
         k > 20)
ggplot(df_eoy_90, aes(x = year, y = elo, color = factor(k))) + 
  geom_line() +geom_point()

#Filter to 75% 
df_eoy_75 <- df_eoy_percentile %>%
  filter(quantile == "75%",
         k > 20)
ggplot(df_eoy_75, aes(x = year, y = elo, color = factor(k))) + 
  geom_line() +geom_point()

#Filter to 50% 
df_eoy_50 <- df_eoy_percentile %>%
  filter(quantile == "50%",
         k > 20)
ggplot(df_eoy_50, aes(x = year, y = elo, color = factor(k))) + 
  geom_line() +geom_point()

#Filter to 1% 
df_eoy_1 <- df_eoy_percentile %>%
  filter(quantile == "1%",
         k > 20)
ggplot(df_eoy_1, aes(x = year, y = elo, color = factor(k))) + 
  geom_line() +geom_point()



#####################
### ELO improvements analysis
#####################

# Home and home results.
files <- fs::dir_ls(path = "../../Inputs/NCAA/", 
                    regexp = "torvik_box_score.*.csv")
df_torvik <- vroom(files) 
df_torvik <- df_torvik %>%
  filter(loc == "A")
df_torvik <- df_torvik %>%
  mutate(result = if_else(team2 == win, 1, 0)) %>%
  select(team1, team2, result, season, team1_pts, team2_pts)
df_repeat <- df_torvik %>%
  inner_join(df_torvik, by = c("team1" = "team2", "team2" = "team1", 
                        "season" = "season")) %>%
  mutate(game1_diff = team2_pts.x - team1_pts.x,
         game2_diff = team1_pts.y - team2_pts.y) 
df_summary <- df_repeat %>%
  group_by(game1_diff) %>%
  summarise(game2_diff = mean(game2_diff),
            n = n(),
            result = mean(result.y)) %>%
  filter(n > 100)
  #filter(n > 27)
df_summary <- df_summary %>%
  mutate(rating_delta = (400 * log ((1 - result)/ result))/log(10)/2)
  
ggplot(df_summary, aes(x=game1_diff, y=game2_diff)) + 
  #geom_point(aes(size=n)) +
  geom_point() + 
  scale_x_continuous(breaks= seq(-30,30, by=5), limits = c(-30,30)) + 
  scale_y_continuous(breaks= seq(-10, 8, by=2), limits = c(-15,8)) + 
  geom_hline(yintercept=0)

ggplot(df_summary, aes(x=game1_diff, y=result)) + 
  geom_point(aes(size=n)) + geom_hline(yintercept=.5)


formula <- y ~ x
ggplot(df_summary, aes(x=rating_delta, y=game2_diff)) + 
  geom_point(aes(size=n)) +
  geom_smooth(method = "lm", se = FALSE) + 
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 3) +
  geom_abline(aes(intercept = 0, slope = .0815))

ggplot(df_summary, aes(x=rating_delta, y=game2_diff)) + 
  geom_point(aes(size=n)) +
  geom_abline(aes(intercept = 0, slope = .0815))

#Elo results predicted odds (converted to line) versus results.

#Look at year over year distribution. Density plot or violin plot.
#If spread gets bigger, look into autocorrelation.






#Convert team win percentage to line.
for (season in 2008:2022){
  df <- vroom(paste0("../../Inputs/NCAA/results_eoy_", season,
                     "_mbb_box_score.csv"), altrep = FALSE)
  df <- df %>%
    mutate(rating_delta = (400 * log ((1 - team2_odds)/ team2_odds))/
             log(10)/2) %>%
    mutate(line = rating_delta * .0815)
  
  write_csv(df, paste0("../../Inputs/NCAA/results_eoy_", season,
                       "_mbb_box_score.csv"))
}
df_summary <- df_summary %>%
  mutate(rating_delta = (400 * log ((1 - result)/ result))/log(10)/2) %>%
  mutate(line = raing_delta * .0815)



#Compare adjust elo to regular elo lines
df_elo <- tibble()
#Looking at 2009-2014. Compute the score delta and the rounded odds.
for (year in 2009:2014){
  link <- paste0("C:/Users/ckoho/Documents/Inputs/NCAA/results_eoy_", 
                 year, "_mbb_box_score.csv")
  df <- vroom(link)
  df <- df %>%
    mutate(score = team2_pts - team1_pts,
           rounded = round(team2_odds, 2))
  df_elo <- df_elo %>%
    bind_rows(df)
}

#Same for adjusted elo ratings. This path will move.
df_aelo <- tibble()
for (year in 2009:2014){
  link <- paste0("results_eoy_", 
                 year, "_mbb_box_score.csv")
  df <- vroom(link)
  df <- df %>%
    mutate(score = team2_pts - team1_pts,
           rounded = round(team2_odds, 2))
  df_aelo <- df_aelo %>%
    bind_rows(df)
}

#AELO needs results calculated.
df_aelo <- df_aelo %>%
  mutate(result = if_else(team2 == win, 1, 0))
#compute actual win rate based on predicted odds.  
df_elo_summary <- df_elo %>%
  group_by(rounded) %>%
  summarize(mean_line = mean(team2_odds),
            mean_result = mean(result),
            median_result = median(result),
            n = n()) 
df_aelo_summary <- df_aelo %>%
  group_by(rounded) %>%
  summarize(mean_line = mean(team2_odds),
            mean_result = mean(result),
            median_result = median(result),
            n = n()) 

ggplot(df_elo_summary, aes(x=rounded, y=mean_result)) + geom_point(aes()) + 
  xlim(0,1) + ylim(0,1) + geom_abline(intercept = 0, slope = 1)

ggplot(df_aelo_summary, aes(x=rounded, y=mean_result)) + geom_point(aes()) + 
  xlim(0,1) + ylim(0,1) + geom_abline(intercept = 0, slope = 1)

df_elo <- df_elo %>%
  mutate(rounded_line = -1 * round(line, 0))

df_aelo <- df_aelo %>%
  mutate(rounded_line = -1 * round(line, 0))

df_elo_summary_line <- df_elo %>%
  group_by(rounded_line) %>%
  summarize(mean_line = mean(rounded_line),
            mean_result = mean(score),
            median_result = median(score),
            n = n()) 
df_aelo_summary_line <- df_aelo %>%
  group_by(rounded_line) %>%
  summarize(mean_line = mean(rounded_line),
            mean_result = mean(score),
            median_result = median(score),
            n = n()) 

ggplot(df_elo_summary_line, aes(x=rounded_line, y=mean_result)) + 
  geom_point(aes(size = n)) +
  geom_abline(intercept = 0, slope = 1)

ggplot(df_aelo_summary_line, aes(x=rounded_line, y=mean_result)) +  
  geom_point(aes(size = n)) +
  geom_abline(intercept = 0, slope = 1)

df1 <-df_elo_summary_line %>%
  mutate(type = "ELO")
df2 <-df_aelo_summary_line %>%
  mutate(type = "AELO")

df <- df1 %>%
  bind_rows(df2) %>%
  filter(n > 50)

ggplot(df, aes(x=rounded_line, y=mean_result)) +  
  geom_point(aes()) +
  geom_abline(intercept = 0, slope = 1) + 
  xlim(-30,30) + ylim(-30,30) +
  facet_wrap(~ type)
ggsave("elo_aleo_line_results.png")


df1 <-df_elo_summary %>%
  mutate(type = "ELO")
df2 <-df_aelo_summary %>%
  mutate(type = "AELO")

df <- df1 %>%
  bind_rows(df2) %>%
  filter(n > 50)

ggplot(df, aes(x=rounded, y=mean_result)) +  
  geom_point(aes()) +
  geom_abline(intercept = 0, slope = 1) + 
  xlim(0,1) + ylim(0,1) +
  facet_wrap(~ type)
ggsave("elo_aleo_results.png")

#Seeing how often team beat the calculated line based on the spread.
df_elo1 <- df_elo %>%
  mutate(beat_line = if_else(score > (-1 * line),1,0))


df_elo1_summary <- df_elo1 %>%
  group_by(rounded) %>%
  summarize(mean_line = mean(team2_odds),
            mean_result = mean(result),
            median_result = median(result),
            n = n(),
            mean_beat_line = mean(beat_line))  %>%
  filter(n > 50)


ggplot(df_elo1_summary, aes(x=rounded, y=mean_beat_line)) + geom_point(aes()) 




#############################
### Violin Plot           ###
#############################
df_ranking <- vroom("C:/Users/ckoho/Documents/Inputs/NCAA/mbb_elo_2021.csv", 
                    altrep = FALSE)
#Select just each end of year ranking.
df_ranking <- df_ranking %>%
  select(team,elo_2008, elo_2009, elo_2010, elo_2011, elo_2012, elo_2013, elo_2014, 
         elo_2015, elo_2016, elo_2017, elo_2018, elo_2019, elo_2020, elo_2021) %>%
  pivot_longer(!team, names_to = "year", values_to = "rating")

#Violin plot of each year ratings.
ggplot(df_ranking, aes(year, rating)) + geom_violin(adjust = .5) #+ geom_jitter()

#Violin plot of each year ratings.
df_ranking_percentile <- df_ranking %>% 
  group_by(year) %>%  
  summarise(quantile = scales::percent(c(.01, .05, 0.1, 0.25, 0.5, 0.75, 0.9, 
                                         .95, 0.99)),
            rating = quantile(rating, c(.01, .05, 0.1, 0.25, 0.5, 0.75, 0.9, 
                                        .95, 0.99)))

#Plotting percentiles over time to check trend.
ggplot(df_ranking_percentile, aes(year, rating)) + 
  geom_point(aes(color = as_factor(quantile))) 





########################################################################
###Log Loss calculation. Comparing autocor, default, line, and aelo. ###
########################################################################

#autocor
files <- fs::dir_ls(path = "../../Inputs/NCAA/auto_corr/", 
                    regexp = "results_eoy.*.csv")
df_autocor <- vroom(files, altrep = FALSE)
df_autocor <- df_autocor %>%
  mutate(eq1 = result * log(team2_odds),
         eq2 = (1 - result) * log(1-team2_odds),
         logloss = -(eq1 + eq2)
         )
df_ac_sum <- df_autocor %>%
  group_by(season) %>%
  summarize(ac = mean(logloss))

#default
files <- fs::dir_ls(path = "../../Inputs/NCAA/", 
                    regexp = "results_eoy.*.csv")
df <- vroom(files, altrep = FALSE)
df <- df %>%
  mutate(eq1 = result * log(team2_odds),
         eq2 = (1 - result) * log(1-team2_odds),
         logloss = -(eq1 + eq2)
  )
df_def_sum <- df %>%
  group_by(season) %>%
  summarize(normal = mean(logloss))

#aelo
files <- fs::dir_ls(path = "../../Inputs/NCAA/aelo/", 
                    regexp = "results_eoy.*.csv")
df <- vroom(files, altrep = FALSE)
df <- df %>%
  mutate(eq1 = result * log(team2_odds),
         eq2 = (1 - result) * log(1-team2_odds),
         logloss = -(eq1 + eq2)
  )
df_aelo_sum <- df %>%
  group_by(season) %>%
  summarize(aelo = mean(logloss))

#line
files <- fs::dir_ls(path = "../../Inputs/NCAA/line/", 
                    regexp = "results_eoy.*.csv")
df <- vroom(files, altrep = FALSE)
df <- df %>%
  mutate(eq1 = result * log(team2_odds),
         eq2 = (1 - result) * log(1-team2_odds),
         logloss = -(eq1 + eq2)
  )
df_line_sum <- df %>%
  group_by(season) %>%
  summarize(line = mean(logloss))

df1 <- df_def_sum %>%
  left_join(df_ac_sum) %>%
  left_join(df_aelo_sum) %>%
  #left_join(df_line_sum) %>%
  pivot_longer(!season, names_to = "group", values_to = "logloss")
ggplot(df1, aes(x=season, y=logloss)) + geom_point(aes(color = group))
ggsave("logloss_options_noline.png")




#########################
# AELO adjust value   ###
#########################
l_loop <- c(0, .01, .05, .1, .15, .2, .25, .3, .4, .5, .75)
#This k loop is to be removed once values are chosen.
df_all <- tibble()
for (l in l_loop){
  for (season in 2008:2021){
      link <- paste0("../../Inputs/NCAA/aelo/results_eoy_", l, "_",
                     season, "_mbb_box_score.csv")
      df <- vroom(link)
      df <- df %>%
        mutate(eq1 = result * log(team2_odds),
               eq2 = (1 - result) * log(1-team2_odds),
               logloss = -(eq1 + eq2),
               l = l
        )
        df_all <- df_all %>%
          bind_rows(df)
      
  }
}
df_summary <- df_all %>%
  group_by(l) %>%
  summarize(logloss = mean(logloss))
write_csv(df_summary, "aelo_logloss_all.csv")
df_all %>%
  group_by(l) %>%
  summarize(logloss = mean(logloss)) %>%
  gt() %>%
  gt_color_rows(logloss,palette = "ggsci::default_gsea")
df_all_summary <- df_all %>%
  group_by(l, season) %>%
  summarize(logloss = mean(logloss))
ggplot(df_all_summary, aes(x=season, y=logloss)) + geom_point(aes(color = as_factor(l)))
ggsave("aelo_ggplot_logloss.png")
df_table <- df_all_summary %>%
  pivot_wider(names_from = season, values_from = logloss)
df_table %>%
  gt() %>%
  gt_color_rows(`2008`: `2021`,palette = "ggsci::default_gsea")  

write_csv(df_table, "elo_adjust_logloss.csv")
df_table <- df_all_summary %>%
  filter(l < .4  ) %>%
  pivot_wider(names_from = season, values_from = logloss)
df_table %>%
  gt() %>%
  gt_color_rows(`2008`: `2021`,palette = "ggsci::default_gsea")  

df_all_summary_reduced <- df_all_summary %>%
  filter(l < .4 & l > .1 )
ggplot(df_all_summary_reduced, aes(x=season, y=logloss)) + geom_point(aes(color = as_factor(l)))



df_all_summary_reduced <- df_all %>%
  group_by(l, season) %>%
  summarize(logloss = mean(logloss)) %>%
  filter(l < .4)
ggplot(df_all_summary_reduced, aes(x=season, y=logloss)) + geom_point(aes(color = as_factor(l)))









###################
### Align ESPN names with Torvik
###################
files <- fs::dir_ls(glob = "espn_line*") 
df_espn <- vroom(files)

#List all torvik names
#Pulling in all team names.
files <- fs::dir_ls(path = "../../Inputs/NCAA/", 
                    regexp = "torvik_box_score.*.csv")
df_names <- vroom(files) %>%
  select(team1, team2)
df_pivot <- df_names %>%
  pivot_longer(everything()) %>%
  select(value) %>%
  count(value) %>%
  arrange(n)
write_csv(df_pivot, "../../Inputs/NCAA/torvik_team_names.csv")


#List all ESPN names
#Pulling in all team names.
files <- fs::dir_ls(path = "../../Inputs/NCAA/", 
                    regexp = "espn_mbb_betting_lines.*.csv")

df_names <- vroom(files, col_select = c(team1_id,team2_id,team1_name,team2_name,team1_sname,team2_sname))





#############################
# PBP Analysis
#############################
progressr::with_progress({
  mbb_pbp <-  hoopR::load_mbb_pbp(2009)
})
mbb_pbp_2008_oregon <- mbb_pbp %>%
  filter(home_team_name == "Oregon",
         away_team_name == "California")
future::plan("multisession")
gamezoneR:::available_seasons()
progressr::with_progress({
  pbp <- gamezoneR::load_gamezone_pbp(gamezoneR:::available_seasons())
})
df_shots_only <- pbp %>%
  filter(!is.na(shot_desc))


pbp_2008_oregon <- pbp %>%
  filter(season == "2022-23",
         home == "Stetson",
         away == "Milwaukee")


length(unique(pbp$game_id))

df_sampled <- df_shots_only %>%
  sample_n(500)
df_count <- df_shots_only %>%
  group_by(loc_x,loc_y) %>%
  tally(sort = TRUE) %>%
  arrange()

df_round <- df_shots_only %>%
  mutate(round_loc_x = plyr::round_any(loc_x, 1),
         round_loc_y = plyr::round_any(loc_y, 1),
         shooting_per  = if_else(shot_outcome == "made", 100, 0))
df_per <- df_round %>%
  group_by(round_loc_x,round_loc_y) %>% 
  summarise(mean(shooting_per),
            n = n()) %>%
  rename(loc_x = round_loc_x,
         loc_y = round_loc_y,
         shooting_per = `mean(shooting_per)`) %>%
  arrange(n) %>%
  filter(n>2000)

ggplot(df_count, aes(y = n, x = loc_x)) + geom_jitter() + scale_y_log10()
ggplot(df_count, aes(y = n, x = loc_y)) + geom_jitter() + scale_y_log10()





#gamezoneR::base_court +
#  geom_jitter(data = df_sampled,
#              aes(loc_x, loc_y, color = shot_outcome),
#              alpha = 0.8, width=.5, height = .5) +
gamezoneR::base_court +
  geom_jitter(data = df_per,
             aes(loc_x, loc_y, color = shooting_per),
             alpha = 0.8, width=.5, height = .5) +
  theme(axis.line = element_blank(),
        axis.text= element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 30/.pt, margin = margin(0, 0, 5, 0)),
        plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 24/.pt),
        plot.caption = element_text(face = "italic", hjust = 1, size = 20/.pt, margin = margin(0, 0, 0, 0)),
        legend.spacing.x = grid::unit(0, 'cm'),
        legend.title = element_text(size = 20/.pt, face = "bold"),
        legend.text = element_text(size = 16/.pt),
        legend.margin = margin(0, 0, 0, 0),
        legend.position = 'bottom',
        legend.box.margin = margin(-35, 0, 0, 0),
        plot.margin = margin(5, 0, 5, 0)) +
  labs(title = "All of Zion Williamson's shots at Duke",
       subtitle = "2018-19 college basketball season",
       color = "Outcome",
       caption = "Chart: @jacklich10 | Data: @gamezoneR")




df_shots_only <- df_shots_only %>%
  mutate(shot_made = if_else(shot_outcome == "made", 1, 0),
         shot_value = if_else(three_pt == TRUE, 3, 2),
         shot_dist = ((loc_x - 25)^2 + (loc_y - 5.25)^2)^.5)

df_count <- df_shots_only %>%
  group_by(shot_dist) %>%
  tally(sort = TRUE)
ggplot(df_count, aes(y = n, x = shot_dist)) + geom_jitter() + scale_y_log10()


df_round <- df_shots_only %>%
  mutate(round_dist = plyr::round_any(shot_dist, 1)) %>%
  group_by(round_dist) %>%
  summarize(mean = mean(shot_made),
            n = n()) 

ggplot(df_round, aes(y = mean, x = round_dist, size = n)) + geom_point() 
