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
#sportsdataverse_packages()
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


bart_expanded_box_score<- function(year = current_season()) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
          2008)) {
      cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2008!")
    } else {
      box_score_duplicates <- c(
        "fgm", "fga",  "tpm", "tpa", "ftm", "fta", "oreb", "dreb", "reb", "ast", "stl", "blk", "to", "pf", "pts")
      gf_specifc <- c(
        "result", "adj_o", "adj_d")
      gf_duplicates <-  c(
        "ppp", "efg", "to_per", "or_per", "ftr")
      gf_other <- c( 
        "game_score", "opp_conf", "season", "tempo", "game_id", "coach", "opp_coach", "avg_marg", "opp_avg_marg")
      x1_unique <- c(
        "date", "type", "team1", "team1_conf", "team2", "loc", "team1_adj_o", "team1_adj_d", "team1_game_score", "team2_conf", "season", "tempo", "game_id", "team1_coach", "team2_coach", "team1_avg_margin", "team1_opp_avg_margin")
      x2_unique <- c(
        "adj_o", "adj_d", "game_score", "game_id", "avg_margin", "opp_avg_margin")
      x <- jsonlite::fromJSON(paste0("https://barttorvik.com/getgamestats.php?year=", year))
      x <- dplyr::as_tibble(x)
      
      x1 <- x %>%
        dplyr::filter(V22 == 1) %>%
        dplyr::select(-c(7, 22, 31)) %>%
        dplyr::rename_at(c(1:8, 19:27), ~ x1_unique) %>%
        dplyr::rename_at(9:13, ~ paste0("team1_", gf_duplicates)) %>%
        dplyr::rename_at(14:18, ~ paste0("team2_", gf_duplicates)) %>%
        tidyr:: separate("V30", c("date" , "min", "team1", "team2",  paste0("team1_", box_score_duplicates), paste0("team2_", box_score_duplicates), "pos", 
                                  NA, "win", "loss"
                                  ), ","
                         )               
                      
      x2 <- x %>%
        dplyr::filter(V22 == 2) %>%
        dplyr::select(c(8, 9, 20, 25, 28, 29)) %>%
        dplyr::rename_all(~paste0("team2_", x2_unique)) %>%
        dplyr::rename_at(4, ~"game_id")
      x <- x1 %>%
        dplyr::full_join(x2, by = "game_id") %>%
        dplyr::mutate(
          date = lubridate::mdy(date),
          type = dplyr::case_when(
            type == 0 ~ "nc",
            type == 1 ~ "conf",
            type == 2 ~ "conf_t",
            type == 3 ~ "post",
            TRUE ~ "nond1"
            ),
          across(c(4:16, 18:19, 23:24, 26, 29:59,62:66), as.numeric),
          loss = stringr::str_remove_all(loss, "]")
          ) %>%
        dplyr::relocate(dplyr::starts_with("team1"), .after="loss") %>%
        dplyr::relocate(dplyr::starts_with("team2"), .after="team1_pts") %>%
        dplyr::arrange(date)
      }
    })}

df_2008 <- bart_expanded_box_score(2008)
df_2022 <- bart_expanded_box_score(2022)
