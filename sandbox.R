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

season <- 2022
#Playing with loading ESPN line data
for (season in 2008:2021){
  df_gi <- hoopR::load_mbb_team_box(season) %>%
    select(game_id) %>%
    group_by(game_id) %>%
    summarise(n = n()) %>%
    select(game_id)
  df_line <- NULL
  #Will turn this into function
  #Goal is to save teams, date, and lines
  for (i in 1:nrow(df_gi)){
    df_line <- df_line %>%
      bind_rows(try(espn_mbb_lines(df_gi[[i,1]])))
  }
  write_csv(df_line, paste0("espn_line_information_", season, ".csv"))
}




season <- 2008
df_torvik <- vroom(paste0("torvik_box_score_", season, ".csv"))
df_espn <- vroom(paste0("espn_line_information_", season, ".csv"))

season <- 2015
#Adding in short team name to espn df.
#join ESPn box score with line info by game id and one other attribute.
#Still a work in progress!!!
for (season in 2013:2022){
  df_torvik <- vroom(paste0("torvik_box_score_", season, ".csv"), 
                     altrep = FALSE)
  df_espn <- vroom(paste0("espn_line_information_", season, ".csv"), 
                   altrep = FALSE)
  df_espn <- df_espn %>%
    dplyr::mutate(
      team_short_display_name = dplyr::case_when(
        team_short_display_name == "Abilene Chrstn" & team_name == "Wildcats" & team_abbreviation == "ACU" ~"Abilene Christian",
        team_short_display_name == "Alabama State" & team_name == "Hornets" & team_abbreviation == "ALST" ~"Alabama St.",
        team_short_display_name == "Alabama St" & team_name == "Hornets" & team_abbreviation == "ALST" ~"Alabama St.",
        team_short_display_name == "Alcorn State" & team_name == "Braves" & team_abbreviation == "ALCN" ~"Alcorn St.",
        team_short_display_name == "Alcorn St" & team_name == "Braves" & team_abbreviation == "ALCN" ~"Alcorn St.",
        team_short_display_name == "Appalachian St" & team_name == "Mountaineers" & team_abbreviation == "APP" ~"Appalachian St.",
        team_short_display_name == "Arizona State" & team_name == "Sun Devils" & team_abbreviation == "ASU" ~"Arizona St.",
        team_short_display_name == "Ark-Pine Bluff" & team_name == "Golden Lions" & team_abbreviation == "UAPB" ~"Arkansas Pine Bluff",
        team_short_display_name == "AR-Pine Bluff" & team_name == "Golden Lions" & team_abbreviation == "UAPB" ~"Arkansas Pine Bluff",
        team_short_display_name == "Arkansas State" & team_name == "Red Wolves" & team_abbreviation == "ARST" ~"Arkansas St.",
        team_short_display_name == "Ball State" & team_name == "Cardinals" & team_abbreviation == "BALL" ~"Ball St.",
        team_short_display_name == "Bethune-Cookman" & team_name == "Wildcats" & team_abbreviation == "BCU" ~"Bethune Cookman",
        team_short_display_name == "Boise State" & team_name == "Broncos" & team_abbreviation == "BSU" ~"Boise St.",
        team_short_display_name == "Boston U" & team_name == "Terriers" & team_abbreviation == "BU" ~"Boston University",
        team_short_display_name == "Boston Univ" & team_name == "Terriers" & team_abbreviation == "BU" ~"Boston University",
        team_short_display_name == "CA Baptist" & team_name == "Lancers" & team_abbreviation == "CBU" ~"Cal Baptist",
        team_short_display_name == "CSU Bakersfield" & team_name == "Roadrunners" & team_abbreviation == "CSUB" ~"Cal St. Bakersfield",
        team_short_display_name == "CSU Fullerton" & team_name == "Titans" & team_abbreviation == "CSUF" ~"Cal St. Fullerton",
        team_short_display_name == "CSU Northridge" & team_name == "Matadors" & team_abbreviation == "CSUN" ~"Cal St. Northridge",
        team_short_display_name == "NA" & team_name == "Fighting Camels" & team_abbreviation == "CAMP" ~"Campbell",
        team_short_display_name == "Centenary LA" & team_name == "Gentlemen" & team_abbreviation == "CENT" ~"Centenary",
        team_short_display_name == "NA" & team_name == "Bears" & team_abbreviation == "CARK" ~"Central Arkansas",
        team_short_display_name == "C Arkansas" & team_name == "Bears" & team_abbreviation == "CARK" ~"Central Arkansas",
        team_short_display_name == "Cent Conn St" & team_name == "Blue Devils" & team_abbreviation == "CCSU" ~"Central Connecticut",
        team_short_display_name == "C Connecticut" & team_name == "Blue Devils" & team_abbreviation == "CCSU" ~"Central Connecticut",
        team_short_display_name == "Cent Michigan" & team_name == "Chippewas" & team_abbreviation == "CMU" ~"Central Michigan",
        team_short_display_name == "C Michigan" & team_name == "Chippewas" & team_abbreviation == "CMU" ~"Central Michigan",
        team_short_display_name == "Charleston So" & team_name == "Buccaneers" & team_abbreviation == "CHSO" ~"Charleston Southern",
        team_short_display_name == "NA" & team_name == "49ers" & team_abbreviation == "CLT" ~"Charlotte",
        team_short_display_name == "Chicago State" & team_name == "Cougars" & team_abbreviation == "CHIC" ~"Chicago St.",
        team_short_display_name == "Cleveland State" & team_name == "Vikings" & team_abbreviation == "CLEV" ~"Cleveland St.",
        team_short_display_name == "NA" & team_name == "Chanticleers" & team_abbreviation == "CCU" ~"Coastal Carolina",
        team_short_display_name == "Coast Carolina" & team_name == "Chanticleers" & team_abbreviation == "CCU" ~"Coastal Carolina",
        team_short_display_name == "Charleston" & team_name == "Cougars" & team_abbreviation == "COFC" ~"College of Charleston",
        team_short_display_name == "Colorado State" & team_name == "Rams" & team_abbreviation == "CSU" ~"Colorado St.",
        team_short_display_name == "UConn" & team_name == "Huskies" & team_abbreviation == "CONN" ~"Connecticut",
        team_short_display_name == "Coppin State" & team_name == "Eagles" & team_abbreviation == "COPP" ~"Coppin St.",
        team_short_display_name == "Delaware State" & team_name == "Hornets" & team_abbreviation == "DSU" ~"Delaware St.",
        team_short_display_name == "Detroit Mercy" & team_name == "Titans" & team_abbreviation == "DET" ~"Detroit",
        team_short_display_name == "Dixie State" & team_name == "Trailblazers" & team_abbreviation == "DXST" ~"Dixie St.",
        team_short_display_name == "ETSU" & team_name == "Buccaneers" & team_abbreviation == "ETSU" ~"East Tennessee St.",
        team_short_display_name == "E Illinois" & team_name == "Panthers" & team_abbreviation == "EIU" ~"Eastern Illinois",
        team_short_display_name == "E Kentucky" & team_name == "Colonels" & team_abbreviation == "EKU" ~"Eastern Kentucky",
        team_short_display_name == "E Michigan" & team_name == "Eagles" & team_abbreviation == "EMU" ~"Eastern Michigan",
        team_short_display_name == "E Washington" & team_name == "Eagles" & team_abbreviation == "EWU" ~"Eastern Washington",
        team_short_display_name == "Fair. Dickinson" & team_name == "Knights" & team_abbreviation == "FDU" ~"Fairleigh Dickinson",
        team_short_display_name == "Fair Dickinson" & team_name == "Knights" & team_abbreviation == "FDU" ~"Fairleigh Dickinson",
        team_short_display_name == "Florida Int'l" & team_name == "Panthers" & team_abbreviation == "FIU" ~"FIU",
        team_short_display_name == "FAMU" & team_name == "Rattlers" & team_abbreviation == "FAMU" ~"Florida A&M",
        team_short_display_name == "FAU" & team_name == "Owls" & team_abbreviation == "FAU" ~"Florida Atlantic",
        team_short_display_name == "FGCU" & team_name == "Eagles" & team_abbreviation == "FGCU" ~"Florida Gulf Coast",
        team_short_display_name == "Florida State" & team_name == "Seminoles" & team_abbreviation == "FSU" ~"Florida St.",
        team_short_display_name == "Fresno State" & team_name == "Bulldogs" & team_abbreviation == "FRES" ~"Fresno St.",
        team_short_display_name == "Gardner-Webb" & team_name == "Runnin' Bulldogs" & team_abbreviation == "GWEB" ~"Gardner Webb",
        team_short_display_name == "G Washington" & team_name == "Colonials" & team_abbreviation == "GW" ~"George Washington",
        team_short_display_name == "GA Southern" & team_name == "Eagles" & team_abbreviation == "GASO" ~"Georgia Southern",
        team_short_display_name == "Georgia So" & team_name == "Eagles" & team_abbreviation == "GASO" ~"Georgia Southern",
        team_short_display_name == "Georgia State" & team_name == "Panthers" & team_abbreviation == "GAST" ~"Georgia St.",
        team_short_display_name == "Grambling" & team_name == "Tigers" & team_abbreviation == "GRAM" ~"Grambling St.",
        team_short_display_name == "Hawai'i" & team_name == "Rainbow Warriors" & team_abbreviation == "HAW" ~"Hawaii",
        team_short_display_name == "Idaho State" & team_name == "Bengals" & team_abbreviation == "IDST" ~"Idaho St.",
        team_short_display_name == "UIC" & team_name == "Flames" & team_abbreviation == "UIC" ~"Illinois Chicago",
        team_short_display_name == "Illinois State" & team_name == "Redbirds" & team_abbreviation == "ILST" ~"Illinois St.",
        team_short_display_name == "Indiana State" & team_name == "Sycamores" & team_abbreviation == "INST" ~"Indiana St.",
        team_short_display_name == "Iowa State" & team_name == "Cyclones" & team_abbreviation == "ISU" ~"Iowa St.",
        team_short_display_name == "Purdue FW" & team_name == "Mastodons" & team_abbreviation == "PFW" ~"IPFW",
        team_short_display_name == "Jackson State" & team_name == "Tigers" & team_abbreviation == "JKST" ~"Jackson St.",
        team_short_display_name == "Jackson St" & team_name == "Tigers" & team_abbreviation == "JKST" ~"Jackson St.",
        team_short_display_name == "Jacksonville St" & team_name == "Gamecocks" & team_abbreviation == "JVST" ~"Jacksonville St.",
        team_short_display_name == "J'Ville St" & team_name == "Gamecocks" & team_abbreviation == "JVST" ~"Jacksonville St.",
        team_short_display_name == "Kansas State" & team_name == "Wildcats" & team_abbreviation == "KSU" ~"Kansas St.",
        team_short_display_name == "Kennesaw State" & team_name == "Owls" & team_abbreviation == "KENN" ~"Kennesaw St.",
        team_short_display_name == "Kent State" & team_name == "Golden Flashes" & team_abbreviation == "KENT" ~"Kent St.",
        team_short_display_name == "Long Island" & team_name == "Sharks" & team_abbreviation == "LIU" ~"LIU Brooklyn",
        team_short_display_name == "Long Beach St" & team_name == "Beach" & team_abbreviation == "LBSU" ~"Long Beach St.",
        team_short_display_name == "Louisiana" & team_name == "Ragin' Cajuns" & team_abbreviation == "ULL" ~"Louisiana Lafayette",
        team_short_display_name == "UL Monroe" & team_name == "Warhawks" & team_abbreviation == "ULM" ~"Louisiana Monroe",
        team_short_display_name == "Loyola Marymnt" & team_name == "Lions" & team_abbreviation == "LMU" ~"Loyola Marymount",
        team_short_display_name == "LMU" & team_name == "Lions" & team_abbreviation == "LMU" ~"Loyola Marymount",
        team_short_display_name == "Loyola (MD)" & team_name == "Greyhounds" & team_abbreviation == "L-MD" ~"Loyola MD",
        team_short_display_name == "NA" & team_name == "Black Bears" & team_abbreviation == "MAINE" ~"Maine",
        team_short_display_name == "MD-Eastern" & team_name == "Hawks" & team_abbreviation == "UMES" ~"Maryland Eastern Shore",
        team_short_display_name == "UMass" & team_name == "Minutemen" & team_abbreviation == "MASS" ~"Massachusetts",
        team_short_display_name == "McNeese" & team_name == "Cowboys" & team_abbreviation == "MCNS" ~"McNeese St.",
        team_short_display_name == "NA" & team_name == "Cowboys" & team_abbreviation == "MCNS" ~"McNeese St.",
        team_short_display_name == "Miami" & team_name == "Hurricanes" & team_abbreviation == "MIA" ~"Miami FL",
        team_short_display_name == "Miami (OH)" & team_name == "Redhawks" & team_abbreviation == "M-OH" ~"Miami OH",
        team_short_display_name == "Michigan State" & team_name == "Spartans" & team_abbreviation == "MSU" ~"Michigan St.",
        team_short_display_name == "NA" & team_name == "Blue Raiders" & team_abbreviation == "MTSU" ~"Middle Tennessee",
        team_short_display_name == "MTSU" & team_name == "Blue Raiders" & team_abbreviation == "MTSU" ~"Middle Tennessee",
        team_short_display_name == "Mississippi St" & team_name == "Bulldogs" & team_abbreviation == "MSST" ~"Mississippi St.",
        team_short_display_name == "Miss Valley St" & team_name == "Delta Devils" & team_abbreviation == "MVSU" ~"Mississippi Valley St.",
        team_short_display_name == "Ole Miss" & team_name == "Rebels" & team_abbreviation == "MISS" ~"Mississippi",
        team_short_display_name == "Missouri State" & team_name == "Bears" & team_abbreviation == "MOST" ~"Missouri St.",
        team_short_display_name == "Montana State" & team_name == "Bobcats" & team_abbreviation == "MTST" ~"Montana St.",
        team_short_display_name == "Morehead State" & team_name == "Eagles" & team_abbreviation == "MORE" ~"Morehead St.",
        team_short_display_name == "Morgan State" & team_name == "Bears" & team_abbreviation == "MORG" ~"Morgan St.",
        team_short_display_name == "Mt. St. Mary's" & team_name == "Mountaineers" & team_abbreviation == "MSM" ~"Mount St. Mary's",
        team_short_display_name == "Mount St Mary" & team_name == "Mountaineers" & team_abbreviation == "MSM" ~"Mount St. Mary's",
        team_short_display_name == "Murray State" & team_name == "Racers" & team_abbreviation == "MUR" ~"Murray St.",
        team_short_display_name == "Omaha" & team_name == "Mavericks" & team_abbreviation == "OMA" ~"Nebraska Omaha",
        team_short_display_name == "New Mexico St" & team_name == "Aggies" & team_abbreviation == "NMSU" ~"New Mexico St.",
        team_short_display_name == "Nicholls" & team_name == "Colonels" & team_abbreviation == "NICH" ~"Nicholls St.",
        team_short_display_name == "Norfolk State" & team_name == "Spartans" & team_abbreviation == "NORF" ~"Norfolk St.",
        team_short_display_name == "NC A&T" & team_name == "Aggies" & team_abbreviation == "NCAT" ~"North Carolina A&T",
        team_short_display_name == "NC Central" & team_name == "Eagles" & team_abbreviation == "NCCU" ~"North Carolina Central",
        team_short_display_name == "NC State" & team_name == "Wolfpack" & team_abbreviation == "NCST" ~"North Carolina St.",
        team_short_display_name == "North Dakota St" & team_name == "Bison" & team_abbreviation == "NDSU" ~"North Dakota St.",
        team_short_display_name == "N Dakota St" & team_name == "Bison" & team_abbreviation == "NDSU" ~"North Dakota St.",
        team_short_display_name == "N Arizona" & team_name == "Lumberjacks" & team_abbreviation == "NAU" ~"Northern Arizona",
        team_short_display_name == "N Colorado" & team_name == "Bears" & team_abbreviation == "UNCO" ~"Northern Colorado",
        team_short_display_name == "N Illinois" & team_name == "Huskies" & team_abbreviation == "NIU" ~"Northern Illinois",
        team_short_display_name == "N Kentucky" & team_name == "Norse" & team_abbreviation == "NKU" ~"Northern Kentucky",
        team_short_display_name == "Northwestern St" & team_name == "Demons" & team_abbreviation == "NWST" ~"Northwestern St.",
        team_short_display_name == "N'Western St" & team_name == "Demons" & team_abbreviation == "NWST" ~"Northwestern St.",
        team_short_display_name == "NA" & team_name == "Wildcats" & team_abbreviation == "NU" ~"Northwestern",
        team_short_display_name == "Ohio State" & team_name == "Buckeyes" & team_abbreviation == "OSU" ~"Ohio St.",
        team_short_display_name == "Oklahoma State" & team_name == "Cowboys" & team_abbreviation == "OKST" ~"Oklahoma St.",
        team_short_display_name == "Oregon State" & team_name == "Beavers" & team_abbreviation == "ORST" ~"Oregon St.",
        team_short_display_name == "Penn State" & team_name == "Nittany Lions" & team_abbreviation == "PSU" ~"Penn St.",
        team_short_display_name == "Pennsylvania" & team_name == "Quakers" & team_abbreviation == "PENN" ~"Penn",
        team_short_display_name == "Portland State" & team_name == "Vikings" & team_abbreviation == "PRST" ~"Portland St.",
        team_short_display_name == "Prairie View" & team_name == "Panthers" & team_abbreviation == "PV" ~"Prairie View A&M",
        team_short_display_name == "NA" & team_name == "Blue Hose" & team_abbreviation == "PRE" ~"Presbyterian",
        team_short_display_name == "Sacramento St" & team_name == "Hornets" & team_abbreviation == "SAC" ~"Sacramento St.",
        team_short_display_name == "Sam Houston St" & team_name == "Bearkats" & team_abbreviation == "SHSU" ~"Sam Houston St.",
        team_short_display_name == "Sam Houston" & team_name == "Bearkats" & team_abbreviation == "SHSU" ~"Sam Houston St.",
        team_short_display_name == "San Diego State" & team_name == "Aztecs" & team_abbreviation == "SDSU" ~"San Diego St.",
        team_short_display_name == "San José State" & team_name == "Spartans" & team_abbreviation == "SJSU" ~"San Jose St.",
        team_short_display_name == "Savannah State" & team_name == "Tigers" & team_abbreviation == "SAV" ~"Savannah St.",
        team_short_display_name == "Seattle U" & team_name == "Redhawks" & team_abbreviation == "SEA" ~"Seattle",
        team_short_display_name == "SIUE" & team_name == "Cougars" & team_abbreviation == "SIUE" ~"SIU Edwardsville",
        team_short_display_name == "SC State" & team_name == "Bulldogs" & team_abbreviation == "SCST" ~"South Carolina St.",
        team_short_display_name == "South Dakota St" & team_name == "Jackrabbits" & team_abbreviation == "SDST" ~"South Dakota St.",
        team_short_display_name == "S Dakota St" & team_name == "Jackrabbits" & team_abbreviation == "SDST" ~"South Dakota St.",
        team_short_display_name == "SE Missouri St" & team_name == "Redhawks" & team_abbreviation == "SEMO" ~"Southeast Missouri St.",
        team_short_display_name == "SE Louisiana" & team_name == "Lions" & team_abbreviation == "SELA" ~"Southeastern Louisiana",
        team_short_display_name == "So Illinois" & team_name == "Salukis" & team_abbreviation == "SIU" ~"Southern Illinois",
        team_short_display_name == "S Illinois" & team_name == "Salukis" & team_abbreviation == "SIU" ~"Southern Illinois",
        team_short_display_name == "St. Fran. (BKN)" & team_name == "Terriers" & team_abbreviation == "SFBK" ~"St. Francis NY",
        team_short_display_name == "St Francis BK" & team_name == "Terriers" & team_abbreviation == "SFBK" ~"St. Francis NY",
        team_short_display_name == "St. Fran. (PA)" & team_name == "Red Flash" & team_abbreviation == "SFPA" ~"St. Francis PA",
        team_short_display_name == "St Francis PA" & team_name == "Red Flash" & team_abbreviation == "SFPA" ~"St. Francis PA",
        team_short_display_name == "St. Thomas Aquinas" & team_name == "Saints" & team_abbreviation == "STAC" ~"St. Thomas",
        team_short_display_name == "SF Austin" & team_name == "Lumberjacks" & team_abbreviation == "SFA" ~"Stephen F. Austin",
        team_short_display_name == "Tarleton" & team_name == "Texans" & team_abbreviation == "TAR" ~"Tarleton St.",
        team_short_display_name == "UT Martin" & team_name == "Skyhawks" & team_abbreviation == "UTM" ~"Tennessee Martin",
        team_short_display_name == "Tennessee State" & team_name == "Tigers" & team_abbreviation == "TNST" ~"Tennessee St.",
        team_short_display_name == "Texas A&M-CC" & team_name == "Islanders" & team_abbreviation == "AMCC" ~"Texas A&M Corpus Chris",
        team_short_display_name == "Texas State" & team_name == "Bobcats" & team_abbreviation == "TXST" ~"Texas St.",
        team_short_display_name == "UCSB" & team_name == "Gauchos" & team_abbreviation == "UCSB" ~"UC Santa Barbara",
        team_short_display_name == "NA" & team_name == "Roos" & team_abbreviation == "KC" ~"UMKC",
        team_short_display_name == "Kansas City" & team_name == "Roos" & team_abbreviation == "KC" ~"UMKC",
        team_short_display_name == "SC Upstate" & team_name == "Spartans" & team_abbreviation == "SCUP" ~"USC Upstate",
        team_short_display_name == "NA" & team_name == "Vaqueros" & team_abbreviation == "RGV" ~"UT Rio Grande Valley",
        team_short_display_name == "UT Rio Grande" & team_name == "Vaqueros" & team_abbreviation == "RGV" ~"UT Rio Grande Valley",
        team_short_display_name == "Utah State" & team_name == "Aggies" & team_abbreviation == "USU" ~"Utah St.",
        team_short_display_name == "Weber State" & team_name == "Wildcats" & team_abbreviation == "WEB" ~"Weber St.",
        team_short_display_name == "W Carolina" & team_name == "Catamounts" & team_abbreviation == "WCU" ~"Western Carolina",
        team_short_display_name == "W Illinois" & team_name == "Leathernecks" & team_abbreviation == "WIU" ~"Western Illinois",
        team_short_display_name == "Western KY" & team_name == "Hilltoppers" & team_abbreviation == "WKU" ~"Western Kentucky",
        team_short_display_name == "W Michigan" & team_name == "Broncos" & team_abbreviation == "WMU" ~"Western Michigan",
        team_short_display_name == "Wichita State" & team_name == "Shockers" & team_abbreviation == "WICH" ~"Wichita St.",
        team_short_display_name == "Wright State" & team_name == "Raiders" & team_abbreviation == "WRST" ~"Wright St.",
        TRUE ~ team_short_display_name
      )
    )
  df_torvik <- df_torvik %>%
    mutate(team2 = gsub('\"', "", team2)) %>%
    mutate(team1 = gsub('\"', "", team1)) 
  df_torvik_debug <- df_torvik %>%
    filter(team2 == "South Carolina")
  df_espn_debug <- df_espn %>%
    filter(team_short_display_name == "South Carolina")
  #Need to join by game date and team name.
  df <- df_torvik %>%
    left_join(df_espn, by = c("date" = "game_date", "team1" = "team_short_display_name"))
  write_csv(df, paste0("1box_score_betting_", season, ".csv"))

  }

files <- fs::dir_ls(glob = "torvik_box_score*") 
df_torvik <- vroom(files) %>%
  select(team1) %>% 
  distinct() %>%
  arrange(team1)
files <- fs::dir_ls(glob = "espn_line*") 
df_espn <- vroom(files) %>%
  select(team_short_display_name, team_name, team_abbreviation) %>% 
  distinct() %>%
  arrange(team_short_display_name)
write_csv(df_espn, "espn_names.csv")
write_csv(df_torvik, "torvik_names.csv")



for (season in 2013:2022){
 df <- vroom(paste0("espn_line_information_", season, ".csv"), 
             altrep = FALSE)
 write_csv(df, paste0("espn_line_information_csv", season, ".csv"))
}




#Pulling in all team names.
files <- fs::dir_ls(glob = "torvik_box_score*") 
df_names <- vroom(files) %>%
  select(team1, team2)
df_pivot <- df_names %>%
  pivot_longer(everything()) %>%
  select(value) %>%
  count(value) %>%
  arrange(n)
write_csv(df_pivot, "team_names.csv")







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

#Look at year over year distribution. Density plot or violin plot.
#If spread gets bigger, look into autocorrelation.