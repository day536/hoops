library(tidyverse)

#Get that data boi
curated_player_data <- read_csv("https://raw.githubusercontent.com/day536/hoops/main/curated_data/advanced.csv")

#Curate and prep for the app
curated_player_data <- curated_player_data %>% 
  mutate(ast_usagepercent = astpercent/usgpercent) %>% 
  group_by(pos) %>% 
  mutate_at(vars(fg:bpm, ast_usagepercent), funs("percentile" = rank(.)/length(.))) %>% 
  ungroup()


player_profile_vec <- c("player", "class", "season", "pos", "school")
player_impact_vec <- c("usgpercent", "tspercent", "astpercent", "ast_usagepercent", "tovpercent")
four_factors_vec <- c("efgpercent", "orbpercent", "drbpercent", "blkpercent", "tovpercent")


four_factors_data <- curated_player_data %>% 
  ungroup() %>% 
  select(matches(player_profile_vec) | g, mp, contains(four_factors_vec)) %>% 
  select(-contains("link"))

player_impact_data <- curated_player_data %>% 
  ungroup() %>% 
  select(matches(player_profile_vec) | g, mp, contains(player_impact_vec)) %>% 
  select(-contains("link"))


