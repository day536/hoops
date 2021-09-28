library(tidyverse)

#Get that data boi
curated_player_data <- read_csv("https://raw.githubusercontent.com/day536/hoops/main/curated_data/advanced.csv")
hoop_math_shooting_data <- read_csv("https://raw.githubusercontent.com/day536/hoops/main/curated_data/hoop_math_player_bridge.csv")

#Curate and prep for the app
curated_player_data <- curated_player_data %>% 
  left_join(hoop_math_shooting_data, by = c("player", "school", "season")) %>% 
  mutate(ast_usagepercent = astpercent/usgpercent) %>% 
  filter(fga >= 30) %>% 
  ##mutate_at(across(c("ts_percent","ft_percent"), map_dbl(.x = ., .f ~))
  #mutate(ts_percent = map_dbl(.x = ts_percent, .f = ~replace_na(x = .x, replace = 0))) %>% 
  group_by(pos) %>% 
  mutate_at(vars(fg:bpm, percent_shots_at_rim:ast_usagepercent), funs("percentile" = rank(.)/length(.))) %>% 
  ungroup() %>% 
  mutate(tovpercent_percentile = 1 - tovpercent_percentile)


player_profile_vec <- c("player", "class", "season", "pos", "school")
player_impact_vec <- c("usgpercent", "tspercent", "astpercent", "ast_usagepercent", "tovpercent")
four_factors_vec <- c("efgpercent", "orbpercent", "drbpercent", "blkpercent", "tovpercent")
#shooting_vec <- c("shots_at_rim", "percent_at_rim", "2pt_j", "2pt_jumper", "x3fg")


four_factors_data <- curated_player_data %>% 
  ungroup() %>% 
  select(matches(player_profile_vec) | g, mp, contains(four_factors_vec), -contains("hoop_math")) %>% 
  select(-contains("link"))

player_impact_data <- curated_player_data %>% 
  ungroup() %>% 
  select(matches(player_profile_vec) | g, mp, contains(player_impact_vec), -contains("hoop_math")) %>% 
  select(-contains("link"))


player_shooting_acc_data <- curated_player_data %>% 
  ungroup() %>% 
  select(matches(player_profile_vec) | g, mp, -contains("link"), -contains("hoop_math"),
         efgpercent, efgpercent_percentile,
         #percent_shots_at_rim,
         #percent_shots_at_rim_percentile,
         fg_percent_at_rim,
         fg_percent_at_rim_percentile,
         #percent_shots_2pt_j,
         #percent_shots_2pt_j_percentile,
         fg_percent_2pt_jumpers,
         fg_percent_2pt_jumpers_percentile,
         #percent_of_shots_3pt,
         #percent_of_shots_3pt_percentile,
         x3fg_percent,
         x3fg_percent_percentile)

player_shooting_freq_data <- curated_player_data %>% 
  ungroup() %>% 
  select(matches(player_profile_vec) | g, mp, -contains("link"), -contains("hoop_math"),
         efgpercent, efgpercent_percentile,
         percent_shots_at_rim,
         percent_shots_at_rim_percentile,
         #fg_percent_at_rim,
         #fg_percent_at_rim_percentile,
         percent_shots_2pt_j,
         percent_shots_2pt_j_percentile,
         #fg_percent_2pt_jumpers,
         #fg_percent_2pt_jumpers_percentile,
         percent_of_shots_3pt,
         percent_of_shots_3pt_percentile) #,
         #x3fg_percent,
         #x3fg_percent_percentile)
  

