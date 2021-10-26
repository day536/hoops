library(tidyverse)


#Get that data boi
curated_player_data <- read_csv("https://raw.githubusercontent.com/day536/hoops/main/curated_data/advanced.csv")
hoop_math_shooting_data <- read_csv("https://raw.githubusercontent.com/day536/hoops/main/curated_data/hoop_math_player_bridge.csv")
advanced_positional_clustering <- read_csv("https://raw.githubusercontent.com/day536/hoops/main/curated_data/advanced_positional_clustering.csv")

#Curate and prep for the app
curated_player_data <- curated_player_data %>% 
  left_join(hoop_math_shooting_data, by = c("player", "school", "season")) %>% 
  left_join(advanced_positional_clustering, by = "player_link") %>% 
  mutate(netrtg = ortg - drtg,
         ast_usagepercent = astpercent/usgpercent,
         fga_at_rim = round(percent_shots_at_rim * hm_fag, 0)/g,
         fga_2pt_j = round(percent_shots_2pt_j * hm_fag, 0)/g,
         fga_3pt = round(percent_of_shots_3pt * hm_fag, 0)/g,
         tspercent = tspercent*200) %>% 
  mutate(across(g:fga_3pt, ~replace_na(.x, 0))) %>% 
  filter(fga >= 30) %>% 
  ##mutate_at(across(c("ts_percent","ft_percent"), map_dbl(.x = ., .f ~))
  #mutate(ts_percent = map_dbl(.x = ts_percent, .f = ~replace_na(x = .x, replace = 0))) %>% 
  group_by(advanced_position_group) %>% 
  mutate_at(vars(fg:bpm, percent_shots_at_rim:ft_percent, netrtg:fga_3pt), funs("percentile" = rank(.)/length(.))) %>% 
  ungroup() %>% 
  mutate(tovpercent_percentile = 1 - tovpercent_percentile,
         percent_assisted_at_rim_percentile = 1 - percent_assisted_at_rim_percentile,
         percent_assisted_2pt_j_percentile = 1 - percent_assisted_2pt_j_percentile,
         percent_assisted_3s_percentile = 1 - percent_assisted_3s_percentile) %>% 
  mutate(x3fg_percent_percentile = case_when(
                                      fga_3pt < 0.5 ~ 0,
                                      TRUE ~ x3fg_percent_percentile),
         percent_assisted_3s_percentile = case_when(
                                      fga_3pt < 0.5 ~ 0,
                                      TRUE ~ percent_assisted_3s_percentile))


player_profile_vec <- c("player", "class", "season", "advanced_position_group", "school")
player_impact_vec <- c("usgpercent", "tspercent", "astpercent", "ast_usagepercent", "tovpercent")
four_factors_vec <- c("netrtg", "efgpercent", "orbpercent", "drbpercent", "blkpercent", "tovpercent")#, "fta_fga", "ortg", "drtg")
four_factors_off_vec <- c("ortg", "efgpercent", "orbpercent", "tovpercent", "fta_fga")#, "fta_fga", "ortg", "drtg")

#shooting_vec <- c("shots_at_rim", "percent_at_rim", "2pt_j", "2pt_jumper", "x3fg")


four_factors_data <- curated_player_data %>% 
  ungroup() %>% 
  select(matches(player_profile_vec) | g, mp, contains(four_factors_vec), -contains("hoop_math")) %>% 
  select(-contains("link"))

four_factors_off_data <- curated_player_data %>% 
  ungroup() %>% 
  select(matches(player_profile_vec) | g, mp, bpm, bpm_percentile,
         contains(four_factors_off_vec), -contains("hoop_math")) %>% 
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
         fga_at_rim,
         fg_percent_at_rim,
         fg_percent_at_rim_percentile,
         #percent_shots_2pt_j,
         #percent_shots_2pt_j_percentile,
         fga_2pt_j,
         fg_percent_2pt_jumpers,
         fg_percent_2pt_jumpers_percentile,
         #percent_of_shots_3pt,
         #percent_of_shots_3pt_percentile,
         fga_3pt,
         x3fg_percent,
         x3fg_percent_percentile,
         ftpercent,
         ftpercent_percentile)

player_shooting_freq_data <- curated_player_data %>% 
  ungroup() %>% 
  select(matches(player_profile_vec) | g, mp, -contains("link"), -contains("hoop_math"),
         efgpercent, efgpercent_percentile,
         fga_at_rim,
         percent_shots_at_rim,
         percent_shots_at_rim_percentile,
         #fg_percent_at_rim,
         #fg_percent_at_rim_percentile,
         fga_2pt_j,
         percent_shots_2pt_j,
         percent_shots_2pt_j_percentile,
         #fg_percent_2pt_jumpers,
         #fg_percent_2pt_jumpers_percentile,
         fga_3pt,
         percent_of_shots_3pt,
         percent_of_shots_3pt_percentile) #,
         #x3fg_percent,
         #x3fg_percent_percentile)
  
player_shooting_assisted_data <- curated_player_data %>% 
  ungroup() %>% 
  select(matches(player_profile_vec) | g, mp, -contains("link"), -contains("hoop_math"),
         efgpercent, efgpercent_percentile,
         #percent_shots_at_rim,
         #percent_shots_at_rim_percentile,
         fga_at_rim,
         percent_assisted_at_rim,
         percent_assisted_at_rim_percentile,
         #percent_shots_2pt_j,
         #percent_shots_2pt_j_percentile,
         fga_2pt_j,
         percent_assisted_2pt_j,
         percent_assisted_2pt_j_percentile,
         #percent_of_shots_3pt,
         #percent_of_shots_3pt_percentile,
         fga_3pt,
         percent_assisted_3s,
         percent_assisted_3s_percentile)

