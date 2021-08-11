library(httr)
library(tidyverse)
library(rvest)
library(janitor)

bbr_season_stats_etl <- function(url) {
  url %>% 
    read_html(., as.data.frame=T, stringsAsFactors = TRUE) %>% 
    html_nodes("table") %>% 
    .[[1]] %>% 
    html_table(fill=T) %>% 
    clean_names() %>% 
    filter(rk != "Rk")
}

bbr_free_agent_etl <- function(url) {
  url %>% 
    read_html(., as.data.frame=T, stringsAsFactors = TRUE) %>% 
    html_nodes("table") %>% 
    .[[1]] %>% 
    html_table(fill=T) %>% 
    clean_names() %>% 
    filter(rk != "Rk")
}

season_2021 <- bbr_season_stats_etl("https://www.basketball-reference.com/leagues/NBA_2021_advanced.html")
season_2020 <- bbr_season_stats_etl("https://www.basketball-reference.com/leagues/NBA_2020_advanced.html")
season_2019 <- bbr_season_stats_etl("https://www.basketball-reference.com/leagues/NBA_2019_advanced.html")
season_2018 <- bbr_season_stats_etl("https://www.basketball-reference.com/leagues/NBA_2018_advanced.html")
season_2017 <- bbr_season_stats_etl("https://www.basketball-reference.com/leagues/NBA_2017_advanced.html")
season_2016 <- bbr_season_stats_etl("https://www.basketball-reference.com/leagues/NBA_2016_advanced.html")


free_agents_2021 <- bbr_season_stats_etl("https://www.basketball-reference.com/friv/free_agents.cgi?year=2021")
free_agents_2020 <- bbr_season_stats_etl("https://www.basketball-reference.com/friv/free_agents.cgi?year=2020")
free_agents_2019 <- bbr_season_stats_etl("https://www.basketball-reference.com/friv/free_agents.cgi?year=2019")
free_agents_2018 <- bbr_season_stats_etl("https://www.basketball-reference.com/friv/free_agents.cgi?year=2018")
free_agents_2017 <- bbr_season_stats_etl("https://www.basketball-reference.com/friv/free_agents.cgi?year=2017")
free_agents_2016 <- bbr_season_stats_etl("https://www.basketball-reference.com/friv/free_agents.cgi?year=2016")

contracts_2021 <- "https://www.basketball-reference.com/contracts/players.html" %>% 
  read_html(., as.data.frame=T, stringsAsFactors = TRUE) %>% 
  html_nodes("table") %>% 
  .[[1]] %>% 
  html_table(fill=T) %>% 
  row_to_names(1, remove_row = TRUE) %>% 
  clean_names() %>% 
  filter(`x2020_21` != "Salary") %>% 
  filter(rk != "Rk")

