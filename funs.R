library(tidyverse)

get_ncaa_season_stats <- function(season, school_id, stat_type){
  
  ncaa_api_base <- "https://www.sports-reference.com/cbb/play-index/psl_finder.cgi"
 
  if (stat_type == "advanced") {
    order_by_stat <- "ts_pct"
  } else if (stat_type == "totals") {
    order_by_stat <- "fg_pct"
  }
  
  #test <- "https://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=2021&year_max=2021&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_f=Y&pos_is_c=Y&school_id=ohio-state&games_type=A&c1stat=fga&c1comp=gt&c1val=1&c2stat=mp&c2comp=gt&c2val=50&c3comp=gt&c4comp=gt&order_by=fg_pct"
  ncaa_url <- paste0(ncaa_api_base,
                     "?request=1&match=single&year_min=",
                     season,
                     "&year_max=",
                     season,
                     "&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y",
                     "&pos_is_g=Y&pos_is_f=Y&pos_is_c=Y",
                     "&school_id=",
                     school_id,
                     "&games_type=A",
                     "&c1stat=fga&c1comp=gt&c1val=1&c2stat=mp&c2comp=gt&c2val=50&c3comp=gt&c4comp=gt",
                     "&order_by=",
                     order_by_stat)
  
  pg <- xml2::read_html(ncaa_url)
  
  ncaa_stats <- rvest::html_table(pg, fill = T)[[1]]
  
  ncaa_stats <- ncaa_stats %>% 
    janitor::row_to_names(1)
  
  if (utils::packageVersion("janitor") > "0.3.1") {
    ncaa_stats <- ncaa_stats %>%
      janitor::clean_names(case = "old_janitor") %>%
      dplyr::filter(.data$player != "Player")
  } else {
    ncaa_stats <- ncaa_stats %>%
      janitor::clean_names() %>%
      janitor::remove_empty_cols() %>%
      dplyr::filter(.data$player != "Player")
  }
  
  links <- pg %>%
    rvest::html_nodes("tr") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  
  link_names <- pg %>%
    rvest::html_nodes("tr") %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  
  links_df <- dplyr::tibble(link_header = as.character(link_names),
                            link   = as.character(links))
  links_df[] <- lapply(links_df, as.character) 
  player_links_df <- links_df %>% 
    filter(grepl("cbb/players/", link)) %>% 
    rename(player = link_header, player_link = link) %>% 
    distinct()
  
  school_links_df <-  links_df %>% 
    filter(grepl("cbb/schools/", link)) %>% 
    rename(school = link_header, school_link = link) %>% 
    distinct()
  
  conf_links_df <-  links_df %>% 
    filter(grepl("cbb/conferences/", link)) %>% 
    rename(conf = link_header, conf_link = link) %>% 
    distinct()
  
  season_links_df <-  links_df %>% 
    filter(grepl("cbb/seasons/", link)) %>% 
    rename(season = link_header, season_link = link) %>% 
    distinct()
  
  
  
  ncaa_stats <- ncaa_stats %>% 
    left_join(player_links_df, by = "player") %>% 
    left_join(school_links_df, by = "school") %>% 
    left_join(conf_links_df, by = "conf") %>% 
    left_join(season_links_df, by = "season")
  ncaa_stats <- dplyr::mutate_at(ncaa_stats,
                                 dplyr::vars(-.data$player, -.data$pos, -.data$school,-.data$conf,
                                             -.data$class, -.data$season,
                                             -.data$player_link, -.data$school_link, 
                                             -.data$conf_link,-.data$season_link),
                                 as.numeric) %>% 
    select(-rk)
  
  return(ncaa_stats)
}
