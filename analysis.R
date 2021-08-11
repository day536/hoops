library(tidyverse)

free_agents_curated <- free_agents_2021 %>% mutate(year = 2021) %>% 
  bind_rows(free_agents_2020 %>% mutate(year = 2020)) %>% 
  bind_rows(free_agents_2019 %>% mutate(year = 2019)) %>% 
  bind_rows(free_agents_2018 %>% mutate(year = 2018)) %>% 
  bind_rows(free_agents_2017 %>% mutate(year = 2017)) %>% 
  bind_rows(free_agents_2016 %>% mutate(year = 2016)) %>% 
  ##mutate(py_stats = str_c(x2020_21_stats, x2019_20_stats)) Eventually get py stats
  filter(nchar(terms) > 0, nchar(n_tm) > 0) %>% 
  select(-x2020_21_stats, -x2019_20_stats, -x2018_19_stats, -x2017_18_stats, -x2016_17_stats, -x2015_16_stats) %>% 
  filter(str_detect(string = terms, "^Signed")) %>% 
  mutate(deal_amt = substr(terms, 6, 10))
  mutate(deal_lgl = map(.x = terms, .f = ~str_detect(string = .x, "M")))
  mutate(test = str_extract(terms, boun))

stringr::

  free_agents_curated %>% 
    sample_n(1) %>% 
    select(terms)
