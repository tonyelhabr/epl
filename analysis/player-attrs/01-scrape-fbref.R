
# # Everything checks out.
# robotstxt::get_robotstxt('https://fbref.com)

# TODO: Write a function to retrieve this.
links_fbref <- file.path('data-raw', 'fbref-links-tms.csv') %>% read_csv()
links_fbref

# link <- links_fbref %>% slice(1) %>% pull(link)
# scrape_fbref_tm_page_safely <- possibly(scrape_fbref_tm_page, otherwise = tibble())
# res_safe <- link %>% scrape_fbref_tm_page()
# res_safe

res_1_nested <-
  links_fbref %>% 
  filter(team == 'Tottenham Hotspur') %>% 
  mutate(
    res = map(
      link, 
      ~scrape_fbref_tm_page(link = .x, meta_header = FALSE)
    )
  )
res_1_nested

# 1,'Premier League','Standard Stats'
# 2,'Champions League','Standard Stats'
# 3,'EFL Cup','Standard Stats'
# 4,'All Competitions','Standard Stats'
# 5,'Premier League','Goalkeeping'
# 6,'Champions League','Goalkeeping'
# 7,'EFL Cup','Goalkeeping'
# 8,'All Competitions','Goalkeeping'
# 9,'Premier League','Scores & Fixures'
# 10,'Champions League','Scores & Fixures'
# 11,'FA Cup','Scores & Fixures'
# 12,'EFL Cup','Scores & Fixures'
# 13,'All Competitions','Scores & Fixures'
res_1 <-
  res_1_nested %>% 
  select(team, res) %>% 
  unnest(res) %>% 
  # group_by(.idx) %>% 
  # split(.$res)
  filter(.idx == 14) %>% 
  unnest(res)
res_1

res_nested <-
  links_fbref %>% 
  # crossing(idx = c(1L, 5L)) %>% # PL Standard Stats, PL Goalkeeping
  mutate(idx = 1L) %>% 
  mutate(
    res = map2(
      link, 
      idx, 
      ~scrape_fbref_tm_page(link = .x, idx = .y, meta_header = FALSE)
    )
  )
res_nested
res_nested <- res_nested

# res_nms <-
#   res_nested %>% 
#   select(idx, res) %>% 
#   mutate(nm = map(res, names)) %>% 
#   select(idx, nm) %>% 
#   unnest(nm)
# res_nms
# res_nms %>% count(nm, sort = TRUE)
res <- 
  res_nested %>% 
  select(team, res) %>% 
  unnest(res) %>% 
  # This part is just personal preference for names. (Took this out of the function, which I intend to go into its own package.)
  rename_all(~str_replace_all(., 'per_', 'p') %>% str_remove('_minutes.*') %>% str_remove_all('_')) %>% 
  mutate_at(vars(nation), ~str_remove(., '^[a-z]+\\s+')) %>% 
  select(-matches)
res
# res %>% arrange(-mp_playing_time)
# res %>% count(player, sort = TRUE)
write_csv(res, 'output/players-epl-fbref-2019.csv', na = '')

# eda ----
# res %>% count(tm)
# nat_n <- res %>% mutate_at(vars(nation), ~str_remove(., '^[a-z]+\\s+')) %>% count(tm, nation, sort = TRUE)
 #nat_n %>% filter(nation != 'ENG')
