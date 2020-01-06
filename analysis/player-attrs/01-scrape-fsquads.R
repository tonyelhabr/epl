
epl_teams_meta <- get_fsquads_epl_teams_meta()
epl_teams_meta

epl_team_players <- get_fsquads_epl_team_players(year = 2019L, team = 'Liverpool')
epl_team_players

url_index <- 'http://www.footballsquads.co.uk/eng/2019-2020/engprem.htm'
html_index <- url_index %>% xml2::read_html()
html_index
nodes_tms <- 
  html_index %>% 
  rvest::html_node('body') %>% 
  rvest::html_nodes('h5') %>% 
  rvest::html_children()
nodes_tms

links_tms <- nodes_tms %>% rvest::html_attr('href')
links_tms
tms <- nodes_tms %>% rvest::html_text()
tms

df_links <-
  tibble(
    team = nodes_tms %>% rvest::html_text(),
    slug = nodes_tms %>% rvest::html_attr('href') %>% str_remove_all('engprem/|\\.htm'),
  )
df_links

df_nested <-
  df_links %>% 
  # filter(slug == 'arsenal') %>% 
  mutate(
    data = map(slug, scrape_fsquads_epl_rosters)
  )
df_nested

df <- 
  df_nested %>% 
  unnest(data)
df
# df %>% filter(name == 'Name')
# df %>% filter(name %in% c('Players no longer at this club', 'Name'))
write_csv(df, 'output/players-epl-fsquads-2019.csv', na = '')

# eda ----
df %>% 
  ggplot() +
  aes(x = height_in) +
  geom_histogram()

df %>% 
  ggplot() +
  aes(x = weight_lb) +
  geom_histogram()

df %>% 
  ggplot() +
  aes(x = height_in, y = weight_lb) +
  geom_point()

df %>% 
  ggplot() +
  aes(x = bmi) +
  geom_histogram()

df %>% 
  ggplot() +
  aes(x = bmi_ptile) +
  geom_histogram()

df_filt <- df %>% filter(!is.na(height_m))
df_filt
df_active <- df_filt %>% filter(number < 40L)
df_active
df_active %>% count(slug)
df_active %>% filter(name %>% str_detect('Traor'))
df_active %>% arrange(-bmi)
df_active %>% arrange(-height_in)
df_active %>% arrange(-weight_lb)
nats_n <- df_active %>% count(slug, nat, sort = TRUE)
nats_n
df_active
nats_n %>% tidytext::bind_tf_idf(nat, slug, n) %>% arrange(desc(tf_idf))
nats_n %>% tidytext::bind_tf_idf(slug, nat, n) %>% arrange(desc(tf_idf))

nats_noeng_n <- df_active %>% count(slug, nat, sort = TRUE) %>% filter(nat != 'ENG')
nats_noeng_n
df_active %>% filter(slug == 'leicester', nat == 'WAL')
df_active %>% filter(slug == 'tottenha') %>% count(nat, sort = TRUE)
