
# all team links ----
url_base <- 'https://fbref.com'
html_raw <- url_base %>% xml2::read_html()
html_raw
nodes_script <- html_raw %>% rvest::html_nodes('script')
nodes_script
idx_allowed <- nodes_script %>% rvest::html_attr('class') %>% str_which('allowed')
idx_allowed
script_raw <- nodes_script[idx_allowed[-1]] # %>% pluck(8)
script_raw
text_raw <- script_raw %>% rvest::html_text()
text_raw
engine <- V8::v8()
invisible(engine$eval(text_raw))
teams_json_raw <- engine$get('sr_goto_json["team_json"]')
teams_json <- teams_json_raw %>% map(names)

teams_links_df <-
  teams_json %>% 
  enframe('.idx_league', 'link') %>% 
  unnest(link) %>% 
  filter(link != '') %>% 
  mutate(
    team = link %>% str_replace_all('(^.*\\/)(.*$)', '\\2')
  )
teams_links_df

nodes_body <- html_raw %>% rvest::html_node('body') # %>% rvest::html_nodes('div')
nodes_body
nodes_filt <- nodes_body %>% rvest::html_nodes(xpath = '//*[@name="league_val"]')
text_leagues <- nodes_filt %>% rvest::html_children() %>% rvest::html_text()
idx_leagues <- nodes_filt %>% rvest::html_children() %>% rvest::html_attr('value')

leagues_df <-
  tibble(
    .idx_league = idx_leagues,
    league = text_leagues
  ) %>% 
  filter(.idx_league != '')
leagues_df

teams_leagues_links_df <-
  leagues_df %>% 
  full_join(teams_links_df, by = '.idx_league') %>% 
  select(.idx_league, league, team, link) %>% 
  mutate_at(vars(.idx_league), as.integer) %>% 
  mutate_at(vars(link), ~sprintf('%s%s', url_base, .))
teams_leagues_links_df
# #selector_0 > option:nth-child(2)
# //*[@id="selector_0"]

# one team page ----
tms_fbref_links <-
  tribble(
    ~link, ~tm,
    '/en/squads/18bb7c10/Arsenal', 'Arsenal'
  )
url_tm <- sprintf('%s/%s', url_base, '/en/squads/18bb7c10/Arsenal')
url_tm
html_raw <- url_tm %>% xml2::read_html()
html_raw
nodes_body <- html_raw %>% rvest::html_nodes('body')
nodes_tr <- nodes_body %>% rvest::html_nodes('tr , h2 , .left , #all_kitchen_sink_passing .section_heading , #all_stats_player_summary .section_heading , .right , .center')
nodes_tr %>% rvest::html_nodes('h2')
nodes_tr[length(nodes_tr)]
nodes_tr %>% pluck(length(nodes_tr)-1)



# node_1 <- nodes_tr %>% pluck(3) %>% rvest::html_nodes('td')
 # node_1
tables <- html_raw %>% rvest::html_table()
tables
