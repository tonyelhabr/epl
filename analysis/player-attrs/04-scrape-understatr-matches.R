
leagues_meta <- understatr::get_leagues_meta()
leagues_meta
.years <- 2019L
league_teams_stats_nested <- 
  crossing(
    league_name = 'EPL',
    year = .years
  ) %>% 
  mutate(
    res = map2(league_name, year, understatr::get_league_teams_stats)
  )
league_teams_stats_nested

league_teams_stats <-
  league_teams_stats_nested %>% 
  select(res) %>% 
  unnest(res)
league_teams_stats

teams <- league_teams_stats %>% distinct(league_name, year, team_name)
teams

# teams_meta_nested <-
#   teams %>% 
#   mutate(
#     res = map(team_name, understatr::get_team_meta)
#   )
# teams_meta_nested
# 
# teams_meta <-
#   teams_meta_nested %>% 
#   select(-team_name) %>% 
#   rename(.year = year) %>% 
#   unnest(res) %>% 
#   filter(.year == year) %>% 
#   select(-.year)
# teams_meta

team_players_stats_nested <-
  teams %>% 
  mutate(
    res = map2(team_name, year, understatr::get_team_players_stats)
  )
team_players_stats_nested

team_players_stats <-
  team_players_stats_nested %>% 
  select(-team_name, -year) %>% 
  unnest(res)
team_players_stats

get_player_matches_stats_slowly <- function(..., sleep = 5L, .pb) {
  res <- understatr::get_player_matches_stats(...)
  Sys.sleep(sleep)
  if (!is.null(.pb)) {
    .pb$tick()
  }
  res
}

n_players <- team_players_stats %>% nrow()
n_players
pb <-
  progress::progress_bar$new(
    total = n_players,
    format = '[:bar] :percent eta :eta\n',
    width = 80L
  )

get_player_matches_stats_slowly_safely <- safely(get_player_matches_stats_slowly, otherwise = tibble())

player_matches_stats_nested <-
  team_players_stats %>% 
  select(league_name, player_id, player_name) %>% 
  mutate(
    res = map(player_id, ~get_player_matches_stats_slowly_safely(.x, .pb = pb))
  )
player_matches_stats_nested

player_matches_stats <-
  player_matches_stats_nested %>% 
  select(-player_name, -player_id) %>% 
  unnest(res) %>% 
  unnest(res)
player_matches_stats

write_csv(player_matches_stats, 'output/players-epl-2019-understat.csv', na = '')
