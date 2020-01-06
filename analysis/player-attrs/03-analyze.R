
players_attrs <- 'output/players-epl-2019.csv' %>% read_csv()
players_attrs

# players_attrs %>% filter(is.na(min))
# players_summ <-
#   players_attrs %>% 
#   rename(x = min) %>% 
#   summarize_at(
#     vars(x),
#     list(
#       median = ~median(., na.rm  = TRUE),
#       mean = ~mean(., na.rm  = TRUE),
#       min = ~min(., na.rm  = TRUE),
#       max = ~max(., na.rm  = TRUE),
#       q05 = ~quantile(., 0.05, na.rm  = TRUE),
#       q95 = ~quantile(., 0.95, na.rm  = TRUE)
#     )
#   )
# players_summ
# 
# players_attrs %>% 
#   ggplot() +
#   aes(x = min) +
#   geom_histogram()

players_attrs %>% arrange(-bmi)
players_attrs %>% arrange(-height_in)
players_attrs %>% arrange(-weight_lb)

n_by_grp <- players_attrs %>% count(team, nat, sort = TRUE)
n_by_grp

n_by_grp %>% filter(nat != 'ENG')
players_attrs %>% filter(team == 'Norwich City') %>% filter(nat == 'GER')
players_attrs %>% filter(is.na(height_in))

n_tfidfby_nat_team <- 
  n_by_grp %>% 
  tidytext::bind_tf_idf(nat, team, n) %>% 
  arrange(desc(tf_idf))
n_tfidfby_nat_team

n_tfidfby_team_nat <- 
  n_by_grp %>% 
  tidytext::bind_tf_idf(team, nat, n) %>% 
  arrange(desc(tf_idf))
n_tfidfby_team_nat
n_tfidfby_team_nat %>% filter(n > 1L)

sums_by_grp <-
  players_attrs %>% 
  group_by(nat, team) %>% 
  summarize_at(vars(mp, app, start), sum, na.rm = TRUE) %>% 
  ungroup()
sums_by_grp

mp_tfidfby_nat_team <- 
  sums_by_grp %>% 
  tidytext::bind_tf_idf(nat, team, mp) %>% 
  arrange(desc(tf_idf))
mp_tfidfby_nat_team

mp_tfidfby_team_nat <- 
  sums_by_grp %>% 
  tidytext::bind_tf_idf(team, nat, mp) %>% 
  arrange(desc(tf_idf))
mp_tfidfby_team_nat

fit_bmi <-
  players_attrs %>% 
  drop_na() %>% 
  distinct() %>% 
  lm(weight_lb ~ height_in + age, data = .)
fit_bmi

resids_bmi <-
  fit_bmi %>% 
  broom::augment() %>% 
  inner_join(players_attrs) %>% 
  # mutate(rnk = row_number(desc(.cooksd))) %>% 
  filter(weight_lb > 150) %>% 
  mutate(rnk = row_number(desc(abs(.resid)))) %>% 
  select(rnk, everything()) %>% 
  arrange(rnk)
resids_bmi
resids_bmi %>% filter(player %>% str_detect('Traor'))

resids_bmi %>% 
  select(rnk, one_of(names(players_attrs)), .resid, .cooksd) %>% 
  # filter(.resid < 0)
  arrange(-mp)
