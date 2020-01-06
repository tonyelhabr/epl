
gms_single <-
  engsoccerdata::england %>% 
  janitor::clean_names() %>% 
  filter(season >= 1995, tier == 1) %>% 
  select(
    season,
    date,
    tm_home = home,
    tm_away = visitor,
    g_h = hgoal,
    g_a = vgoal,
    result
  ) %>% 
  arrange(date, tm_home) %>% 
  mutate_at(
    vars(season, matches('^g_')),
    as.integer
  )
gms_single

gms_stack <-
  bind_rows(
    gms_single %>% mutate(tm = tm_home),
    gms_single %>% mutate(tm = tm_away)
  ) %>% 
  filter(tm == tm_home | tm == tm_away) %>% 
  group_by(season, tm) %>% 
  mutate(wk = row_number(date)) %>% 
  ungroup() %>% 
  select(
    season,
    wk,
    tm,
    date,
    tm_home,
    tm_away,
    g_h,
    g_a,
    result
  ) %>% 
  arrange(season, tm, wk)
gms_stack

gms_clean <-
  gms_stack %>% 
  mutate(
    h_a = if_else(tm == tm_home, 'h', 'a'),
    g = if_else(tm == tm_home, g_h, g_a),
    ga = if_else(tm == tm_home, g_a, g_h)
  ) %>% 
  mutate(
    w = if_else(g > ga, 1L, 0L),
    l = if_else(ga > g, 1L, 0L),
    d = if_else(g == ga, 1L, 0L)
  ) %>% 
  mutate(
    pts = 3L * w + 0L * d + 1L * d
  ) %>% 
  select(
    season,
    wk,
    tm,
    g,
    ga,
    w,
    l,
    d,
    pts,
    h_a
  )
gms_clean

standings <-
  gms_clean %>% 
  group_by(season, tm) %>% 
  arrange(wk, .by_group = TRUE) %>% 
  mutate_at(vars(g, ga, w, l, d, pts), cumsum) %>% 
  ungroup() %>% 
  group_by(season, wk) %>% 
  mutate(
    rnk = dense_rank(desc(pts))
  ) %>% 
  ungroup() %>% 
  arrange(season, tm, wk)
standings

standings_calcs <-
  standings %>% 
  group_by(season, tm) %>% 
  arrange(wk, .by_group = TRUE) %>% 
  mutate(rnk_diff = rnk - dplyr::lag(rnk)) %>% 
  mutate(rnk_diff_wt = ((20L - rnk) / 20L) * rnk_diff) %>% 
  mutate_at(
    vars(matches('^rnk.*diff')),
    list(abs = abs)
  ) %>% 
  ungroup() %>% 
  filter(wk > 1L)
standings_calcs

standings_summ_by_tm <-
  standings_calcs %>% 
  group_by(season, tm) %>% 
  summarize_at(vars(matches('^rnk_')), list(sum)) %>% 
  ungroup()
standings_summ_by_tm %>% arrange(desc(rnk_diff_abs))

standings_summ_by_wk <-
  standings_calcs %>% 
  group_by(season, wk) %>% 
  summarize_at(vars(matches('^rnk_')), list(sum)) %>% 
  ungroup()
standings_summ_by_wk

# # DEbugging...
# standings_calcs %>% 
#   count(wk) %>% 
#   arrange(desc(wk)) %>% 
#   filter(n < 440L)

standings_summ <-
  standings_calcs %>% 
  group_by(season) %>% 
  summarize_at(vars(matches('^rnk_')), list(sum)) %>% 
  ungroup() %>% 
  mutate_at(vars(matches('^rnk_')), list(rnk = ~dense_rank(desc(.)))) %>% 
  arrange(desc(rnk_diff_wt_abs))
standings_summ

standings_summ_by_wk %>% 
  group_by(season) %>% 
  arrange(wk, .by_group = TRUE) %>% 
  mutate_at(vars(matches('^rnk_')), cumsum) %>% 
  ungroup() %>%
  mutate_at(vars(season), as.factor) %>% 
  ggplot() +
  aes(x = wk, y = rnk_diff_abs, group = season, color = season) +
  geom_step() +
  fix_color_legend() +
  theme_custom() +
  add_lab_caption()




