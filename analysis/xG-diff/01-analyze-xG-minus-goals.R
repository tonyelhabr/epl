
.dir_proj <- here::here()
file_path <- partial(file.path, fsep = '/', ... = )
file_path_out <- partial(file_path, .dir_proj, 'output', ... = )
path_data <- file_path_out('lg_tm_stats.csv')
path_exists <- fs::file_exists(path_data)

if(!path_exists) {
  lgs_meta <- understatr::get_leagues_meta()
  lgs_meta
  lgs_meta %>% count(league_name, sort = T)
  lgs <- lgs_meta %>% distinct(lg = league_name)
  lgs
  
  # lgs %>% group_by(lg) %>% group_modify(~understatr::get_league_teams_stats(.x))
  lg_tm_stats_nested <-
    lgs %>% 
    crossing(yr = 2016L:2018L) %>% 
    mutate(data = map2(lg, yr, understatr::get_league_teams_stats))
  lg_tm_stats_nested
  
  lg_tm_stats <- 
    lg_tm_stats_nested %>% 
    # select(-lg) %>%
    unnest(data) %>% 
    # mutate(wk = wins + loses + draws) %>% 
    select(
      lg,
      # lg = league_name,
      yr,
      # yr = year,
      date,
      tm = team_name,
      # team_id, 
      g = scored,
      ga = missed,
      xg = xG,
      xga = xGA,
      npxg = npxG,
      npxga = npxGA,
      npxgd = npxGD,
      xpts = xpts,
      h_a,
      w = wins, 
      l = loses, 
      d = draws, 
      result,
      pts
    ) %>% 
    arrange(lg, yr, date, tm) %>% 
    group_by(lg, yr, tm) %>% 
    mutate(
      wk = cumsum(w + l + d)
    ) %>% 
    ungroup() %>% 
    select(lg, yr, wk, everything())
  lg_tm_stats
  
  teproj::export_path(lg_tm_stats, dir = 'output')

} else {
  lg_tm_stats <- teproj::import_path(path_data)
}


# Experimenting.. keep it simple for now
lg_tm_stats_filt <-
  lg_tm_stats %>% 
  # filter(yr == 2018L) %>% 
  filter(lg == 'EPL')

# Purely for checking the data (to see that the end-of-ssason standings are correct).
lg_tm_summ <-
  lg_tm_stats_filt %>% 
  group_by(lg, yr, tm) %>% 
  summarise_if(is.numeric, sum) %>% 
  ungroup() %>% 
  group_by(lg, yr) %>% 
  mutate(rnk = row_number(-pts)) %>% 
  ungroup() %>% 
  arrange(lg, yr, -pts)
lg_tm_summ

lg_tm_stats_aug <-
  lg_tm_stats_filt %>% 
  arrange(yr, tm, wk) %>% 
  mutate(
    xgd = g - xg,
    xgad = ga - xga
  ) %>% 
  group_by(yr, tm) %>% 
  mutate_at(
    vars(matches('^xga?d$')),
    list(diff1 = ~{. - dplyr::lag(., 1L)}, cusum = cumsum)
  ) %>% 
  ungroup() %>% 
  mutate(grp = sprintf('%s (%s), %s', tm, lg, yr))
lg_tm_stats_aug

lg_tm_stats_aug_filt <- lg_tm_stats_aug_viz %>% filter(tm %in% names(tms_colors_filt))
tms_colors_filt <- .recreate_epl_colors_filt()
viz_xgd_cusum <-
  lg_tm_stats_aug %>% 
  ggplot() +
  aes(x = wk, y = xgd_cusum, group = grp) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(size = 1, color = 'grey90') +
  geom_line(
    data = lg_tm_stats_aug_filt,
    inherit.aes = TRUE,
    size = 1.25,
    aes(color = tm)
  ) +
  scale_color_epl_filt() +
  fix_color_legend() +
  theme_custom() +
  labs(
    title = 'Running Sum of Difference in Actuals Goals and xG',
    y = 'Actual Goals - xG',
    x = 'Game Week'
  ) +
  add_lab_caption() +
  add_lab_subtitle()
viz_xgd_cusum
# plotly::ggplotly(viz_xgd_cusum)
# library(highcharter)
# hc <-
#   highchart() %>% 
#   # hc_xAxis(categories = lg_tm_stats_aug_viz %>% distinct(wk) %>% pull(wk)) %>% 
#   hc_add_series(data = lg_tm_stats_aug_viz %>% filter(yr == 2018L, tm == 'Liverpool'), hcaes(x = wk, y = xgd_cusum))
# hc
# hchart(lg_tm_stats_aug_viz %>% filter(yr == 2018L), hcaes(x = wk, y = xgd_cusum, group = tm), type = 'line') 
# 
# lg_tm_stats_aug %>% 
#   # filter(tm == 'Liverpool') %>% 
#   .plot_acf(xgad, grp)

acfs <-
  lg_tm_stats_aug %>% 
  group_by(grp) %>% 
  summarize_at(
    vars(matches('^xga?d$')),
    list(res_acf = ~list(acf(., nrow(.), plot = FALSE)))
  ) %>%
  #mutate(val_acf = map(res_acf, ~ as.numeric(.x$acf))) %>%
  mutate_at(
    vars(matches('_res_acf$')),
    list(val_acf = ~map(., ~as.numeric(.x$acf)))
  ) %>% 
  rename_at(
    vars(matches('_val_acf$')),
    ~str_remove(., '_res_acf')
  ) %>% 
  select(-matches('_res_acf$')) %>%
  unnest(matches('_val_acf$')) %>%
  group_by(grp) %>%
  mutate(lag = row_number() - 1L) %>% 
  ungroup()
acfs

acf_cors_nested <-
  acfs %>% 
  # filter(grp == 'Arsenal (EPL), 2016') %>% 
  group_by(grp) %>% 
  nest() %>% 
  mutate(
    cors = map(data, ~corrr::correlate(.x, quiet = TRUE))
  ) %>% 
  select(-data) %>% 
  ungroup()
acf_cors_nested

acf_cors <-
  acf_cors_nested %>% 
  unnest(cors) %>% 
  rename(col_1 = rowname) %>% 
  gather(key = 'col_2', value = 'value', -grp, -col_1) %>% 
  filter(col_1 != col_2) %>% 
  filter(col_1 == 'lag') %>% 
  spread(col_2, value) %>% 
  select(-col_1)
acf_cors

acf_cors_long <-
  acf_cors %>% 
  gather(key = 'key', value = 'value', -grp) %>% 
  arrange(key, value) %>% 
  group_by(key) %>%
  mutate(idx = row_number(value)) %>% 
  ungroup()
acf_cors_long

acf_cors_long_top <-
  acf_cors_long %>% 
  group_by(key) %>% 
  filter(idx <= 5L)
acf_cors_long_top

acf_cors_long %>% 
  ggplot() +
  aes(x = idx, y = value, color = key, group = key) +
  geom_point() +
  geom_line() +
  theme_custom()


viz_xgd_diff1 <-
  lg_tm_stats_aug %>% 
  filter(!is.na(xgd_diff1)) %>% 
  ggplot() +
  aes(x = wk, y = xgd_diff1, group = grp) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(size = 1, color = 'grey90') +
  # geom_point(
  ggalt::geom_lollipop(
    data = lg_tm_stats_aug %>% inner_join(acf_cors_long_top),
    inherit.aes = TRUE,
    size = 1,
    aes(color = tm)
  ) +
  # scale_color_epl_filt() +
  fix_color_legend() +
  theme_custom() +
  labs(
    title = 'First-Order Differences in Actuals Goals and xG',
    y = 'Diff. of Actual Goals - xG (by Game Week)',
    x = 'Game Week'
  ) +
  # facet_wrap(~yr) +
  add_lab_caption() +
  add_lab_subtitle()
viz_xgd_diff1

# Eh, this would need more work to gain meaningful insight...

