
players_fbref_raw <- 'output/players-epl-fbref-2019.csv' %>% read_csv()
players_fbref_raw
players_fsquads_raw <- 'output/players-epl-fsquads-2019.csv' %>% read_csv()
players_fsquads_raw

players_fbref <-
  players_fbref_raw %>% 
  mutate_at(vars(pos), ~str_sub(., 1L, 1L)) %>% 
  select(
    team,
    player,
    nat = nation,
    pos,
    age,
    app = mp,
    start = starts,
    mp = min
  )
players_fbref

# NOTE: 'Harry Wilson' and 'Danny Drinkwater' appear on 2 different teams in the `fsquads` data set.
# ('Jack McIntyre' also appears twice, but he doesn't get past all of the data processing.)
players_fsquads %>% count(player, sort = T) %>% filter(n > 1)
players_fsquads <-
  players_fsquads_raw %>% 
  select(-height_m, -weight_kg, -bmi_ptile) %>% 
  rename(player = name)
players_fsquads

nms_fsquads <- players_fsquads %>% names()
nms_fbref <- players_fbref %>% names()
nms_share <- intersect(nms_fsquads, nms_fbref)
nms_share

players_fulljoin <- 
  full_join(
    players_fbref %>% mutate(dummy = player) %>% rename_at(vars(one_of(nms_share)), ~paste0(., '_fbref')),
    players_fsquads %>% mutate(dummy = player) %>% rename_at(vars(one_of(nms_share)), ~paste0(., '_fsquads'))
  ) %>% 
  select(
    player = dummy,
    matches('player'),
    matches('team'),
    matches('pos'),
    matches('age'),
    matches('nat'),
    # everything() # Comment this back in once everything else is figured out.
  )
players_fulljoin

players_nomatch_tidy <-
  players_fulljoin %>% 
  filter(is.na(player_fbref) | is.na(player_fsquads)) %>% 
  # filter_at(vars(matches('players_'))
  select(matches('player_')) %>% 
  gather(key = 'src', value = 'player') %>% 
  mutate_at(vars(src), ~str_remove(., 'player_')) %>% 
  drop_na()
players_nomatch_tidy
players_nomatch_tidy %>% filter(player %>% str_detect('Aaron'))
players_nomatch_tidy %>% inner_join(players_fbref)

# players_nomatch_tidy %>% tidystringdist::tidy_comb_all(player)
players_nomatch_combos <-
  players_nomatch_tidy %>% 
  pull(player) %>% 
  tidystringdist::tidy_comb_all()
players_nomatch_combos
players_nomatch_sdist <- players_nomatch_combos %>% tidystringdist::tidy_stringdist()
players_nomatch_sdist

# Existing mismatches:
# player_fbref,player_fsquad_INCORRECT,player_fsquad_CORRECT
# Jonny Castro,Juan Castillo,Jonny
# Lucas Moura,Lucas Gamblin,Lucas
# Martinelli,Meritan Shabani,Gabriel Martinelli
# Trézéguet,Frédéric Guilbert,Mahmoud Hassan
# Wesley Moraes,Wesley Hoedt,Wesley
players_nomatch_sdist_arr <-
  players_nomatch_sdist %>% 
  rename(player_fbref = V1, player_fsquads = V2) %>% 
  mutate(z = (cosine + jaccard + jw) * (soundex + 1)) %>% 
  group_by(player_fbref) %>% 
  # arrange(cosine, jaccard, jw, soundex) %>% 
  mutate(rnk = row_number(z)) %>% 
  ungroup() %>% 
  arrange(player_fbref, rnk)
      
players_nomatch_sdist_arr
players_nomatch_sdist_arr %>% filter(player_fbref == 'Wesley Moraes')
players_nomatch_sdist_arr %>% filter(player_fbref %>% str_detect('O\'Driscoll')) -> z

players_rematched <-
  players_nomatch_sdist_arr %>% 
  filter(rnk == 1L) %>% 
  inner_join(
    players_nomatch_tidy %>% filter(src == 'fbref') %>% select(player_fbref = player)
  )
players_rematched

players_rematched_redux <-
  players_rematched %>% 
  left_join(
    tribble(
      ~player_fbref, ~player_fsquads_manual,
      'Jonny Castro', 'Jonny',
      'Lucas moura', 'Lucas',
      'Martinelli', 'Gabriel Martinelli',
      'Trézéguet', 'Mahmoud Hassan',
      'Wesley Moraes', 'Wesley'
    )
  ) %>% 
  mutate_at(vars(player_fsquads), ~coalesce(player_fsquads_manual, .))
players_rematched_redux

players_fsquads_redux <-
  players_fsquads %>% 
  full_join(
    players_rematched_redux %>% 
      select(player_impute = player_fbref, player = player_fsquads)
  ) %>% 
  mutate_at(vars(player), ~coalesce(player_impute, .))
players_fsquads_redux 
players_fsquads_redux %>% filter(!is.na(player_impute)) %>% arrange(player)

players_attrs <- 
  players_fbref %>% 
  replace_na(list(mp = 0, app = 0, start = 0)) %>% 
  left_join(
    players_fsquads_redux %>% 
      select(player, number, dob, height_in, weight_lb, bmi)
  )  %>% 
  # drop_na() %>% 
  select(
    player,
    team,
    pos,
    number,
    nat,
    age,
    dob,
    height_in,
    weight_lb,
    bmi,
    mp,
    app,
    start
  ) %>% 
  mutate_if(is.numeric, ~round(., 2)) %>% 
  mutate_at(vars(number, age, bmi, mp, app), as.integer)
players_attrs

# For the duplicates, choose the lowest number (assuming the duplicates are on the same `team`.)
players_attrs_clean <-
  players_attrs %>% 
  group_by(player) %>% 
  filter(number == min(number)) %>% 
  ungroup()
players_attrs_clean

# Data check.
players_attrs_clean %>%
  drop_na() %>% 
  # distinct(weight_lb, height_in, age, dob, .keep_all = TRUE) %>% 
  count(weight_lb, height_in, age, dob, sort = TRUE) %>%
  filter(n > 1L) %>% 
  inner_join(players_attrs) %>% 
  inner_join(players_fbref)

write_csv(players_attrs_clean, 'output/players-epl-2019.csv', na = '')

