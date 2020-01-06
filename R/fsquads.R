
.get_leagues_meta_1 <- function(url) {
  # year <- 2019L
  # year <- 2018L
  
  page <- url %>% xml2::read_html()
  slugs <- page %>% rvest::html_nodes(xpath = '//*[@id="main"]/table/tr/td/a') %>% rvest::html_attr('href')
  # Or
  # slugs <- page %>% rvest::html_nodes('#main > table > tr > td > a') %>% rvest::html_attr('href')
  # countries <- slugs %>% str_subset('(^.*)\\/([1-2].*$)', '\\1')
  # seasons <- slugs %>% str_subset('(^.*)\\', '\\2')
  rgx <- '(^.*)\\/([0-9-]+)\\/(.*)[.]htm$'
  url_prefix <- url %>% str_replace('k\\/.*$', 'k/')
  res <-
    tibble(slug = slugs) %>% 
    mutate_at(
      vars(slug),
      list(
        country = ~str_replace_all(., rgx, '\\1'),
        season = ~str_replace_all(., rgx, '\\2'),
        league = ~str_replace_all(., rgx, '\\3'),
        url = ~sprintf('%s%s', url_prefix, .)
      )
    ) %>%
    mutate_at(vars(season), list(year = ~str_sub(., 1L, 4L) %>% as.integer())) %>% 
    select(-slug)
  res
}

get_leagues_meta_1 <- memoise::memoise(.get_leagues_meta_1)

.get_leagues_meta <- function() {
  urls <- 
    c(
      current = 'http://www.footballsquads.co.uk/squads.htm', 
      archive = 'http://www.footballsquads.co.uk/archive.htm'
    )
  
  res_prelim <-
    urls %>% 
    tibble(url = .) %>% 
    mutate(
      res = map(url, get_leagues_meta_1)
    ) %>% 
    select(-url) %>% 
    # Or
    # select(res) %>% 
    unnest(res) %>% 
    # select(country, league, year, season, url) %>% 
    select(league, year, url) %>% 
    arrange(league, year)
  
  res <-
    res_prelim %>% 
    # Hard-coding fixes. (The `url` should not be changed.)
    mutate_at(
      vars(league),
      ~case_when(
        . == 'faprem' & year <= 2017L ~ 'engprem',
        TRUE ~ .
      )
    )
  res
}

get_leagues_meta <- memoise::memoise(.get_leagues_meta)

.pull_distinctly <- function(.data, var = -1, ..., decreasing = FALSE)  {
  var <- tidyselect::vars_pull(names(.data), !!rlang::enquo(var))
  sort(unique(.data[[var]]), decreasing = decreasing, ...)
}

.stop_for_league <- function(leagues_meta, message_prefix = '') {
  leagues <- leagues_meta %>% .pull_distinctly(.data$league)
  leagues_chr <- leagues %>% paste0(sep = '', collapse = '\n')
  stop(
    glue::glue(
      '{message_prefix}Try one of the following: 
      {leagues_chr}'
    ),
    call. = FALSE
  )
}

.stop_for_year <- function(leagues_meta, message_prefix = '') {
  years <- leagues_meta_filt_1 %>% .pull_distinctly(.data$year)
  years_chr <- years %>% paste0(sep = '', collapse = '\n')
  stop(
    glue::glue(
      '{message_prefix}Try one of the following: 
      {years_chr}'
    ),
    call. = FALSE
  )
}

.validate_league <- function(leagues_meta, league) {
  res <-
    leagues_meta %>% 
    filter(.data$league == league)
  
  if(nrow(res) == 0L) {
    # stop(sprintf('Invalid `league` (%s)', league), call. = FALSE)
    .stop_for_league(leagues_meta, sprintf('`league` (%s) is invalid. ', league))
  }
  res
}

# NOTE: This could become unneeded depending on how/if `.validate_league()` and `.validate_year()` are implemented.
.validate_league_year <- function(leagues_meta, league, year) {
  leagues_meta_filt_1 <-
    leagues_meta %>% 
    filter(.data$league == league)
  
  if(nrow(leagues_meta_filt_1) == 0L) {
    # stop(sprintf('Invalid `league` (%s)', league), call. = FALSE)
    .stop_for_league(leagues_meta, sprintf('`league` (%s) is invalid. ', league))
  }
  
  leagues_meta_filt_2 <-
    leagues_meta_filt_1 %>% 
    filter(.data$year == year)

  if(nrow(leagues_meta_filt_2) == 0L) {
    .stop_for_year(leagues_meta_filt_1, sprintf('Invalid `year` (%s) (given `league` = (%s)).', year, league), call. = FALSE)
  }
  
  leagues_meta_filt_2
}

.get_league_teams_meta <- function(league, year) {
  leagues_meta <- get_leagues_meta()
  if(missing(league)) {
    .stop_for_league(leagues_meta, '`league` must be specified. ')
  }
  leagues_meta_filt_1 <- leagues_meta %>% .validate_league(league)
  if(missing(league)) {
    .stop_for_league(leagues_meta, '`league` must be specified. ')
  }
  
  leagues_meta_filt <- leagues_meta %>% .validate_league_year(year = year, league = league)
  url <- leagues_meta_filt %>% pull(url)
  page <- url %>% xml2::read_html()
  nodes_teams <- 
    page %>% 
    rvest::html_node('body') %>% 
    rvest::html_nodes('h5') %>% 
    rvest::html_children()
  teams <- nodes_teams %>% rvest::html_text()
  slugs <- 
    nodes_tms %>% 
    rvest::html_attr('href')
  links <-
    sprintf('%s%s/%s', url_prefix, season, slugs)
  res <-
    tibble(
      team = teams,
      link = links
    )
  res
}

get_league_teams_meta <- memoise::memoise(.get_league_teams_meta)

.get_team_players_1 <- function(url) {
  # url <- 'http://www.footballsquads.co.uk/eng/2019-2020/engprem/arsenals.htm'
  # browser()
  page <- url %>% xml2::read_html()
  
  df <- 
    page  %>% 
    rvest::html_table(header = TRUE) %>% 
    pluck(1) %>% 
    janitor::clean_names() %>% 
    as_tibble() %>% 
    filter(name != '') %>% 
    mutate_all(~ifelse(. == '', NA, .))
  
  # get_bmi_ptile_safely <- safely(PAutilities::get_BMI_percentile, otherwise = tibble())
  # .get_bmi_ptile <- PAutilities::get_BMI_percentile
  
  res <-
    df %>% 
    filter(
      !(name %in% c('Players no longer at this club', 'Name'))
    ) %>% 
    mutate_at(vars(number, weight), as.integer) %>% 
    mutate_at(vars(height), as.double) %>% 
    mutate_at(vars(date_of_birth), lubridate::dmy) %>% 
    select(
      number,
      name,
      nat,
      pos,
      dob = date_of_birth,
      height_m = height,
      weight_kg = weight
    ) %>% 
    mutate(
      bmi = weight_kg / (height_m ^ 2),
      height_in = height_m * 39.3701, # %>% measurements::conv_unit('m', 'inch'),
      weight_lb = weight_kg * 2.2046, # %>% measurements::conv_unit('kg', 'lbs'),
      age_dur = lubridate::ymd(Sys.Date()) - dob
    ) %>% 
    mutate_at(
      vars(age_dur),
      list(
        # age_mon = ~lubridate::time_length(., unit = 'months') %>% floor(),
        age_yr = ~lubridate::time_length(., unit = 'years') %>% floor()
      )
    ) %>% 
    # rename_at(vars(matches('^age_dur_')), ~str_remove(., '_dur')) %>% 
    select(-age_dur) %>% 
    mutate(
      bmi_ptile = suppressWarnings(PAutilities::get_BMI_percentile(
        weight_kg = weight_kg, 
        height_cm = height_m * 100, 
        sex = 'M', 
        age_yrs = age_yr
      ))
    ) %>% 
    select(
      number,
      name,
      nat,
      pos,
      dob,
      age = age_yr,
      height_m,
      height_in,
      weight_kg,
      weight_lb,
      bmi,
      bmi_ptile
    )
  res
}

get_team_players_1 <- memoise::memoise(.get_team_players_1)

.get_team_players <- function(league, year team, unnest = TRUE) {

  
  
  
  f_safe <- safely(get_epl_meta)
  res <- f_safe(year = year)
  if(!is.null(res$error)) {
    stop('Something went wrong.', call. = FALSE)
  }
  meta <- res$result
  
  if(!is.null(team)) {
    stopifnot(is.character(team))
    teams <- meta %>% pull(.data$team)
    # is_valid_team_1 <- any(team %in% teams)
    team_union <- intersect(team, teams)
    is_valid_team <- length(team_union) == length(team)
    # TODO: make an informative error message for this.
    # if(!is_valid_team) {
    #   team_union_diff <- length(team_union) - length(team)
    #   # ...
    # }
    stopifnot(is_valid_team)
    meta <- meta %>% filter(.data$team == team)
  }
  
  # TODO: Need to do a `safely()` here?
  f_safe <- possibly(get_epl_team_players_1, otherwise = tibble())
  res_nested <-
    meta %>% 
    mutate(
      data = map(url, f_safe)
    ) %>%
    select(team, data)
  
  if(!unnest) {
    return(res_nested)
  }
  res <-
    res_nested %>% 
    unnest(data)
  res
}

get_team_players <- memoise::memoise(.get_team_players)
