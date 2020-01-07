
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
  url_prefix <- url %>% stringr::str_replace('k\\/.*$', 'k/')
  res <-
    tibble::tibble(slug = slugs) %>% 
    dplyr::mutate_at(
      vars(slug),
      list(
        country = ~stringr::str_replace_all(., rgx, '\\1'),
        season = ~stringr::str_replace_all(., rgx, '\\2'),
        league = ~stringr::str_replace_all(., rgx, '\\3'),
        url = ~sprintf('%s%s', url_prefix, .)
      )
    ) %>%
    dplyr::mutate_at(dplyr::vars(season), list(year = ~stringr::str_sub(., 1L, 4L) %>% as.integer())) %>% 
    dplyr::select(-slug)
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
    tibble::tibble(url = .) %>% 
    dplyr::mutate(
      res = purrr::map(url, get_leagues_meta_1)
    ) %>% 
    dplyr::select(-url) %>% 
    # Or
    # select(res) %>% 
    tidyr::unnest(res) %>% 
    # select(country, league, year, season, url) %>% 
    dplyr::select(league, year, url) %>% 
    dplyr::arrange(league, year)
  
  # # Notably, 'engprem' was 'faprem' prior to `2018`.
  # faprem <-
  #   res_prelim %>% 
  #   dplyr::filter(league == 'faprem')
  # 
  # engprem <-
  #   res_prelim %>% 
  #   dplyr::filter(league == 'engprem')
  # 
  # # ... more to duplicate these leagues.
  # 
  # res <-
  #   res_prelim %>% 
  #   # Hard-coding fixes. (The `url` should not be changed.)
  #   dplyr::mutate_at(
  #     dplyr::vars(league),
  #     ~dplyr::case_when(
  #       . == 'faprem' & year <= 2017L ~ 'engprem',
  #       TRUE ~ .
  #     )
  #   )
  res <- res_prelim
  res
}

get_leagues_meta <- memoise::memoise(.get_leagues_meta)

.pull_distinctly <- function(.data, var = -1, ..., decreasing = FALSE)  {
  var <- tidyselect::vars_pull(names(.data), !!rlang::enquo(var))
  sort(unique(.data[[var]]), decreasing = decreasing, ...)
}

.stop_for_league <- function(leagues_meta, msg_prefix = '') {
  leagues <- leagues_meta %>% .pull_distinctly(league)
  leagues_chr <- leagues %>% paste0(sep = '', collapse = ', ')
  stop(
    glue::glue(
      '{msg_prefix}Try one of the following: 
      {leagues_chr}'
    ),
    call. = FALSE
  )
}

.stop_for_year <- function(leagues_meta, msg_prefix = '') {
  years <- leagues_meta %>% .pull_distinctly(year)
  years_chr <- years %>% paste0(sep = '', collapse = ', ')
  stop(
    glue::glue(
      '{msg_prefix}Try one of the following: 
      {years_chr}'
    ),
    call. = FALSE
  )
}

.stop_for_team <- function(league_teams_meta, msg_prefix = '') {
  teams <- league_teams_meta %>% .pull_distinctly(team)
  teams_chr <- teams %>% paste0(sep = '', collapse = ', ')
  stop(
    glue::glue(
      '{msg_prefix}Try one of the following: 
      {teams_chr}'
    ),
    call. = FALSE
  )
}

.filter_league <- function(leagues_meta, league) {
  stopifnot(is.character(league))
  stopifnot(length(league) == 1L)
  res <-
    leagues_meta %>% 
    dplyr::filter(league == !!league)
  
  if(nrow(res) == 0L) {
    # stop(sprintf('Invalid `league` (%s)', league), call. = FALSE)
    .stop_for_league(leagues_meta, sprintf('`league` (%s) is invalid. ', league))
  }
  res
}

.filter_year <- function(leagues_meta, year) {
  stopifnot(is.numeric(year)) # Really, it should be an integer, but we can let this pass.
  stopifnot(length(year) == 1L)
  
  res <-
    leagues_meta %>% 
    dplyr::filter(year == !!year)
  
  if(nrow(res) == 0L) {
    # Don't think there should ever be more than 1 `league`, given the way the rest of the functions are designed.
    leagues <- leagues_meta %>% .pull_distinctly(league)
    leagues_chr <- leagues %>% paste0(sep = '', collapse = '\n')
    n_leagues <- length(leagues) == 1L
    if(n_leagues) {
      leagues_chr <- sprintf('given `league = %s`', leagues)
    } else if(n_leagues > 1L) {
      leagues_chr <- sprintf('multiple (%s) `league`s', n_leagues)
    } else {
      leagues_chr <- 'no `league`'
    }
    msg_suffix <- sprintf(' (%s).', leagues_chr)
    .stop_for_year(leagues_meta, sprintf('`year` (%s) is invalid %s', year, msg_suffix))
  }
  res
}


.filter_team <- function(league_teams_meta, team) {
  stopifnot(is.character(team))
  stopifnot(length(team) == 1L)
  res <-
    league_teams_meta %>% 
    dplyr::filter(team == !!team)
  
  if(nrow(res) == 0L) {
    # Don't think there should ever be more than 1 `league`, given the way the rest of the functions are designed.
    leagues <- league_teams_meta %>% .pull_distinctly(league)
    leagues_chr <- leagues %>% paste0(sep = '', collapse = '\n')
    n_leagues <- length(leagues) == 1L
    if(n_leagues) {
      leagues_chr <- sprintf('given `league = %s`', leagues)
    } else if(n_leagues > 1L) {
      leagues_chr <- sprintf('multiple (%s) `league`s', n_leagues)
    } else {
      leagues_chr <- 'no `league`.'
    }
    
    # Don't think there should ever be more than 1 `year`, given the way the rest of the functions are designed.
    years <- league_teams_meta %>% .pull_distinctly(year)
    years_chr <- years %>% paste0(sep = '', collapse = '\n')
    n_years <- length(years) == 1L
    if(n_years) {
      years_chr <- sprintf('`year = %s`', years)
    } else if(n_years > 1L) {
      years_chr <- sprintf('multiple (%s) years', n_years)
    } else {
      years_chr <- 'no `year`'
    }
    
    msg_suffix <- sprintf(' (%s and %s).', leagues_chr, years_chr)
    .stop_for_team(league_teams_meta, sprintf('`team` (%s) is invalid%s. ', team, msg_suffix))
  }
  res
}

.get_league_teams_meta_1 <- function(url) {
  page <- url %>% xml2::read_html()
  nodes_teams <- 
    page %>% 
    rvest::html_node('body') %>% 
    rvest::html_nodes('h5') %>% 
    rvest::html_children()
  teams <- nodes_teams %>% rvest::html_text()
  slugs <- 
    nodes_teams %>% 
    rvest::html_attr('href')
  url_prefix <- url %>% str_replace('(^.*[0-9])\\/?.*htm$', '\\1')
  urls <-
    sprintf('%s/%s', url_prefix, slugs)
  res <-
    tibble::tibble(
      team = teams,
      url = urls
    )
}

get_league_teams_meta_1 <- memoise::memoise(.get_league_teams_meta_1)

.get_league_teams_meta <- function(league, year) {
  
  leagues_meta <- get_leagues_meta()
  
  if(missing(league)) {
    .stop_for_league(leagues_meta, '`league` must be specified. ')
  }
  
  if(missing(year)) {
    .stop_for_year(leagues_meta, '`year` must be specified. ')
  }
  
  leagues_meta_filt_1 <- leagues_meta %>% .filter_league(league)
  leagues_meta_filt_2 <- leagues_meta_filt_1 %>% .filter_year(year  = year)
  stopifnot(nrow(leagues_meta_filt_2) == 1L) # Probably don't even need to check for this (given that the prior checks are passed).
  
  # url <- leagues_meta_filt_2 %>% pull(url)
  # res <- url %>% get_league_teams_meta_1()
  res <-
    leagues_meta_filt_2 %>% 
    dplyr::mutate(data = purrr::map(url, get_league_teams_meta_1)) %>% 
    dplyr::select(-url) %>% 
    tidyr::unnest(data)
  res
}

get_league_teams_meta <- memoise::memoise(.get_league_teams_meta)

.get_team_players_1 <- function(url) {
  page <- url %>% xml2::read_html()
  
  df <- 
    page  %>% 
    rvest::html_table(header = TRUE) %>% 
    purrr::pluck(1) %>% 
    janitor::clean_names() %>% 
    tibble::as_tibble() %>% 
    dplyr::filter(name != '') %>% 
    dplyr::mutate_all(~ifelse(. == '', NA, .))
  
  res <-
    df %>% 
    dplyr::filter(
      !(name %in% c('Players no longer at this club', 'Name'))
    ) %>% 
    dplyr::mutate_at(dplyr::vars(number, weight), as.integer) %>% 
    dplyr::mutate_at(dplyr::vars(height), as.double) %>% 
    dplyr::mutate_at(dplyr::vars(date_of_birth), lubridate::dmy)
  res
}

get_team_players_1 <- memoise::memoise(.get_team_players_1)

.get_team_players <- function(league, year, team) {

  if(missing(team)) {
    .stop_for_league(leagues_meta, '`team` must be specified. ')
  }
  
  league_teams_meta <- get_league_teams_meta(league = league, year = year)
  
  league_teams_meta_filt <- league_teams_meta %>% .filter_team(team)
  stopifnot(nrow(league_teams_meta_filt) == 1L) # Probably don't even need to check for this (given that the prior checks are passed).
  
  # url <- league_teams_meta_filt %>% dplyr::pull(url)
  # res <- url %>% get_team_players_1()
  res <-
    league_teams_meta_filt %>% 
    dplyr::mutate(data = purrr::map(url, get_team_players_1)) %>% 
    dplyr::select(-url) %>% 
    tidyr::unnest(data)
  res
  res
}

get_team_players <- memoise::memoise(.get_team_players)
