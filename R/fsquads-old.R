
# TODO: Coerce season like `2019L` or '2019' to '2019-20' and use in `url`.
.validate_epl_year <- function(year, year_min = 1992L, year_max = as.integer(format(Sys.Date(), '%Y')) - 1L) {
  stopifnot(length(year) == 1L)
  if (is.character(year)) {
    if(str_detect(year, '-')) {
      rgx <- '(^.*)-(.*$)'
      x1 <- str_replace(year, rgx, '\\1')
      x2 <- str_replace(year, rgx, '\\2')
    } else {
      res <- safely(~as.numeric(year))
      if(!is.null(res$error)) {
        stop('`year` could not be coerced to an integer.', call. = FALSE)
      }
      year <- res$result
    }
  } else if(!is.numeric(year)) {
    stop('`year` could not be coerced to an integer..', call. = FALSE)
  }
  if(year < year_min | year > year_max) {
    stop('`year` is not a valid season.', call. = FALSE)
  }
  # TODO: ...
  invisible(year)
}

.generate_epl_season <- function(year) {
  year1 <- .validate_epl_year(year)
  year2 <- year1 + 1L
  sprintf('%04d-%04d', year1, year2)
}

.get_epl_year_current <- function(message = TRUE) {
  res <- as.integer(format(Sys.Date(), '%Y')) - 1L
  if(message) {
    message(sprintf('Using default (%04d) for `year` since none provided', res))
  }
  res
}

# TODO: Convert this to `get_league_meta()`, and make a wrapper for "epl"(?)
.get_epl_teams_meta <- function(year = NULL) {
  if(is.null(year)) {
    year <- .get_epl_year_current(message = TRUE)
  }
  season <- .generate_epl_season(year)
  league <- sprintf('%sprem', ifelse(year < 2018L, 'fa', 'eng'))
  url_suffix <- sprintf('%s/%s.htm', season, league)
  url_prefix <- 'http://www.footballsquads.co.uk/eng/'
  url <- sprintf('%s%s', url_prefix, url_suffix)
  page <- xml2::read_html(url)
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

get_epl_teams_meta <- memoise::memoise(.get_epl_teams_meta)

.get_epl_team_players_1 <- function(url) {
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

get_epl_team_players_1 <- memoise::memoise(.get_epl_team_players_1)

.get_epl_team_players <- function(year = NULL, team = NULL, unnest = TRUE) {
  if(is.null(year)) {
    year <- .get_epl_year_current(message = TRUE)
  }
  season <- .validate_fsquad_year(year)
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

get_epl_team_players <- memoise::memoise(.get_epl_team_players)
