
scrape_fsquads_epl_rosters <- function(slug) {
  # slug = 'arsenal'
  url <- sprintf('http://www.footballsquads.co.uk/eng/2019-2020/engprem/%s.htm', slug)
  page <- url %>% xml2::read_html()
  
  df_raw <- 
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
    df_raw %>% 
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