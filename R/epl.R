
.colors_ryo <-
  c(
    'Liverpool' = 'red',
    'Tottenham' = '#132257',
    'Manchester City' = '#6CABDD',
    'Chelsea' = 'blue', #'#034694',
    'Arsenal' = 'green', #'#9C824A',
    'Manchester United' = 'orange',
    'Leicester' = '#b15928',
    'Sheffield United' = 'pink',
    'Bournemouth' = 'purple',
    'Brighton' = 'white',
    'Other' = 'grey'
  )

.recreate_epl_colors_filt <- memoise::memoise({
  function(data, ...) {
    
    tms_colors <-
      teamcolors::teamcolors %>%
      as_tibble() %>%
      filter(league == 'epl') %>%
      select(tm = name, color_1 = primary)
    tms_colors
    
    tms_filt <-
      c(
        'Liverpool',
        'Manchester City',
        'Tottenham',
        'Chelsea',
        'Arsenal',
        'Manchester United',
        'Leicester City',
        'Everton',
        'Wolverhampton Wanderers'
      ) %>% 
      tibble(tm = .)
    tms_filt
    
    tms_colors_filt <-
      tms_colors %>% 
      right_join(tms_filt, by = 'tm') %>% 
      # Need to manually add these. (Really should just make my own list of colors (and logos?).
      mutate_at(
        vars(color_1),
        ~case_when(
          tm == 'Tottenham' ~ '#132257', # blue, from https://teamcolorcodes.com/?s=tottenham
          tm == 'Wolverhampton Wanderers' ~ '#FDB913', # yellow, from https://teamcolorcodes.com/?s=wolverhampton
          TRUE ~ .
        )
      )
    res <- tms_colors_filt %>% deframe()
    res
  }
})

.colors_epl_filt <- .recreate_epl_colors_filt()
.width_section_label <- 30
scale_color_epl_filt <- function(..., colors = .colors_epl_filt, width = .width_section_label) {
  scale_color_manual(values = colors, labels = function(x) str_wrap(x, width = width), ...)
}
