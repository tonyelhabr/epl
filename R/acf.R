
.plot_acf <- function(data, value, ...) {
  value_quo <- rlang::enquo(value)
  # stopifnot(length(value) != 1)
  keys <- rlang::enquos(...)
  # keys_chr <- purrr::map_chr(keys, rlang::as_string)
  # stopifnot(length(keys) >= 1, length(keys) <= 2)
  
  data_acf <-
    data %>%
    group_by(!!!keys) %>%
    # For some reason `!!value` does not work in `acf()`, so just rename it before.
    # rename(value = !!value_quo) %>% 
    # summarise(list_acf = list(acf(value, plot = FALSE))) %>%
    summarize(list_acf = list(acf(!!value_quo, plot = FALSE))) %>%
    mutate(acf_vals = map(list_acf, ~ as.numeric(.x$acf))) %>%
    select(-list_acf) %>%
    unnest(acf_vals) %>%
    group_by(!!!keys) %>%
    mutate(lag = row_number() - 1) %>% 
    ungroup()
  
  data_ci <-
    data %>%
    group_by(!!!keys) %>%
    summarize(ci = qnorm((1 + 0.95) / 2) / sqrt(n())) %>% 
    ungroup()
  
  viz <-
    data_acf %>%
    ggplot(aes(x = lag, y = acf_vals)) +
    # Note that `0.05` will make some columns seem to disappear.
    geom_col(width = .2) +
    geom_hline(yintercept = 0) +
    geom_hline(
      data = data_ci,
      aes(yintercept = -ci),
      color = 'blue',
      size = 1,
      linetype = 'dotted'
    ) +
    # Don't use 'global' labels here since this function is supposed
    # to be 'general purpose'.
    geom_hline(
      data = data_ci,
      aes(yintercept = ci),
      color = 'blue',
      size = 1,
      linetype = 'dotted'
    ) +
    labs(x = 'Lag', y = 'ACF')
  
  if(length(keys) == 1L) {
    viz <-
      viz +
      facet_wrap(vars(!!!keys))
  } else {
    # `facet_grid()` is weird... It ends up plotting everything in one column.
    viz <-
      viz +
      facet_wrap(vars(!!!keys))
  }
  viz
}

# visualize_acf_byproduct <-
#   function(...) {
#     viz <- .plot_acf(...)
#     viz <-
#       viz %>%
#       .gg_constants_custom() +
#       .labs_acf()
#     viz
#   }