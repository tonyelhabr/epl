
theme_custom <-
  function(...,
           base_family = 'Arial',
           base_size = 12,
           plot.caption = element_text(hjust = 1)) {
    teplot::theme_te(
      base_family = base_family,
      base_size = 12,
      plot.caption = plot.caption,
      ...
    )
  }

# # This is already done in `teplot::theme_te()`.
# hrbrthemes::update_geom_font_defaults(family = 'Arial', size = 4.5)

# labs_xy_null <- function(...) {
#   labs(
#     ...,
#     x = NULL,
#     y = NULL
#   )
# }

fix_color_legend <- function(..., size = 3) {
  guides(color = guide_legend(override.aes = list(size = size)))
}

.lab_subtitle <- 'Seasons 2016-17 - 2018-19'
add_lab_subtitle <- function(..., lab = .lab_subtitle) {
  labs(
    subtitle = lab,
    ...
  )
}

# Ryo: "Data: understat.com\nTwitter: @R_by_Ryo" (sometimes just puts Twitter handle instead of pre-fixing with "Twitter: ")
# presidual: "Positive Residual (@presidual) | Data: NBA.com"
.lab_caption <- '@TonyElHabr | Data: understat.com'
add_lab_caption <- function(..., lab = .lab_caption) {
  labs(
    caption = lab,
    ...
  )
}


