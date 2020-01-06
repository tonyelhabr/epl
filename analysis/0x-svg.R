
library(tidyverse)

# # attempt #1 ----
# url_base <- 'https://teamcolors.jim-nielsen.com/'
# html_raw <- url_base %>% xml2::read_html()
# html_raw
# nodes_img <- html_raw %>% rvest::html_nodes('h3')
# nodes_img
# 
# # attempt #2 ----
# nodes <- html_raw %>% rvest::html_nodes('script')
# nodes
# children <- nodes %>% rvest::html_children()
# children
# nodes %>% rvest::html_attr('src')

# attempt #3 ----
# # This works, but maybe isn't optimal.
# url <- 'https://teamcolors.jim-nielsen.com/img/epl/afc-bournemouth.svg'
# # Note that the [`{teamcolors}` R package](https://github.com/jimniels/teamcolors) does not have logo URLs for EPL teams. (It's the only league without logos in the package!). But there seems to be a good reason for that; [its source for logos](https://github.com/beanumber/teamcolors/blob/master/data-raw/99_logos.R)---<sportslogos.net>---does not have them! Alternatively, we can retrieve the SVGs from the [main source of the `{teamcolor}` package's color data[(https://github.com/beanumber/teamcolors/blob/master/data-raw/01_bigfour.R)---<https://teamcolors.jim-nielsen.com/>. However, as the author of the site notes, some of the logos are "approximations", including the EPL ones.
# # [Micheal Lopez (https://twitter.com/statsbylopez) "manually" created a CSV ](https://statsbylopez.netlify.com/post/nfl-team-logos-using-ggimage/) with Wikipedia image URLs (see https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv) which might be the next best approach.
# path_res <- 'C:/users/aelhabr/Downloads/afc-bournemouth.svg'
# download.file(url, destfile = path_res, quiet = TRUE, mode = 'wb')
# # resp <- httr::GET(url)
# # resp
# # res <- httr::content(resp, type = 'image/svg+xml')
# # res
# 
# library(ggimage)
# d <-
#   1L:10L %>% tibble(
#     x = .,
#     y = .,
#     image = path_res
#   ) %>% 
#   mutate(
#     size = 0.1 * x
#   )
# d
# 
# d %>% 
#   ggplot() +
#   aes(x = x, y = y) +
#   geom_image(aes(image = image, size = I(size)), by = 'height')

# Generalizing...
tms_svg <-
  c(
    'afc-bournemouth',
    'arsenal',
    'brighton-&-hove-albion',
    'burnley',
    'chelsea',
    'crystal-palace',
    'everton',
    'huddersfield-town',
    'leicester-city',
    'liverpool',
    'machester-city',
    'manchester-united',
    'necaste-united',
    'southampton',
    'stoke-city',
    'swansea-city',
    'tottenham-hotspur',
    'watford',
    'west-bromwich-albion',
    'west-ham-united'
  )

paths_svg <-
  tms_svg %>% 
  tibble(tm = .) %>% 
  mutate_at(
    vars(tm),
    list(
      url = ~sprintf('https://teamcolors.jim-nielsen.com/img/epl/%s', .),
      path_local = ~here::here('data-raw', .)
    )
  )
paths_svg
