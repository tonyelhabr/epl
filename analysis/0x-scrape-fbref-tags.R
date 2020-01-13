
library(rvest)
url <- 'https://fbref.com/en/squads/822bd0ba/Liverpool'
page <- url %>% xml2::read_html()
page
# # 1
# page %>% html_nodes(xpath = '//*[@id="all_ks_sched_2900"]/div[1]/div[1]/ul/li[2]/div/ul/li[4]/button')
# x
# # 2
# x <- page %>% html_nodes(xpath = '//*[@id="all_ks_sched_2900"]/div[1]/div[1]/ul/li')
# x
# x %>% html_children() %>% html_text()
# # 3
x <- page %>% html_nodes(xpath = '//*[@id="div_ks_sched_2900"]')
x
x %>% html_children() %>% html_table()
# # 4
# y <- page %>% html_nodes(xpath = '//*[@id="all_stats_keeper_ks_7796"]')
# y
# y %>% html_children() %>% html_table()
# # 5
# a <- page %>% html_nodes(xpath = '//*[@id="all_stats_passing_ks_2900"]')
# a
# a %>% html_children() %>% html_table()
# # 6
# a <- page %>% html_nodes(xpath = '//*[@id="div_stats_passing_ks_2900_clone"]')
# a
# a %>% html_children() %>% html_table()
# # 7
# a <- page %>% html_nodes(xpath = '//*[@id="all_stats_passing_ks_2900"]')
# a
a <- page %>% html_nodes(xpath = '//*[@id="div_kitchen_sink_sched"]')
a %>% 
  html_children() %>%
  html_children() %>% 
  html_children() %>% 
  html_children() %>% # html_nodes('.overthrow table_container')
  # html_children() %>% 
  # html_tag()
  # html_table()
  .[[9]] %>% 
  html_attrs()
a
