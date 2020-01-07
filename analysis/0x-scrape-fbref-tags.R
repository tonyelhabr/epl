
library(rvest)
url <- 'https://fbref.com/en/squads/822bd0ba/Liverpool'
page <- url %>% xml2::read_html()
page
x <- page %>% html_nodes(xpath = '//*[@id="all_ks_sched_2900"]/div[1]/div[1]/ul/li[2]/div/ul/li[4]/button')
x
x <- page %>% html_nodes(xpath = '//*[@id="all_ks_sched_2900"]/div[1]/div[1]/ul/li')
x
x %>% html_children() %>% html_text()
x <- page %>% html_nodes(xpath = '//*[@id="div_ks_sched_2900"]')
x
x %>% html_children() %>% html_table()
y <- page %>% html_nodes(xpath = '//*[@id="all_stats_keeper_ks_7796"]')
y
