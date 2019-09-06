library(rvest)

url<-"https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h<-read_html(url)
class(h)
h

tab <- h %>% html_nodes("table")
tab <- tab[[2]]

tab <- tab %>% html_table
class(tab)