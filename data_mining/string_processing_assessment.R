library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[7]] %>% html_table(fill = TRUE)

polls<- polls %>% select("Date(s) conducted","Remain","Leave","Undecided","Lead","Sample","Conducted by","Polling type","Notes")%>% 
  setNames(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes"))

polls <- polls %>% filter(str_detect(polls$remain,"%$"))

polls <- polls %>% mutate(remain = as.numeric(str_replace(polls$remain,"%",""))/100)

new_undecided <- polls$undecided %>% str_replace("N/A","0")

polls <- polls %>% mutate(undecided=new_undecided)

temp <- str_extract_all(polls$dates,"\\d+\\s[a-zA-Z]+")
end_date <- sapply(temp,function(x) x[length(x)])