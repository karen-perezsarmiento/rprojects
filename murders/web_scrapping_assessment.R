library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

nodes <- html_nodes(h,"table")

tab1 <- html_table(nodes[[2]])
tab2 <- html_table(nodes[[3]])
tab3 <- html_table(nodes[[4]])
tab4 <- html_table(nodes[[5]])

tab_1 <- html_table(nodes[[10]])%>%select(X2,X3,X4)%>%slice(2:n())%>%setNames(c("Team","Payroll","Average"))
tab_2 <- html_table(nodes[[19]])%>%slice(2:n())%>%setNames(c("Team","Payroll","Average"))

tab_1%>%full_join(tab_2,by="Team")

library(rvest)
library(tidyverse)
url_wiki <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
wiki_page<-read_html(url_wiki)

tab <- wiki_page %>% html_nodes("table")