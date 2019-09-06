library(dslabs)
library(tidyverse)
library(lubridate)
library(dslabs)
data("brexit_polls")

brexit_polls %>% filter(month(ymd(startdate))==4) %>% nrow

brexit_polls %>% filter(round_date(enddate,unit="week")==ymd("2016-06-12"))

brexit_polls %>% mutate(weekday_end = weekdays(enddate)) %>% group_by(weekday_end) %>% summarize(num=n())

data(movielens)

movielens %>% mutate(year=year(as_datetime(timestamp,origin=ymd("1970-01-01")))) %>% group_by(year) %>% count(year) %>% arrange(desc(n))

movielens %>% mutate(hour=hour(as_datetime(timestamp,origin=ymd("1970-01-01")))) %>% group_by(hour) %>% count(hour) %>% arrange(desc(n))



library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits=3)

sum(str_detect(gutenberg_metadata$title,"Pride and Prejudice"),na.rm=TRUE)

p_and_p <- gutenberg_metadata %>% filter(str_detect(gutenberg_metadata$title,"Pride and Prejudice")) 

gutenberg_works(title=="Pride and Prejudice",languages = "en")

book<-gutenberg_download(c(1342))

words<- book %>% unnest_tokens(word,text)

words <- words %>% filter(!word %in% stop_words$word)

words <- words %>% filter(str_detect(word,"\\d+",negate=TRUE))

words %>% count(word) %>% filter(n>100)
words %>% count(word) %>%arrange(desc(n)) %>% top_n(1)

library(textdata)

afinn <- get_sentiments("afinn")

afinn_sentiments <- words %>% inner_join(afinn)

sum(afinn_sentiments$value>=0)/nrow(afinn_sentiments)

sum(afinn_sentiments$value==4)