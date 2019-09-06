library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

#In general, we can extract data directly from Twitter using the \emph{rtweet} package.
#However, in this case, a group has already compiled data for us and made it available at this url http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'

#data loaded in dslabs package

library(dslabs)
data("trump_tweets")

head(trump_tweets)

names(trump_tweets)

#the help file includes details of each of the variables
#the tweets themselves are in the text variable

#we can use extract to remove the Twitter for part of the the source and filter out retweets

trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source) 

#we are interested in what happened during the campaign so we will focus on what was tweeted between the day
#trump announced his campaign and election day

campaign_tweets <- trump_tweets %>%
  extract(source,"source","Twitter for (.*)")%>%
  filter(source %in% c("Android","iPhone") & created_at>= ymd("2015-06-17") & created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>% arrange(created_at)



#we can now use data visualization to explore the possibility that two different groups were tweeting
#from these devices. For each tweet, we will extract the hour, then compute the proportion of tweets tweeted
#at each hour for each device

ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at,"EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n/sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour,percent,color=source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of the day (ETS)",y="%of tweets",color="")

#we notice a big peak for the android i early hours of the morning.
#there seems to be a clear difference in these patterns
#we will assume two different entities are using these two devices.


#text as data
#the tidytext package helps us convert free form text into a tidy table,
#which facilitates data visualization and statistics

library(tidytext)

#main function is unnest_tokens. A token refers to the units that we are considering
#to be a data point.
#most token will be words but they can also be characters, sentences, patterns.
#the functions will take a vector of strings and extract the tokens 
example <- data_frame(line = c(1, 2, 3, 4),
                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example
example %>% unnest_tokens(word, text)

#quick exmaple for tweet number 3008

i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

#function triws to convert tokens into words and strips characters important to twiter such as # and @
#thus instead of using defualt token words, we define regex that captures twitter characters.
#The pattern starst with @, # or neither and is followed by any combination of letters or digits

pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

#We can now use the unnest_tokens function with the regex option and appropriately extract hastags and mentions:

campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

#another adjustment is to remove links to pictures:

campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

#we are now ready to extract the words for all out tweets
tweet_words <- campaign_tweets %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) 

#we can now answer questions such as what are the most commonly used words?

tweet_words %>%
  count(word) %>%
  arrange(desc(n))
=
#let's filter our stop words (words that are not very informative, prepositions, verbs etc)

stop_words

tweet_words <- campaign_tweets %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word)

#a much more informative set of top 10 words

tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))



#more exploration reveals some unwanted characteristics. First, some tokens are justnumbers. Also
#some tokens come from a quote and start with "

tweet_words <- campaign_tweets %>%
  mutate(text = str_replace_all(text,"https://t.co/[A-Za-z\\d]+|&amp;", ""))%>%
  unnest_tokens(word,text,token="regex",pattern=pattern) %>%
  filter(!word %in% stop_words$word & !str_detect(word,"^\\d+$")) %>%
  mutate(word=str_replace(word,"^'",""))

#Now that we have all words in a table, we can see what device was used to compose that tweet
#For each word, we want to know if it is more likely to come from an Android or an iphone.

android_iphone_or <- tweet_words %>%
  count(word,source) %>%
  spread(source,n,fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android)-Android+0.5) / 
           ((iPhone+0.5)/(sum(iPhone)-iPhone+0.5)))

android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

#some words are being tweeted more in one device versus the other

