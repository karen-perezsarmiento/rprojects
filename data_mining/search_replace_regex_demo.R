library(tidyverse)
library(dslabs)
data(reported_heights)
class(reported_heights$height)

# keep only entries that result in NAs
reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>% 
  head(n=10)

# keep only entries that either result in NAs or are outside the plausible range of heights
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

# problematic entries
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  .$height

#number of entries matching our desired pattern
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems,pattern))

#inspect examples of entries with problems
problems[c(2,10,11,12,15)] %>% str_view(pattern)
str_subset(problems,"inches")
str_subset(problems,"''")


#replace or remove feet/inches words before matching
pattern <- "^[4-7]'\\d{1,2}$"
problems %>%
  str_replace("feet\ft\foot","'") %>% #replace feet, ft foot with '
  str_replace("inches\in|''|\"","") %>% #remove all inches symbols
  str_detect(pattern) %>%
  sum()

#R does not ignore whitespace
identical("Hi","Hi ")

# \\s represents whitespace
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems,pattern_2)

# * means 0 or more instances of a character
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B")
str_detect(no, "A1*B")

#test how *, ? and + differ
data.frame(string = c("AB","A1B","A11B","A111B","A1111B"),
           none_or_more=str_detect(yes,"A1*B"),
           none_or_once=str_detect(yes,"A1?B"),
           once_or_more=str_detect(yes,"A1+B"))

#update pattern y adding optional spaces before and after feet symbol
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>%
  str_replace("feet|ft|foot","'") %>% #replace feet, ft foot with '
  str_replace("inches|in|''|\"","") %>% #remove all inches symbols
  str_detect(pattern) %>%
  sum()