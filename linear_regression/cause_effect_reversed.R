library(ggplot2)
library(tidyverse)
library(HistData)

#cause and effect reversal
library(HistData)
data("GaltonFamilies")
GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father,childHeight) %>%
  rename(son = childHeight) %>%
  do(tidy(lm(father ~ son, data=.)))