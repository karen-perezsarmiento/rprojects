library(Lahman)
library(tidyverse)
library(dplyr)
library(broom)

dat<- Teams %>% filter(yearID==1971)

fit <- lm(R~BB+HR,data=dat)

tidy(fit)

dat2 <- Teams %>% filter(yearID %in% 1961:2018) %>% group_by(yearID) %>% do(tidy(lm(R ~ BB+HR,data=.),conf.int=TRUE))

dat2 %>% filter(term=="BB") %>% ggplot(aes(yearID,estimate,ymin=conf.low,ymax=conf.high)) + geom_errorbar() + geom_point() +geom_smooth()

BB_year <- Teams %>% 
  filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>% 
  do(tidy(lm(R ~ BB+HR, data=.),conf.int=TRUE)) %>%
  filter(term=="BB")

fit_bbyear<-lm(estimate~yearID,data=BB_year)
summary(fit_bbyear)