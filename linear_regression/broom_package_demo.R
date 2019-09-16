library(Lahman)
library(tidyverse)
library(dplyr)

#data
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G,1),
         BB=BB/G,
         R=R/G) %>%
  select(HR,BB,R) %>%
  filter(HR>=0.4 & HR<=1.2)


#use tidy to return lm estimates and related information as a data frame
library(broom)
fit <- lm(R~BB,data=dat)
tidy(fit)

#add confidence intervals with tidy
tidy(fit,conf.int=TRUE)

#pipeline with lm, do, tidy
dat %>%
  group_by(HR) %>%
  do(tidy(lm(R~BB,data=.),conf.int=TRUE)) %>%
  filter(term=="BB") %>%
  select(HR,estimate,conf.low,conf.high)

#make ggplots
dat %>%
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data =.),conf.int=TRUE)) %>%
  filter(term=="BB") %>%
  select(HR,estimate,conf.low,conf.high) %>%
  ggplot(aes(HR,y=estimate,ymin=conf.low,ymax=conf.high)) +
  geom_errorbar() +
  geom_point()

#inspect with glance
glance(fit)
