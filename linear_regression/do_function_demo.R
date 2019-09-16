library(dslabs)
library(Lahman)
library(dplyr)
library(tidyverse)

#use do function to fit a regression line to each HR stratum
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G,1),
         BB=BB/G,
         R=R/G) %>%
  select(HR,BB,R) %>%
  filter(HR>=0.4 & HR<=1.2) %>%
  group_by(HR) %>%
  do(fit=lm(R~BB,data=.))

#using do without a column name gives an error
dat %>%
  group_by(HR) %>%
  do(lm(R~BB,data.))

#define a function to extract slope from lm
get_slope <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2], 
             se = summary(fit)$coefficient[2,2])
}

# return the desired data frame
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G,1),
         BB=BB/G,
         R=R/G) %>%
  select(HR,BB,R) %>%
  filter(HR>=0.4 & HR<=1.2)

dat %>%  
  group_by(HR) %>%
  do(get_slope(.))

#not the desired output: a column containing data frames
dat %>%
  group_by(HR) %>%
  do(slope = get_slope(.))


