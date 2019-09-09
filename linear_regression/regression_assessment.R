library(HistData)
library(dplyr)
library(tidyverse)

options(digits = 3)

set.seed(1989,sample.kind="Rounding")
data("GaltonFamilies")

female_heights <- GaltonFamilies %>%
  filter(gender == "female") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(mother,childHeight) %>%
  rename(daughter = childHeight)

mean_mother<-mean(female_heights$mother)
mean_daughter<-mean(female_heights$daughter)
sd_mother<-sd(female_heights$mother)
sd_daughter<-sd(female_heights$daughter)

rho <- cor(female_heights$mother,female_heights$daughter)

slope <- rho* sd_daughter/sd_mother

intercept <- mean_daughter - slope*mean_mother

per_var <- 100*rho^2

exp_daughter_height <- intercept + slope*60