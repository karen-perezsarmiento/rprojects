library(tidyverse)
library(dslabs)

set.seed(1989,sample.kind="Rounding")
library(HistData)
data("GaltonFamilies")
options(digits=3)

female_heights <- GaltonFamilies %>%
  filter(gender=="female") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(mother,childHeight) %>%
  rename(daughter = childHeight)

fit <- lm(mother ~ daughter, data=female_heights)

library(Lahman)
bat_02 <- Batting %>% filter(yearID==2002) %>%
  mutate(pa=AB+BB,singles=(H-X2B-X3B-HR)/pa,bb=BB/pa) %>%
  filter(pa>=100) %>%
  select(playerID,singles,bb)

bat <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB+BB, singles=(H-X2B-HR)/pa,bb=BB/pa) %>%
  filter(pa>=100) %>%
  group_by(playerID) %>%
  summarize(mean_singles=mean(singles),mean_bb=mean(bb)) %>%
  select(playerID,mean_singles,mean_bb)

in_join <- bat %>% inner_join(bat_02,by="playerID") 

in_join %>% summarize(rho_singles=cor(singles,mean_singles),rho_bb=cor(bb,mean_bb))

in_join %>% ggplot() + geom_point(aes(singles,mean_singles)) 

in_join %>% ggplot() + geom_point(aes(bb,mean_bb)) 

lm_singles <- lm(singles ~ mean_singles,data=in_join)
lm_bb <- lm(bb ~ mean_bb,data=in_join)