library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1,sample.kind="Rounding")
galton <- GaltonFamilies %>%
  group_by(family,gender) %>%
  sample_n(1) %>%
  ungroup() %>%
  gather(parent,parentHeight,father:mother) %>%
  mutate(child = ifelse(gender=="female","daughter","son")) %>%
  unite(pair,c("parent","child"))

galton %>% group_by(pair) %>% filter(pair=="father_daughter") %>% count()

galton %>% group_by(pair) %>% filter(pair=="mother_son") %>% count()

galton %>% group_by(pair) %>% summarize(rho=cor(parentHeight,childHeight))

lm_heights<-galton %>% 
  group_by(pair) %>%
  do(tidy(lm(childHeight~parentHeight,data=.),conf.int=TRUE)) %>%
  filter(term == "parentHeight") %>%
  select(pair,term,estimate,conf.low,conf.high)

lm_heights %>% ggplot(aes(x=pair,y=estimate,ymin=conf.low,ymax=conf.high)) +
  geom_errorbar()+
  geom_point()