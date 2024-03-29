library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

#exploring the relationship between two variables like home runs and runs with a scatterplot

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game,R_per_game)) +
  geom_point(alpha=0.5)

#relationship between stolen bases and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB/G,R_per_game=R/G) %>%
  ggplot(aes(SB_per_game,R_per_game)) +
  geom_point(alpha=0.5)

#scatter plot of the relationship between bases on balls and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
  ggplot(aes(BB_per_game,R_per_game)) +
  geom_point(alpha=0.5)