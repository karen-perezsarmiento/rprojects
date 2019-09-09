library(Lahman)
library(dplyr)
library(tidyverse)

data(Teams)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(R_per_game = R/G, AB_per_game=AB/G) %>%
  ggplot(aes(R_per_game,AB_per_game)) +
  geom_point(alpha=0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(W_per_game = W/R, E_per_game=E/R) %>%
  ggplot(aes(E_per_game,W_per_game)) +
  geom_point(alpha=0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3B_per_game=X3B/G,X2B_per_game=X2B/G) %>%
  ggplot(aes(X2B_per_game,X3B_per_game)) +
  geom_point(alpha=0.5)