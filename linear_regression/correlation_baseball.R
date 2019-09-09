library(Lahman)
library(tidyverse)
library(dplyr)


data(Teams)
corr_R_AB<-Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(R_per_game = R/G, AB_per_game = AB/G) %>% 
  summarize(rho=cor(R_per_game,AB_per_game)) %>% 
  pull(rho)

corr_W_E<-Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(W_per_game = W/G, E_per_game = E/G) %>% 
  summarize(rho=cor(W_per_game,E_per_game)) %>% 
  pull(rho)

corr_X3B_X2B <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3B_per_game = X3B/G,X2B_per_game = X2B/G) %>%
  summarize(rho = cor(X3B_per_game,X2B_per_game)) %>%
  pull(rho)