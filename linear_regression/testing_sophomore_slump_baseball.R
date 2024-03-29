library(reshape2)
library(lpSolve)
library(Lahman)
library(tidyverse)

#code to create a table with player ID, their names and their most played position

playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(Master,by="playerID") %>%
  select(playerID,nameFirst,nameLast,POS)

#code to create a table with only the ROY winners and add their batting stats

ROY <- AwardsPlayers %>%
  filter(awardID=="Rookie of the Year") %>%
  left_join(playerInfo,by="playerID") %>%
  rename(rookie_year=yearID) %>%
  right_join(Batting,by="playerID") %>%
  mutate(AVG=H/AB) %>%
  filter(POS!="P")

#to keep only the rookie and sophomore seasons and remove players who did not play sophomore seasons
ROY <- ROY %>%
  filter(yearID==rookie_year|yearID==rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie=ifelse(yearID==min(yearID),"rookie","sophomore")) %>%
  filter(n()==2) %>%
  ungroup %>%
  select(playerID,rookie_year,rookie,nameFirst,nameLast,AVG)

#code to use spread function to have one column for the rookie and sophomore years batting averages
ROY <- ROY %>% spread(rookie,AVG) %>% arrange(desc(rookie))

#code to calculate the proportion of players with lower batting average their sophomore year
mean(ROY$sophomore - ROY$rookie <= 0)

#code for players in 2013-2014 seasons and batter more than 130 times
two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(nameFirst, nameLast, `2013`, `2014`)

two_years

#code to see what happens to the worst performers of 2013

arrange(two_years,`2013`)

#code to see the correlation for performance in two different years

qplot(`2013`,`2014`,data=two_years)

summarize(two_years,cor(`2013`,`2014`))

