library(Lahman)
library(dplyr)
top <- Batting %>%
  filter(yearID==2016) %>%
  arrange(desc(HR)) %>%
  slice(1:10)

top %>% as_tibble()

Master %>% as_tibble()

top_names <- top %>% left_join(Master) %>%
  select(playerID,nameFirst,nameLast,HR)

top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names)%>%
  select(nameFirst, nameLast, teamID, HR, salary)

AwardsPlayers <- AwardsPlayers %>% filter(yearID==2016)

top_names_winner<- semi_join(top_names,AwardsPlayers)

winners_not_top <- anti_join(AwardsPlayers,top_names) %>% distinct(playerID)