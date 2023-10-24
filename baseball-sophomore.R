library(Lahman)
library(tidyverse)
library(dslabs)
library(broom)

# create a table with player ID, their names, and their most played position
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>%
  left_join(People, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

# create a table with only the ROY award winners and add their batting statistics
ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  left_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")

# keep only the rookie and sophomore seasons and remove players who did not play sophomore seasons
ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

# use spread to have 1 column for the rookie and sophomore years batting averages
ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
mean(ROY$sophomore - ROY$rookie <= 0)

# analyze all players that played in 2013 and 2014 and batted more than 130 times (min to win ROY)
Batting %>% filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%                       #ascending shows regression to the mean
  select(nameFirst, nameLast, `2013`, `2014`)