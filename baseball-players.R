library(Lahman)
library(tidyverse)
library(dslabs)
library(broom)

# regression with BB, singles, doubles, triples, HR (assume all variables are jointly normal)
fit <- Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples = X3B/G, 
         HR = HR/G,
         R = R/G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)    #include "data = ." when using tidy()
coefs <- tidy(fit, conf.int = TRUE)

# predict number of runs for each team in 2002 and plot
Teams %>% filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples = X3B/G, 
         HR = HR/G,
         R = R/G)  %>% 
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()

# some players only play part of a game (find player's fractional opportunity in a game)
# first compute average # of team plate appearances per game
pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  pull(pa_per_game) %>% 
  mean

# compute per-PA rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,              #player PA / team avg PA per game
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))       #if that same player batted every time

# add 2002 salary of each player
players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

# pick each player's most played defensive position (if tie, take the first max)
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>% filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(1, which.max)                                                          #apply to tmp_tab rows to get index of position_names
players <- tibble(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%                        #looks better in uppercase
  filter(POS != "P") %>%                                                       #remove pitchers AFTER which.max
  right_join(players, by="playerID") %>%
  filter(!is.na(POS) & !is.na(salary))

# add first and last names
players <- People %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

save(players, file = "players.rda")