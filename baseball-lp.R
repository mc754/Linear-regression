#install.packages("lpSolve", dependencies = T)
library(Lahman)
library(tidyverse)
library(dslabs)
library(broom)
library(reshape2)
library(lpSolve)
load("players.rda")

players <- players %>% filter(debut <= "1997-01-01" & debut > "1988-01-01")
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)   #transform long format into wide [vector/matrix/array]
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)        #add 1 row for salary
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max",                         #optimization direction: "min" (default) or "max"
                  players$R_hat,                 #objective.in: numeric vector
                  constraint_matrix,             #1 row per constraint, 1 column per variable (unless transpose.constraints = F)
                  constraint_dir,                #character vector for direction
                  constraint_limit,
                  all.int = TRUE)                #should all variables be integer? Default: F

# algorithm chooses these 8 players:
our_team <- players %>%
  filter(lp_solution$solution == 1) %>%          #vector of optimal coefficients
  select(nameFirst, nameLast, POS, salary, R_hat) %>%
  arrange(desc(R_hat))

# check if above average
my_scale <- function(x) (x - median(x))/mad(x)   #median absolute deviation (robust to outliers)
players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))