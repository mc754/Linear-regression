library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1, sample.kind = "Rounding")

galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%                     #father/mother columns become 1 for parent, 1 for height
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

# Compute LSE, SE, CI, p-values for the parentHeight coefficient for each pair
i <- galton %>% 
  group_by(pair) %>% 
  summarise(tidy(lm(childHeight ~ parentHeight), conf.int = T)) %>% 
  filter(term == "parentHeight")

i %>% ggplot(aes(pair, y = estimate, ymin = conf.low, ymax = conf.high)) + 
  geom_errorbar() + 
  geom_point()