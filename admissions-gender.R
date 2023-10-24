library(dslabs)
library(tidyverse)
library(broom)

# test whether gender and admission are independent
admissions %>% group_by(gender) %>% 
  summarize(total_admitted = round(sum(admitted / 100 * applicants)), 
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>% 
  summarize(tidy(chisq.test(.)))           #reject null that gender and admissions are independent

# percent admissions by major
admissions %>% select(major, gender, admitted) %>%
  pivot_wider(names_from = gender, values_from = admitted) %>%
  mutate(women_minus_men = women - men)

# plot total percent admitted to major versus percent women applicants
admissions %>% group_by(major) %>% 
  summarize(major_selectivity = sum(admitted * applicants) / sum(applicants),
            percent_w_applicants = sum(applicants * (gender=="women")) / sum(applicants) * 100) %>%
  ggplot(aes(major_selectivity, percent_w_applicants, label = major)) +
  geom_text()