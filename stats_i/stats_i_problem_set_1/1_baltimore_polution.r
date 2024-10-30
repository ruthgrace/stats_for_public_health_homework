# to run this on command line: R < 1_baltimore_pollution.r --no-save

library(tidyverse)
library(haven)

baltps1 <- read_dta("data/baltps1.dta")

baltps1_grouped <- baltps1 %>% group_by(group)

# PROBLEM 1A - Create stem and leaf display

# Stem and leaf plot for High Particulate Day Mortality

high_part_mort <- baltps1_grouped %>%
  filter(group == '1') %>%
  pull(deaths)

stem(high_part_mort)

# Stem and leaf plot for Low Particulate Day Mortality

low_part_mort <- baltps1_grouped %>%
  filter(group == '2') %>%
  pull(deaths)

stem(low_part_mort)

# PROBLEM 1B - Create box plots

ggplot(baltps1_grouped,
       aes(x=as.factor(group), y=deaths)) + ggtitle("Box plots of mortality after high particulate (1) versus low particulate (2) days") + geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("particulate category")

# PROBLEM 1C - output looks good.
