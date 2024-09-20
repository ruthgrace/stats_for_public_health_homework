# to run this on command line: R < 2_carotid_endarterectomy.r --no-save

library(tidyverse)
library(haven)

# ce_data <- read_dta("data/ce621.dta")

# print(ce_data)

# ce_grouped <- ce_data %>% group_by(sex)

# print(ce_grouped)

# male_ce_charges <- ce_grouped %>%
#   filter(sex == 1) %>%
#   pull(totchg)

# stem(male_ce_charges)

# female_ce_charges <- ce_grouped %>%
#   filter(sex == 2) %>%
#   pull(totchg)

# stem(female_ce_charges)

# summary(male_ce_charges)
# sd(male_ce_charges)
# summary(female_ce_charges)
# sd(female_ce_charges)

ce_data <- read_dta("data/ce621entire.dta")

# remove all data except from year 1995
ce_1995 <- ce_data %>% filter(year == 1995)

# add log10 cost column
ce_1995$log10totchg <- log10(ce_1995$totchg)

# assign age categories
ce_1995$agecat <- ifelse(ce_1995$age <= 50, "<=50", ifelse(ce_1995$age >= 65, ">=65", "<=64"))

# group by age category and sex
ce_grouped <- ce_1995 %>% group_by(agecat, sex)

ggplot(ce_grouped,
       aes(x=as.factor(agecat), y=log10totchg, color=factor(sex))) + ggtitle("Box plots of carotid endarterectomy costs in 1995 for men (1) and women (2)") + geom_boxplot(alpha=0.2) + 
  xlab("age") + scale_color_discrete(name="Sex",
                         labels=c("1 Male","2 Female"))

ggsave("carotid endarterectomy box plots.pdf")

female_ce_charges <- ce_grouped %>% 
  filter(sex == 2) %>% filter(agecat == "<=50") %>% 
  pull(totchg) 

summary(female_ce_charges)