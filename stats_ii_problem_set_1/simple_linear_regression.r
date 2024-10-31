# to run this on command line: R < 1_health_risk_behaviors.r --no-save

library(tidyverse)
library(haven)
library(glue)

ce_data <- read_dta("data/ce621.dta")


# 1) Create a new variable, agegen, as you have done before in Biostat 140.621

# add log10 cost column
ce_data$log10totchg <- log10(ce_data$totchg)

# assign age categories - TODO check if there are supposed to be two or three
ce_data$agecat <- ifelse(ce_data$age <= 60, "<=60", ">60")

# group by age category and sex
ce_grouped <- ce_data %>% group_by(agecat, sex)

ce_data %>%
    unite(as_factor(sex), agecat, sep = "_")
