# to run this on command line: R < 1_nepal_vitamin_a.r --no-save
library(tidyverse)
library(haven)
library(infer)

nepal_data <- read_dta("data/nepal621.dta")
print(nepal_data)
nepal_grouped <- nepal_data %>% group_by(trt, status, sex)
print(nepal_grouped)

# Convert labelled columns to factors
nepal_data <- nepal_data %>%
  mutate(
    trt = as_factor(trt),
    status = as_factor(status),
    sex = as_factor(sex)
  )

treatment_chisq_result <- nepal_data %>%
  chisq_test(trt ~ status)
print(treatment_chisq_result)

sex_chisq_result <- nepal_data %>%
  chisq_test(sex ~ status)
print(sex_chisq_result)