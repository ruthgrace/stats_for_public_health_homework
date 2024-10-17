# to run this on command line: R < 1_baltimore_air.r --no-save

library(tidyverse)
library(haven)
library(glue)

air_data <- read_dta("data/balt621.dta")

# PROBLEM 1 i - stratify by season
seasonal_air_data <- air_data %>% group_by(season)
# get summary stats
print(seasonal_air_data %>%
    summarize(
        count = n(),
        mean_pm10 = mean(pm10, na.rm = TRUE),
        mean_mortality = mean(death)
    ))

# PROBLEM 1 ii - stratify by polution
pollution_stratified <- seasonal_air_data %>%
    mutate(strata = ntile(pm10, 5))

print(n = 24, pollution_stratified %>%
    group_by(season, strata) %>%
    summarize(
        count = n(), # Count of observations
        mean_pollution = mean(pm10),
        mean_mortality = mean(death)
    ))

# PROBLEM 1 iv - t-test on highest and lowest strata pollution mortality
winter_low_pollution <- pollution_stratified %>% filter(season == 1, strata == 1)
winter_high_pollution <- pollution_stratified %>% filter(season == 1, strata == 5)
print(t.test(winter_low_pollution$death, winter_high_pollution$death, alternative = "two.sided", var.equal = FALSE))

spring_low_pollution <- pollution_stratified %>% filter(season == 2, strata == 1)
spring_high_pollution <- pollution_stratified %>% filter(season == 2, strata == 5)
print(t.test(spring_low_pollution$death, spring_high_pollution$death, alternative = "two.sided", var.equal = FALSE))

summer_low_pollution <- pollution_stratified %>% filter(season == 3, strata == 1)
summer_high_pollution <- pollution_stratified %>% filter(season == 3, strata == 5)
print(t.test(summer_low_pollution$death, summer_high_pollution$death, alternative = "two.sided", var.equal = FALSE))

fall_low_pollution <- pollution_stratified %>% filter(season == 4, strata == 1)
fall_high_pollution <- pollution_stratified %>% filter(season == 4, strata == 5)
print(t.test(fall_low_pollution$death, fall_high_pollution$death, alternative = "two.sided", var.equal = FALSE))

summary(winter_low_pollution$death)
length(winter_low_pollution$death)
sd(winter_low_pollution$death)

summary(winter_high_pollution$death)
length(winter_high_pollution$death)
sd(winter_high_pollution$death)

summary(spring_low_pollution$death)
length(spring_low_pollution$death)
sd(spring_low_pollution$death)

summary(spring_high_pollution$death)
length(spring_high_pollution$death)
sd(spring_high_pollution$death)

summary(summer_low_pollution$death)
length(summer_low_pollution$death)
sd(summer_low_pollution$death)

summary(summer_high_pollution$death)
length(summer_high_pollution$death)
sd(summer_high_pollution$death)

summary(fall_low_pollution$death)
length(fall_low_pollution$death)
sd(fall_low_pollution$death)

summary(fall_high_pollution$death)
length(fall_high_pollution$death)
sd(fall_high_pollution$death)

library(distributions3)

glue("Winter Low Pollution has mortality 95% confidence interval of {summary(winter_low_pollution$death)['Mean']} ± {abs(quantile(StudentsT(df = (length(winter_low_pollution$death)-1)), 0.025)*sd(winter_low_pollution$death)/sqrt(length(winter_low_pollution$death)))}")
glue("Winter High Pollution has mortality 95% confidence interval of {summary(winter_high_pollution$death)['Mean']} ± {abs(quantile(StudentsT(df = (length(winter_high_pollution$death)-1)), 0.025)*sd(winter_high_pollution$death)/sqrt(length(winter_high_pollution$death)))}")

glue("Spring Low Pollution has mortality 95% confidence interval of {summary(spring_low_pollution$death)['Mean']} ± {abs(quantile(StudentsT(df = (length(spring_low_pollution$death)-1)), 0.025)*sd(spring_low_pollution$death)/sqrt(length(spring_low_pollution$death)))}")
glue("Spring High Pollution has mortality 95% confidence interval of {summary(spring_high_pollution$death)['Mean']} ± {abs(quantile(StudentsT(df = (length(spring_high_pollution$death)-1)), 0.025)*sd(spring_high_pollution$death)/sqrt(length(spring_high_pollution$death)))}")

glue("Summer Low Pollution has mortality 95% confidence interval of {summary(summer_low_pollution$death)['Mean']} ± {abs(quantile(StudentsT(df = (length(summer_low_pollution$death)-1)), 0.025)*sd(summer_low_pollution$death)/sqrt(length(summer_low_pollution$death)))}")
glue("Summer High Pollution has mortality 95% confidence interval of {summary(summer_high_pollution$death)['Mean']} ± {abs(quantile(StudentsT(df = (length(summer_high_pollution$death)-1)), 0.025)*sd(summer_high_pollution$death)/sqrt(length(summer_high_pollution$death)))}")

glue("Fall Low Pollution has mortality 95% confidence interval of {summary(fall_low_pollution$death)['Mean']} ± {abs(quantile(StudentsT(df = (length(fall_low_pollution$death)-1)), 0.025)*sd(fall_low_pollution$death)/sqrt(length(fall_low_pollution$death)))}")
glue("Fall High Pollution has mortality 95% confidence interval of {summary(fall_high_pollution$death)['Mean']} ± {abs(quantile(StudentsT(df = (length(fall_high_pollution$death)-1)), 0.025)*sd(fall_high_pollution$death)/sqrt(length(fall_high_pollution$death)))}")
