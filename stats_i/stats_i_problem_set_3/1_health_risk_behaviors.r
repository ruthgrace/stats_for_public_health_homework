# to run this on command line: R < 1_health_risk_behaviors.r --no-save

library(tidyverse)
library(haven)
library(glue)

class_data <- read_dta("data/2019class621.dta")

# PROBLEM 1 i - Identify outliers

summary(class_data$weight)
summary(class_data$wght18)
summary(class_data$height)

# sort the weight at age 18 to see the range of values apart from the outliers
class_data$wght18[order(class_data$wght18)]

class_data$wght18[class_data$wght18 < 70] <- NA
class_data$wght18[class_data$wght18 > 500] <- NA
summary(class_data$wght18)

# calculate BMI
class_data$bmi <- 704.5 * class_data$weight / (class_data$height * class_data$height)
summary(class_data$bmi)

# PROBLEM 1 ii & iii - Data by gender

gender_grouped_data <- class_data %>% group_by(gender)
tally(gender_grouped_data)
male_data <- gender_grouped_data %>% filter(gender == 1)
female_data <- gender_grouped_data %>% filter(gender == 2)

# stem and leaf plots
stem(male_data$bmi)
stem(female_data$bmi)

# mean and standard deviation
glue("Mean male BMI is {mean(male_data$bmi)}")
glue("Mean female BMI is {mean(female_data$bmi)}")
glue("Standard deviation of male BMI is {sd(male_data$bmi)}")
glue("Standard deviation of female BMI is {sd(female_data$bmi)}")

# 50% intervals if normally distributed
glue("middle 50% interval BMI for males in the class (assuming a normal distribution) ranges from {qnorm(0.25, mean(male_data$bmi), sd(male_data$bmi))} to {qnorm(0.75, mean(male_data$bmi), sd(male_data$bmi))}")
glue("middle 50% interval BMI for females in the class (assuming a normal distribution) ranges from {qnorm(0.25, mean(female_data$bmi), sd(female_data$bmi))} to {qnorm(0.75, mean(female_data$bmi), sd(female_data$bmi))}")
# 95% intervals if normally distributed
glue("middle 95% interval BMI for males in the class (assuming a normal distribution) ranges from {qnorm(0.025, mean(male_data$bmi), sd(male_data$bmi))} to {qnorm(0.975, mean(male_data$bmi), sd(male_data$bmi))}")
glue("middle 95% interval BMI for females in the class (assuming a normal distribution) ranges from {qnorm(0.025, mean(female_data$bmi), sd(female_data$bmi))} to {qnorm(0.975, mean(female_data$bmi), sd(female_data$bmi))}")

# intervals given actual distribution
male_percentiles <- quantile(male_data$bmi, probs = c(0.005, 0.025, 0.25, 0.75, 0.975, 0.995))
female_percentiles <- quantile(female_data$bmi, probs = c(0.005, 0.025, 0.25, 0.75, 0.975, 0.995))
# 50%
glue("middle 50% interval BMI for males in the class ranges from {male_percentiles['25%']} to {male_percentiles['75%']}")
glue("middle 50% interval BMI for females in the class ranges from {female_percentiles['25%']} to {female_percentiles['75%']}")
# 95%
glue("middle 95% interval BMI for males in the class ranges from {male_percentiles['2.5%']} to {male_percentiles['97.5%']}")
glue("middle 95% interval BMI for females in the class ranges from {female_percentiles['2.5%']} to {female_percentiles['97.5%']}")
# 99%
glue("middle 99% interval BMI for males in the class ranges from {male_percentiles['0.5%']} to {male_percentiles['99.5%']}")
glue("middle 99% interval BMI for females in the class ranges from {female_percentiles['0.5%']} to {female_percentiles['99.5%']}")

# PROBLEM 1 iv - comparison with normal distribution
glue("Assuming a normal distribution, the probability of a male in this dataset having a BMI < 25 is {pnorm(25, mean(male_data$bmi), sd(male_data$bmi))} .")
glue("The probability of a male in this dataset having a BMI greater than 25 but less than 30 is {pnorm(30, mean(male_data$bmi), sd(male_data$bmi))-pnorm(25, mean(male_data$bmi), sd(male_data$bmi))} .")
glue("The probability of a male in this dataset having a BMI 30 or greater is {1-pnorm(30, mean(male_data$bmi), sd(male_data$bmi))} .")

glue("Assuming a normal distribution, the probability of a female in this dataset having a BMI < 25 is {pnorm(25, mean(female_data$bmi), sd(female_data$bmi))} .")
glue("The probability of a female in this dataset having a BMI greater than 25 but less than 30 is {pnorm(30, mean(female_data$bmi), sd(female_data$bmi))-pnorm(25, mean(female_data$bmi), sd(female_data$bmi))} .")
glue("The probability of a female in this dataset having a BMI 30 or greater is {1-pnorm(30, mean(female_data$bmi), sd(female_data$bmi))} .")

# PROBLEM 1 v - quantile-normal plot
qqnorm(class_data$bmi, main = "Quantile-Normal plot of class BMI", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(class_data$bmi)

# PROBLEM 1 vi - quantile-normal plot by gender
qqnorm(male_data$bmi, main = "Quantile-Normal plot of male BMI from class sample", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(male_data$bmi)

qqnorm(female_data$bmi, main = "Quantile-Normal plot of female BMI from class sample", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(female_data$bmi)

# PROBLEM 1 vii - quantile-quantile plot by gender
qqplot(male_data$bmi, female_data$bmi, main="Quantile-quantile plot of male vs female BMI from class sample", xlab = "male BMI quantiles", ylab = "female BMI quantiles")
abline(lm(sort(sample(female_data$bmi, length(male_data$bmi), replace = FALSE)) ~ sort(male_data$bmi)))
