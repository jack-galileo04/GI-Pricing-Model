## ----setup, include = FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, tidy = TRUE, warning = FALSE, message = FALSE)

library(tidyverse)

gi_data <- read.csv(here::here("Data/gi_data.csv")) |> dplyr::select(-X)


## ------------------------------------------------------------------------------------------------------------------------
# Summary statistics

summary(gi_data)
str(gi_data)

# No missing values, variable classes are reasonable. 
# exposure uniform, driver_age normal, vehicle_age right skew
# claim_count right skew, avg_claim_sev right skew, total_claim_cost right skew


## ------------------------------------------------------------------------------------------------------------------------
# Discrete Counts

tidy_count(gi_data, gender)
tidy_count(gi_data, car_type)
tidy_count(gi_data, region)
tidy_count(gi_data, claim_count)

# Rarely get 3 claims. Typically zero or 1 claim.


## ------------------------------------------------------------------------------------------------------------------------
# Continuous Distributions

gg_hist(gi_data, exposure) # Uniform
gg_hist(gi_data, avg_claim_sev) # Zero inflated positively skewed
gg_hist(gi_data, total_claim_cost) # Zero inflated positively skewed
gg_hist(gi_data, driver_age) # Slight right skew but mostly normal (18 minimum age)
gg_hist(gi_data, vehicle_age) # Vehicle ages are right skewed

gi_data |> 
  filter(total_claim_cost > 0) |> 
  ggplot(aes(total_claim_cost)) + geom_histogram() # Positive skew without zero claim cost

gi_data |> 
  filter(total_claim_cost > 0) |> 
  ggplot(aes(total_claim_cost)) + geom_histogram() + scale_x_log10() # Log transformation normalises


## ------------------------------------------------------------------------------------------------------------------------
# Predictive Relationships with Severity

gi_data |> 
  filter(avg_claim_sev > 0) |> 
  gg_scatter(x = driver_age, y = avg_claim_sev) + 
  scale_y_log10() # Not much of a relationship

gi_data |> 
  filter(avg_claim_sev > 0) |> 
  mutate(age_bucket = case_when(
    driver_age <= 25 ~ "0-25",
    driver_age > 25 & driver_age <= 40 ~ "26-40",
    driver_age > 40 & driver_age <= 60 ~ "41-60",
    driver_age > 60 ~ ">60"
  )) |> 
  mutate(age_bucket = fct_reorder(age_bucket, avg_claim_sev)) |> 
  gg_box(x = age_bucket, y = avg_claim_sev) + 
  scale_y_log10() # Decent relationship, severity decreases with age

gi_data |> 
  filter(avg_claim_sev > 0) |> 
  gg_scatter(x = vehicle_age, y = avg_claim_sev) + 
  scale_y_log10() # Positive correlation

gi_data |> 
  filter(avg_claim_sev > 0) |> 
  mutate(no_claim_discount = fct_reorder(no_claim_discount, avg_claim_sev)) |> 
  gg_box(x = no_claim_discount, y = avg_claim_sev) + 
  scale_y_log10() # Not much of a relationship

gi_data |> 
  filter(avg_claim_sev > 0) |> 
  gg_box(x = gender, y = avg_claim_sev) + 
  scale_y_log10() # Not much of a relationship

gi_data |> 
  filter(avg_claim_sev > 0) |> 
  mutate(car_type = fct_reorder(car_type, avg_claim_sev)) |> 
  gg_box(x = car_type, y = avg_claim_sev) + 
  scale_y_log10() # Strong relationship, luxury and commercial vehicles have larger claims

gi_data |> 
  filter(avg_claim_sev > 0) |> 
  mutate(region = fct_reorder(region, avg_claim_sev)) |> 
  gg_box(x = region, y = avg_claim_sev) + 
  scale_y_log10() # Rural have larger claims.



## ------------------------------------------------------------------------------------------------------------------------
# Predictive Relationships with Frequency

gi_data |> 
  mutate(claim_count = factor(claim_count)) |> 
  gg_box(x = claim_count, y = driver_age) # Generally younger drivers will have more claims

gi_data |> 
  mutate(claim_count = factor(claim_count)) |> 
  gg_box(x = claim_count, y = vehicle_age) # Older Vehicles generally have more claims

gi_data |> 
  group_by(no_claim_discount) |> 
  summarise(mean_claim_count = mean(claim_count)) |> 
  ggplot(aes(no_claim_discount, mean_claim_count)) +
  geom_col() # Higher no claim discounts generally have less frequent claims

gi_data |> 
  group_by(car_type) |> 
  summarise(avg_claim_count = mean(claim_count)) |> 
  mutate(car_type = fct_reorder(car_type, avg_claim_count)) |> 
  ggplot(aes(car_type, avg_claim_count)) + geom_col() # Commerical have more claims, then luxury, then others.

gi_data |> 
  group_by(gender) |> 
  summarise(avg_claim_count = mean(claim_count)) |> 
  mutate(gender = fct_reorder(gender, avg_claim_count)) |> 
  ggplot(aes(gender, avg_claim_count)) + geom_col() # Males have slightly more claims

gi_data |> 
  group_by(region) |> 
  summarise(avg_claim_count = mean(claim_count)) |> 
  mutate(region = fct_reorder(region, avg_claim_count)) |> 
  ggplot(aes(region, avg_claim_count)) + geom_col() # More urban areas have more claims


## ------------------------------------------------------------------------------------------------------------------------
# Collinearity (domain knowledge)

gg_scatter(gi_data, x = driver_age, y = vehicle_age) # No correlation


## ------------------------------------------------------------------------------------------------------------------------
# Interaction Effects

gi_data |> 
  ggplot(aes(driver_age, avg_claim_sev, colour = no_claim_discount)) +
  geom_point() + 
  scale_y_log10() # No interactions
gi_data |> 
  ggplot(aes(no_claim_discount, driver_age)) +
  geom_boxplot() + 
  facet_wrap(~factor(claim_count)) # Maybe some interaction? Zero claims

gi_data |> 
  ggplot(aes(car_type, avg_claim_sev)) +
  geom_boxplot() + 
  scale_y_log10() + 
  facet_wrap(~region) # No interactions
gi_data |> 
  group_by(car_type, region) |> 
  summarise(avg_claim_count = mean(claim_count)) |> 
  ggplot(aes(car_type, avg_claim_count)) +
  geom_col() + 
  facet_wrap(~region) # No clear interactions (although suburban seems to boost commercial)

gi_data |> 
  ggplot(aes(driver_age, avg_claim_sev)) +
  geom_point() + 
  scale_y_log10() + 
  facet_wrap(~car_type) # No interactions
gi_data |> 
  mutate(claim_count = factor(claim_count)) |> 
  ggplot(aes(claim_count, driver_age)) +
  geom_boxplot() + 
  coord_flip() +
  facet_wrap(~car_type) # No interactions


## ------------------------------------------------------------------------------------------------------------------------
# Feature Engineering

model_data <- gi_data |> 
  mutate(age_bucket = case_when(
    driver_age <= 25 ~ "18-25",
    driver_age > 25 & driver_age <= 40 ~ "26-40",
    driver_age > 40 & driver_age <= 60 ~ "41-60",
    driver_age > 60 ~ ">60")
    ) |> 
  mutate(age_bucket = cut(driver_age, 
                          breaks = c(18, 25, 35, 45 ,55, 65 ,80),
                          right = FALSE))

write.csv(model_data, here::here("Data/model_data.csv"))


## ------------------------------------------------------------------------------------------------------------------------
knitr::purl("EDA and Feature Engineering.Rmd", here::here("Scripts/EDA and Feature Engineering.R"))

