## ------------------------------------------------------------------------------------------------------------------------
# Precomputing

set.seed(123)

n <- 50000
policy_id <- 1:n


## ------------------------------------------------------------------------------------------------------------------------
# Policy Level Variables

# Driver age, no huge pile-up at 18 after enforcing minimum  age
driver_age  <- round(rnorm(n, mean = 42, sd = 11))
driver_age  <- pmin(pmax(driver_age, 18), 80)

# Vehicle age, positively skewed. 
vehicle_age <- pmin(rexp(n, rate = 1/4), 20)
vehicle_age <- round(vehicle_age, 1)

# Gender, evenely distributed
gender <- sample(c("M", "F"), n, replace = TRUE)

# Vehicle types. Mostly Sedans/SUVs, Luxury/Commercial rarer
car_type <- sample(
  c("Sedan", "SUV", "Luxury", "Commercial"),
  n,
  replace = TRUE,
  prob = c(0.45, 0.35, 0.10, 0.10) 
)

# Policyholder region. Mostly urban
region <- sample(
  c("Metro", "Suburban", "Rural"),
  n,
  replace = TRUE,
  prob = c(0.5, 0.35, 0.15) 
)

# Discount based on previous claim history, discrete and skewed
no_claim_discount <- sample(x = c("0%", "20%", "40%", "60%"), 
                            size = n, 
                            replace = TRUE,
                            prob = c(0.15, 0.25, 0.35, 0.25))

# Exposure, Half to one year.
exposure <- runif(n, 0.5, 1) 


## ------------------------------------------------------------------------------------------------------------------------
# Frequency Model

# Linear Predictor

lp_freq <-
  -3.2 +
  ifelse(driver_age < 25, 0.45, 0) + # Young drivers more frequent
  ifelse(driver_age > 70, 0.35, 0) + # Old drivers more frequent
  pmin(vehicle_age, 12) * 0.02 + # Old vehicles more frequent
  ifelse(region == "Metro", 0.30, 
         ifelse(region == "Suburban", 0.10, 0)) + # More urban implies more frequent
  ifelse(car_type == "Luxury", 0.25, 
         ifelse(car_type == "Commercial", 0.40, 0)) + # Commerical have more claims, than luxury, than sedan/suv.
  ifelse(no_claim_discount == "20%", -0.20,
         ifelse(no_claim_discount == "40%", -0.45,
                ifelse(no_claim_discount == "60%", -0.70, 0))) + # High the claim discount, lower the frequency
  ifelse(gender == "M", 0.05, 0) # Slightly higher frequency for males

# Parameters

lambda <- exp(lp_freq) * exposure

# Baked in overdispersion

library(MASS)
claim_count <- rnbinom(n, mu = lambda, size = 3) 


## ------------------------------------------------------------------------------------------------------------------------
# Severity Model

# Linear Predictor

lp_sev <-
  8.3 +
  pmin(vehicle_age, 15) * 0.05 + # Old vehicles more severity
  ifelse(car_type == "Luxury", 0.70,
         ifelse(car_type == "Commercial", 0.45, 0)) + # Expensive vehicles more severity
  ifelse(region == "Rural", 0.25, 0) # Rural regions have higher tow and transport costs

# Parameters

mu_sev <- exp(lp_sev)
shape <- 2.5
scale <- mu_sev / shape

# Severity for policies with claims

avg_claim_sev <- numeric(n)
has_claim <- claim_count > 0

avg_claim_sev[has_claim] <-
  rgamma(sum(has_claim), shape = shape, scale = scale[has_claim])


## ------------------------------------------------------------------------------------------------------------------------
# Total Claim Cost

total_claim_cost <- claim_count * avg_claim_sev


## ------------------------------------------------------------------------------------------------------------------------
# Final Dataset

gi_data <- data.frame(
  policy_id,
  exposure,
  driver_age,
  vehicle_age,
  gender,
  car_type,
  region,
  no_claim_discount,
  claim_count,
  avg_claim_sev,
  total_claim_cost
)

head(gi_data)

write.csv(gi_data, here::here("Data/gi_data.csv"))


## ------------------------------------------------------------------------------------------------------------------------
knitr::purl("Synthetic Data.Rmd", here::here("Scripts/Synthetic Data.R"))

