library(tidyverse)
library(car)
library(lmtest)
library(ggplot2)
library(dplyr)
library(broom)
library(viridis)
library(ggthemes)
library(knitr)
library(kableExtra)

# Data Simulation for Hungarian Income Analysis
set.seed(123) # For reproducibility

# Define parameters
n <- 10000  # Number of observations
cities <- c("Budapest", "Debrecen", "Szeged", "Miskolc", "Pécs", "Győr", "Szombathely", "Eger")
# Define population weights for cities (based on approximate relative population sizes)
city_weights <- c(0.35, 0.15, 0.12, 0.10, 0.08, 0.08, 0.06, 0.06)  # Sums to 1
occupations <- c("Software Developer", "Teacher", "Doctor", "Sales Representative", 
                "Engineer", "Accountant", "Nurse", "Manager", "Chef", "Driver")
# Define occupation weights (based on approximate relative frequency in the workforce)
occupation_weights <- c(0.08, 0.15, 0.05, 0.20, 0.10, 0.12, 0.12, 0.08, 0.05, 0.05)  # Sums to 1
genders <- c("Male", "Female")

# Generate age distribution using a modified beta distribution
# This will create a distribution skewed towards younger ages
# but still maintaining a reasonable proportion of elderly
age_beta <- rbeta(n, 2, 3)  # Beta distribution parameters chosen to skew towards younger ages
data <- data.frame(
  age = round(age_beta * 95),  # Scale to 0-95 range
  city = sample(cities, n, replace = TRUE, prob = city_weights),
  occupation = sample(occupations, n, replace = TRUE, prob = occupation_weights),
  gender = sample(genders, n, replace = TRUE)
)

# Generate income based on relationships
data <- data %>%
  mutate(
    # Base income
    base_income = 300000,
    
    # Age effect (quadratic relationship)
    age_effect = 10000 * (age - 40) - 100 * (age - 40)^2,
    
    # City effect
    city_effect = case_when(
      city == "Budapest" ~ 100000,
      city %in% c("Debrecen", "Szeged") ~ 50000,
      TRUE ~ 0
    ),
    
    # Occupation effect
    occupation_effect = case_when(
      occupation == "Software Developer" ~ 150000,
      occupation == "Doctor" ~ 120000,
      occupation == "Manager" ~ 100000,
      occupation == "Engineer" ~ 80000,
      occupation == "Accountant" ~ 60000,
      occupation == "Teacher" ~ 40000,
      occupation == "Nurse" ~ 30000,
      occupation == "Chef" ~ 20000,
      occupation == "Driver" ~ 10000,
      TRUE ~ 0
    ),
    
    # Gender effect (simulating gender pay gap)
    gender_effect = ifelse(gender == "Male", 20000, 0),
    
    # Random noise
    noise = rnorm(n, 0, 50000)
  ) %>%
  mutate(
    # Calculate regular income
    regular_income = base_income + age_effect + city_effect + occupation_effect + gender_effect + noise,
    
    # Calculate pension (for age > 70)
    # Pension is based on previous occupation and city, but with reduced amounts
    pension_base = case_when(
      occupation == "Software Developer" ~ 120000,
      occupation == "Doctor" ~ 100000,
      occupation == "Manager" ~ 80000,
      occupation == "Engineer" ~ 70000,
      occupation == "Accountant" ~ 50000,
      occupation == "Teacher" ~ 40000,
      occupation == "Nurse" ~ 35000,
      occupation == "Chef" ~ 30000,
      occupation == "Driver" ~ 25000,
      TRUE ~ 20000
    ),
    
    # Add city adjustment to pension
    pension_city_effect = case_when(
      city == "Budapest" ~ 20000,
      city %in% c("Debrecen", "Szeged") ~ 10000,
      TRUE ~ 0
    ),
    
    # Calculate final pension with some random variation
    pension = pension_base + pension_city_effect + rnorm(n, 0, 10000),
    
    # Final income calculation
    income = case_when(
      age < 18 ~ 0,  # No income for children
      age > 70 ~ pension,  # Pension for elderly
      TRUE ~ regular_income  # Regular income for working age
    )
  ) %>%
  select(age, city, occupation, gender, income)

# Save the data
write.csv(data, "hungarian_income_data.csv", row.names = FALSE)

# Print distributions
cat("\nCity Distribution:\n")
print(table(data$city))
cat("\nOccupation Distribution:\n")
print(table(data$occupation))
cat("\nAge Distribution Summary:\n")
print(summary(data$age))
cat("\nAge Group Distribution:\n")
print(table(cut(data$age, breaks = c(0, 15, 65, Inf), labels = c("0-14", "15-64", "65+"))))
cat("\nIncome Distribution Summary:\n")
print(summary(data$income))
cat("\nIncome Distribution by Age Group:\n")
print(tapply(data$income, cut(data$age, breaks = c(0, 18, 70, Inf), labels = c("Under 18", "Working Age", "Pension Age")), summary)) 