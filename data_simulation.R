library(tidyverse)

set.seed(42)

# parameters
n <- 10000
cities <- c("Budapest", "Debrecen", "Szeged", "Miskolc", "Pécs", "Győr", "Szombathely", "Eger")
city_weights <- c(0.35, 0.15, 0.12, 0.10, 0.08, 0.08, 0.06, 0.06)
occupations <- c("Software Developer", "Teacher", "Doctor", "Sales Representative", "Engineer", "Accountant", "Nurse", "Manager", "Chef", "Driver")
occupation_weights <- c(0.08, 0.15, 0.05, 0.20, 0.10, 0.12, 0.12, 0.08, 0.05, 0.05)
genders <- c("Male", "Female")

# Function to generate truncated normal distribution
truncated_normal <- function(n, mean, sd, min, max) {
  x <- rnorm(n, mean, sd)
  x[x < min] <- min
  x[x > max] <- max
  return(round(x))
}

generate_income_noise <- function(n) {
  base_noise <- rgamma(n, shape = 1.5, rate = 0.00005)
  extreme_wealth <- ifelse(runif(n) < 0.01, rgamma(n, shape = 3, rate = 0.00001) * 3, 0)
  negative_noise <- -rgamma(n, shape = 1, rate = 0.0002)
  noise <- base_noise + extreme_wealth + ifelse(runif(n) < 0.3, negative_noise, 0)
  return(noise - mean(noise))
}

age_beta <- rbeta(n, 2, 3)
data <- data.frame(
  age = round(age_beta * 95),
  city = sample(cities, n, replace = TRUE, prob = city_weights),
  occupation = sample(occupations, n, replace = TRUE, prob = occupation_weights),
  gender = sample(genders, n, replace = TRUE),
  starting_age = truncated_normal(n, mean = 19, sd = 2, min = 14, max = 24),
  retirement_age = truncated_normal(n, mean = 67, sd = 3, min = 60, max = 75)
)

data <- data %>%
  mutate(
    base_income = 300000,

    age_effect = -80 * age^2 + 8600 * age - 30000,

    city_effect = case_when(
      city == "Budapest" ~ 100000,
      city %in% c("Debrecen", "Szeged") ~ 50000,
      TRUE ~ 0
    ),

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

    # gender pay gap
    gender_effect = ifelse(gender == "Male", 20000, 0),

    noise = generate_income_noise(n)
  ) %>%
  mutate(
    regular_income = pmax(0, base_income + age_effect + city_effect + occupation_effect + gender_effect + noise),

    pension_base = case_when(
      occupation == "Software Developer" ~ 220000,
      occupation == "Doctor" ~ 200000,
      occupation == "Manager" ~ 180000,
      occupation == "Engineer" ~ 170000,
      occupation == "Accountant" ~ 150000,
      occupation == "Teacher" ~ 140000,
      occupation == "Nurse" ~ 130000,
      occupation == "Chef" ~ 120000,
      occupation == "Driver" ~ 110000,
      TRUE ~ 100000
    ),

    pension_city_effect = case_when(
      city == "Budapest" ~ 20000,
      city %in% c("Debrecen", "Szeged") ~ 10000,
      TRUE ~ 0
    ),

    pension = pmax(0, pension_base + pension_city_effect + rnorm(n, 0, 10000)),

    income = case_when(
      age < starting_age ~ 0,
      age > retirement_age ~ pension,
      TRUE ~ regular_income
    )
  ) %>%
  select(age, city, occupation, gender, income, starting_age, retirement_age)

write.csv(data, "hungarian_income_data.csv", row.names = FALSE)
