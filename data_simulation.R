library(tidyverse)

set.seed(42)

# parameters
n <- 10000
cities <- c("Budapest", "Debrecen", "Szeged", "Miskolc", "Pécs", "Győr", "Szombathely", "Eger")
city_weights <- c(0.35, 0.15, 0.12, 0.10, 0.08, 0.08, 0.06, 0.06)
occupations <- c("Software Developer", "Teacher", "Doctor", "Sales Representative", "Engineer", "Accountant", "Nurse", "Manager", "Chef", "Driver")
occupation_weights <- c(0.08, 0.15, 0.05, 0.20, 0.10, 0.12, 0.12, 0.08, 0.05, 0.05)
genders <- c("Male", "Female")

age_beta <- rbeta(n, 2, 3)
data <- data.frame(
  age = round(age_beta * 95),
  city = sample(cities, n, replace = TRUE, prob = city_weights),
  occupation = sample(occupations, n, replace = TRUE, prob = occupation_weights),
  gender = sample(genders, n, replace = TRUE),
  retirement_age = sample(60:75, n, replace = TRUE)
)

data <- data %>%
  mutate(
    base_income = 300000,

    age_effect = 10000 * (age - 40) - 100 * (age - 40)^2,

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

    noise = rnorm(n, 0, 50000)
  ) %>%
  mutate(
    regular_income = base_income + age_effect + city_effect + occupation_effect + gender_effect + noise,

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

    pension_city_effect = case_when(
      city == "Budapest" ~ 20000,
      city %in% c("Debrecen", "Szeged") ~ 10000,
      TRUE ~ 0
    ),

    pension = pension_base + pension_city_effect + rnorm(n, 0, 10000),

    income = case_when(
      age < 18 ~ 0,
      age > retirement_age ~ pension,
      TRUE ~ regular_income
    )
  ) %>%
  select(age, city, occupation, gender, income, retirement_age)

write.csv(data, "hungarian_income_data.csv", row.names = FALSE)
