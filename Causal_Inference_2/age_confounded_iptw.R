# Load necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

# Set seed for reproducibility
set.seed(123)

# Create a simulated dataset with a confounding variable
n <- 200


data <- tibble(
  id = 1:n,
  age = rnorm(n, mean = 50, sd = 15)
) %>%
  # Treatment assignment depends on age
  mutate(
    p_treat = plogis(-5 + 0.1 * age),
    treatment_num = rbinom(n, 1, p_treat),
    treatment = factor(treatment_num, levels = c(0, 1), labels = c("Control", "Treatment"))
  ) %>%
  # Outcome depends on both age and treatment
  mutate(
    outcome = 50 - 0.3 * age + 10 * treatment_num + rnorm(n, 0, 10)
  ) %>%
  select(-p_treat, -treatment_num)

# Calculate propensity scores and IPW
model_ps <- glm(treatment ~ age, data = data, family = binomial())

data <- data %>%
  mutate(
    ps_raw = predict(model_ps, type = "response"),
    # Adjust propensity score based on actual treatment
    ps = if_else(treatment == "Treatment", ps_raw, 1 - ps_raw),
    # Calculate inverse probability weights
    ipw = 1 / ps
  ) %>%
  select(-ps_raw)

# Calculate estimates using dplyr
estimates <- data %>%
  group_by(treatment) %>%
  summarise(
    naive_mean = mean(outcome),
    weighted_mean = sum(outcome * ipw) / sum(ipw),
    .groups = "drop"
  )

# Extract differences
naive_diff <- estimates %>%
  summarise(diff = diff(naive_mean)) %>%
  pull(diff)

weighted_diff <- estimates %>%
  summarise(diff = diff(weighted_mean)) %>%
  pull(diff)

# True treatment effect
true_effect <- 10

# Print estimates
cat("Naive estimate:", round(naive_diff, 2), "\n")
cat("IPTW estimate:", round(weighted_diff, 2), "\n")
cat("True effect:", true_effect, "\n")
