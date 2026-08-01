# age_confounded_iptw_base.R
# Demonstrates how confounding by age biases a naive treatment-effect estimate
# and how IPTW recovers the true effect.

set.seed(1998)

# ---- Simulate a confounded dataset ----
n <- 200
age <- rnorm(n, mean = 50, sd = 15)

# Treatment assignment depends on age (older subjects more likely treated)
p_treat <- plogis(-5 + 0.1 * age)
treatment_num <- rbinom(n, 1, p_treat)
treatment <- factor(treatment_num,
                    levels = c(0, 1),
                    labels = c("Control", "Treatment"))

# Outcome depends on both age and treatment (true treatment effect = 10)
outcome <- 50 - 0.3 * age + 10 * treatment_num + rnorm(n, 0, 10)

data <- data.frame(id = 1:n, age = age, treatment = treatment, outcome = outcome)

# ---- Estimate propensity scores and IPT weights ----
model_ps <- glm(treatment ~ age, data = data, family = binomial())
ps_raw <- predict(model_ps, type = "response")

# Probability of receiving the actually-observed treatment
data$ps <- ifelse(data$treatment == "Treatment", ps_raw, 1 - ps_raw)

# Inverse probability weights
data$ipw <- 1 / data$ps

# ---- Compare naive and IPTW estimates ----
# Naive group means
naive_mean <- tapply(data$outcome, data$treatment, mean)

# IPTW-weighted group means: sum(outcome * ipw) / sum(ipw) within each arm
weighted_mean <- with(data,
  tapply(outcome * ipw, treatment, sum) / tapply(ipw, treatment, sum)
)

# Differences (Treatment minus Control)
naive_diff    <- diff(naive_mean)
weighted_diff <- diff(weighted_mean)

# True treatment effect
true_effect <- 10

# Print estimates
cat("Naive estimate:", round(naive_diff, 2), "\n")
cat("IPTW estimate:",  round(weighted_diff, 2), "\n")
cat("True effect:",    true_effect, "\n")
