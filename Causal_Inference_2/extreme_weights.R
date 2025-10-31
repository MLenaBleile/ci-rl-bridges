set.seed(1998)
# Include some very young and very old patients
age <- c(rnorm(100, mean = 50, sd = 15),
         rnorm(100, mean = 25, sd = 3), # Very young
         rnorm(5, mean = 85, sd = 2))
n <- length(age)
# Strong age-treatment relationship
p_treat <- plogis(-8 + 0.15 * age)
# Very old
treatment <- rbinom(n, 1, p_treat)
outcome <- 50- 0.3 * age + 10 * treatment + rnorm(n, 0, 10)
# Compute IPTW weights
ps_model <- glm(treatment ~ age, family = binomial())
ps <- predict(ps_model, type = "response")
weights <- ifelse(treatment == 1, 1 / ps, 1 / (1- ps))
# Check for extreme weights
summary(weights)
max(weights)
