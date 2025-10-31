# =====================================================
# Simple EM Algorithm to Show Efficiency Gains
# Using the existing chapter data
# =====================================================

library(tidyverse)
library(patchwork)

# Use the exact simulation from the chapter
set.seed(1998)
n <- 1000

df_oracle <- tibble(
  Subject = 1:n,
  Treatment = sample(c("a1", "a2"), n, replace = TRUE),
  Smoking_Status = sample(c("Smoker", "Non-smoker"), n, 
                          replace = TRUE, prob = c(0.4, 0.6))
) %>%
  mutate(
    True_Outcome = case_when(
      Treatment == "a1" & Smoking_Status == "Smoker" ~ 20,
      Treatment == "a1" & Smoking_Status == "Non-smoker" ~ 22,
      Treatment == "a2" & Smoking_Status == "Smoker" ~ 25,
      Treatment == "a2" & Smoking_Status == "Non-smoker" ~ 55,
      TRUE ~ NA_real_
    ) + rnorm(n, 0, 3)
  )

# =====================================================
# Simple EM for Binary Smoking Status
# =====================================================

simple_em_cate <- function(data, max_iter = 50, tol = 1e-6) {
  # Identify missing patterns
  data <- data %>%
    mutate(
      is_observed = !is.na(Smoking_Status),
      is_missing = is.na(Smoking_Status)
    )
  
  
  # Initialize: estimate P(Smoker) from observed data
  p_smoker_obs <- data %>%
    filter(is_observed) %>%
    summarise(p = mean(Smoking_Status == "Smoker")) %>%
    pull(p)
  
  # Store convergence history
  p_history <- numeric(max_iter)
  
  for (iter in 1:max_iter) {
    p_smoker_old <- p_smoker_obs
    
    # ========== E-STEP ==========
    # Estimate outcome means for each group from observed data
    group_means <- data %>%
      filter(is_observed) %>%
      group_by(Treatment, Smoking_Status) %>%
      summarise(mean_outcome = mean(True_Outcome), .groups = "drop")
    
    mean_a1_smoker <- group_means %>%
      filter(Treatment == "a1", Smoking_Status == "Smoker") %>%
      pull(mean_outcome)
    
    mean_a1_nonsmoker <- group_means %>%
      filter(Treatment == "a1", Smoking_Status == "Non-smoker") %>%
      pull(mean_outcome)
    
    mean_a2_smoker <- group_means %>%
      filter(Treatment == "a2", Smoking_Status == "Smoker") %>%
      pull(mean_outcome)
    
    mean_a2_nonsmoker <- group_means %>%
      filter(Treatment == "a2", Smoking_Status == "Non-smoker") %>%
      pull(mean_outcome)
    
    # For subjects with missing smoking status, compute posterior probability
    # P(Smoker | Outcome, Treatment) using Bayes' rule
    data <- data %>%
      mutate(
        prob_smoker = case_when(
          is_observed & Smoking_Status == "Smoker" ~ 1,
          is_observed & Smoking_Status == "Non-smoker" ~ 0,
          is_missing & Treatment == "a1" ~ {
            lik_smoker <- dnorm(True_Outcome, mean_a1_smoker, 3)
            lik_nonsmoker <- dnorm(True_Outcome, mean_a1_nonsmoker, 3)
            (lik_smoker * p_smoker_obs) / 
              (lik_smoker * p_smoker_obs + lik_nonsmoker * (1 - p_smoker_obs))
          },
          is_missing & Treatment == "a2" ~ {
            lik_smoker <- dnorm(True_Outcome, mean_a2_smoker, 3)
            lik_nonsmoker <- dnorm(True_Outcome, mean_a2_nonsmoker, 3)
            (lik_smoker * p_smoker_obs) / 
              (lik_smoker * p_smoker_obs + lik_nonsmoker * (1 - p_smoker_obs))
          },
          TRUE ~ NA_real_
        )
      )
    
    # ========== M-STEP ==========
    # Update P(Smoker) using all data (observed + expected)
    p_smoker_new <- mean(data$prob_smoker)
    
    # Compute weighted CATE estimates
    weighted_means <- data %>%
      group_by(Treatment) %>%
      summarise(
        mean_smoker = sum(True_Outcome * prob_smoker) / sum(prob_smoker),
        mean_nonsmoker = sum(True_Outcome * (1 - prob_smoker)) / sum(1 - prob_smoker),
        .groups = "drop"
      )
    
    mean_a1_smoker_w <- weighted_means %>%
      filter(Treatment == "a1") %>%
      pull(mean_smoker)
    
    mean_a1_nonsmoker_w <- weighted_means %>%
      filter(Treatment == "a1") %>%
      pull(mean_nonsmoker)
    
    mean_a2_smoker_w <- weighted_means %>%
      filter(Treatment == "a2") %>%
      pull(mean_smoker)
    
    mean_a2_nonsmoker_w <- weighted_means %>%
      filter(Treatment == "a2") %>%
      pull(mean_nonsmoker)
    
    # Update p_smoker for next iteration
    p_smoker_obs <- p_smoker_new
    p_history[iter] <- p_smoker_new
    
    # Check convergence
    if (abs(p_smoker_new - p_smoker_old) < tol) {
      break
    }
  }
  
  # Final CATE estimates
  cate_smokers <- mean_a2_smoker_w - mean_a1_smoker_w
  cate_nonsmokers <- mean_a2_nonsmoker_w - mean_a1_nonsmoker_w
  
  return(list(
    cate = c(smokers = cate_smokers, nonsmokers = cate_nonsmokers),
    iterations = iter,
    p_smoker_final = p_smoker_new,
    p_history = p_history[1:iter]
  ))
}

# Original compute_cate for comparison
compute_cate <- function(data) {
  data %>%
    filter(!is.na(Smoking_Status)) %>%
    group_by(Smoking_Status) %>%
    summarise(
      cate = mean(True_Outcome[Treatment == "a2"]) - 
        mean(True_Outcome[Treatment == "a1"]),
      .groups = "drop"
    ) %>%
    deframe() %>%
    .[c("Smoker", "Non-smoker")] %>%
    setNames(c("smokers", "nonsmokers"))
}

# =====================================================
# Demonstrate Efficiency Gain
# =====================================================

cat("=" , rep("=", 60), "\n", sep="")
cat("COMPARING COMPLETE CASE ANALYSIS VS SIMPLE EM ALGORITHM\n")
cat("=" , rep("=", 60), "\n\n", sep="")

# True values
true_cate <- compute_cate(df_oracle)
cat("TRUE CATE:\n")
cat("  Smokers:     ", round(true_cate[1], 3), "\n")
cat("  Non-smokers: ", round(true_cate[2], 3), "\n\n")

# Test with different missingness levels (MAR)
missingness_levels <- c(0.1, 0.3, 0.5, 0.7)

results_list <- map_dfr(missingness_levels, function(miss_prob) {
  cat("-" , rep("-", 40), "\n", sep="")
  cat("MISSINGNESS LEVEL: ", miss_prob * 100, "%\n", sep="")
  cat("-" , rep("-", 40), "\n", sep="")
  
  # Create MAR missingness (depends on treatment)
  df_mar <- df_oracle %>%
    mutate(
      missing_prob = if_else(Treatment == "a2", miss_prob, miss_prob * 0.2),
      is_missing = runif(n()) < missing_prob,
      Smoking_Status = if_else(is_missing, NA_character_, Smoking_Status)
    )
  
  # Complete case estimate
  cate_cc <- compute_cate(df_mar)
  n_complete <- sum(!is.na(df_mar$Smoking_Status))
  
  # EM estimate
  em_result <- simple_em_cate(df_mar)
  
  cat("\nSample sizes:\n")
  cat("  Complete cases: ", n_complete, "/", n, " (", 
      round(n_complete/n * 100, 1), "%)\n", sep="")
  cat("  EM uses:        ", n, "/", n, " (100%)\n\n", sep="")
  
  cat("CATE Estimates for Smokers:\n")
  cat("  Complete Case: ", round(cate_cc[1], 3), 
      " (bias: ", round(cate_cc[1] - true_cate[1], 3), ")\n", sep="")
  cat("  EM Algorithm:  ", round(em_result$cate[1], 3), 
      " (bias: ", round(em_result$cate[1] - true_cate[1], 3), ")\n", sep="")
  cat("  EM converged in ", em_result$iterations, " iterations\n\n", sep="")
  
  tibble(
    missingness = miss_prob,
    n_complete = n_complete,
    cate_cc_smokers = cate_cc[1],
    cate_em_smokers = em_result$cate[1],
    iterations = em_result$iterations
  )
})

# =====================================================
# Bootstrap to Show Standard Error Reduction
# =====================================================

cat("=" , rep("=", 60), "\n", sep="")
cat("BOOTSTRAP ANALYSIS: Standard Error Comparison\n")
cat("=" , rep("=", 60), "\n\n", sep="")

# Focus on 50% missingness
miss_prob <- 0.5
n_bootstrap <- 200

set.seed(123)
boot_results <- map_dfr(1:n_bootstrap, function(b) {
  # Resample
  df_boot <- df_oracle %>%
    sample_n(n, replace = TRUE) %>%
    mutate(
      missing_prob = if_else(Treatment == "a2", miss_prob, miss_prob * 0.2),
      is_missing = runif(n()) < missing_prob,
      Smoking_Status = if_else(is_missing, NA_character_, Smoking_Status)
    )
  
  # Estimates
  tibble(
    iteration = b,
    cc_estimate = compute_cate(df_boot)[1],
    em_estimate = simple_em_cate(df_boot)$cate[1]
  )
})

# Calculate statistics
boot_stats <- boot_results %>%
  summarise(
    cc_mean = mean(cc_estimate),
    cc_se = sd(cc_estimate),
    em_mean = mean(em_estimate),
    em_se = sd(em_estimate)
  )

cat("With 50% missingness:\n\n")
cat("Complete Case Analysis:\n")
cat("  Mean estimate: ", round(boot_stats$cc_mean, 3), "\n")
cat("  Standard error: ", round(boot_stats$cc_se, 3), "\n")
cat("  95% CI width: ", round(3.92 * boot_stats$cc_se, 3), "\n\n")

cat("EM Algorithm:\n")
cat("  Mean estimate: ", round(boot_stats$em_mean, 3), "\n")
cat("  Standard error: ", round(boot_stats$em_se, 3), "\n")
cat("  95% CI width: ", round(3.92 * boot_stats$em_se, 3), "\n\n")

efficiency_gain <- (boot_stats$cc_se - boot_stats$em_se) / boot_stats$cc_se * 100
variance_reduction <- (boot_stats$cc_se^2 - boot_stats$em_se^2) / boot_stats$cc_se^2 * 100
effective_multiplier <- boot_stats$cc_se^2 / boot_stats$em_se^2

cat("EFFICIENCY GAIN:\n")
cat("  SE reduction: ", round(efficiency_gain, 1), "%\n")
cat("  Variance reduction: ", round(variance_reduction, 1), "%\n")
cat("  Effective sample size multiplier: ", round(effective_multiplier, 2), "x\n\n")

# =====================================================
# Visualization with ggplot2
# =====================================================

# Plot 1: Bootstrap distribution comparison
p1 <- boot_results %>%
  pivot_longer(cols = c(cc_estimate, em_estimate), 
               names_to = "method", 
               values_to = "estimate") %>%
  mutate(method = if_else(method == "cc_estimate", 
                          "Complete Case", 
                          "EM Algorithm")) %>%
  ggplot(aes(x = estimate, fill = method)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  geom_vline(xintercept = true_cate[1], 
             linetype = "dashed", 
             color = "darkgreen", 
             size = 1) +
  scale_fill_manual(values = c("Complete Case" = "darkgreen", 
                               "EM Algorithm" = "grey")) +
  labs(title = "Bootstrap Distribution (50% Missing)",
       subtitle = paste0("True value = ", round(true_cate[1], 2)),
       x = "CATE Estimate for Smokers",
       y = "Count",
       fill = "Method") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 2: EM Convergence
df_test <- df_oracle %>%
  mutate(
    is_missing = runif(n()) < 0.5,
    Smoking_Status = if_else(is_missing, NA_character_, Smoking_Status)
  )
em_conv <- simple_em_cate(df_test, max_iter = 30)

p2 <- tibble(
  iteration = 1:em_conv$iterations,
  p_smoker = em_conv$p_history
) %>%
  ggplot(aes(x = iteration, y = p_smoker)) +
  geom_line(size = 1, color="grey") +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.4, 
             linetype = "dashed", 
             color = "black") +
  annotate("text", x = 5, y = 0.41, 
           label = "True P(Smoker) = 0.4", 
           color = "black") +
  labs(title = "EM Algorithm Convergence",
       x = "Iteration",
       y = "P(Smoker)") +
  theme_minimal()

# Plot 3: Efficiency across missingness levels
p3 <- results_list %>%
  mutate(
    cc_bias = abs(cate_cc_smokers - true_cate[1]),
    em_bias = abs(cate_em_smokers - true_cate[1]),
    sample_retained = n_complete / n
  ) %>%
  pivot_longer(cols = c(cc_bias, em_bias),
               names_to = "method",
               values_to = "bias") %>%
  mutate(method = if_else(method == "cc_bias", 
                          "Complete Case", 
                          "EM Algorithm")) %>%
  ggplot(aes(x = missingness * 100, y = bias, color = method)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Complete Case" = "darkgreen", 
                                "EM Algorithm" = "grey")) +
  labs(title = "Absolute Bias by Missingness Level",
       x = "Missingness %",
       y = "Absolute Bias",
       color = "Method") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 4: Sample size utilization
p4 <- results_list %>%
  mutate(
    `Complete Case` = n_complete,
    `EM Algorithm` = n
  ) %>%
  pivot_longer(cols = c(`Complete Case`, `EM Algorithm`),
               names_to = "method",
               values_to = "n_used") %>%
  ggplot(aes(x = missingness * 100, y = n_used, fill = method)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_hline(yintercept = n, linetype = "dashed", color = "gray50") +
  scale_fill_manual(values = c("Complete Case" = "darkgreen", 
                               "EM Algorithm" = "grey")) +
  labs(title = "Sample Size Usage",
       x = "Missingness %",
       y = "Number of Observations Used",
       fill = "Method") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Combine plots using patchwork
combined_plot <- (p1 + p2) / (p3 + p4) + 
  plot_annotation(
    title = "EM Algorithm vs Complete Case Analysis",
    subtitle = "Demonstrating efficiency gains from using all available data"
  )

print(combined_plot)

# =====================================================
# Summary Statistics Table
# =====================================================

summary_table <- results_list %>%
  mutate(
    missingness_pct = paste0(missingness * 100, "%"),
    cc_bias = round(cate_cc_smokers - true_cate[1], 3),
    em_bias = round(cate_em_smokers - true_cate[1], 3),
    data_used_cc = paste0(round(n_complete/n * 100, 1), "%"),
    data_used_em = "100%"
  ) %>%
  select(
    `Missingness` = missingness_pct,
    `CC Estimate` = cate_cc_smokers,
    `CC Bias` = cc_bias,
    `EM Estimate` = cate_em_smokers,
    `EM Bias` = em_bias,
    `Data Used (CC)` = data_used_cc,
    `Data Used (EM)` = data_used_em
  )

cat("\n", "=" , rep("=", 60), "\n", sep="")
cat("SUMMARY TABLE\n")
cat("True CATE for Smokers: ", round(true_cate[1], 3), "\n", sep="")
cat("=" , rep("=", 60), "\n", sep="")
print(summary_table)

