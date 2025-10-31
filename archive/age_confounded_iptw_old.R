# Load necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

#setwd("C:/Users/User/Documents/RL Book/figures/ch1")
# Set seed for reproducibility
set.seed(123)

# Create a simulated dataset with a confounding variable
n <- 200
# Create age as a confounder (older people more likely to get treatment)
age <- rnorm(n, mean = 50, sd = 15)
# Treatment assignment depends on age
p_treat <- plogis(-5 + 0.1*age)  # Probability increases with age
treatment <- rbinom(n, 1, p_treat)
# Outcome depends on both age and treatment
# Older people have worse outcomes, treatment has positive effect
outcome <- 50 - 0.3*age + 10*treatment + rnorm(n, 0, 10)

# Create dataset
data <- data.frame(
  id = 1:n,
  age = age,
  treatment = factor(treatment, labels = c("Control", "Treatment")),
  outcome = outcome
)

# Calculate propensity scores
model_ps <- glm(treatment ~ age, data = data, family = binomial())
data$ps <- predict(model_ps, type = "response")
data$ps[data$treatment == "Control"] <- 1 - data$ps[data$treatment == "Control"]

# Calculate inverse probability weights
data$ipw <- 1/data$ps

# Unadjusted estimate
naive_diff <- mean(data$outcome[data$treatment == "Treatment"]) - 
  mean(data$outcome[data$treatment == "Control"])

# IPTW-adjusted estimate
weighted_diff <- sum(data$outcome[data$treatment == "Treatment"] * 
                       data$ipw[data$treatment == "Treatment"]) / 
  sum(data$ipw[data$treatment == "Treatment"]) - 
  sum(data$outcome[data$treatment == "Control"] * 
        data$ipw[data$treatment == "Control"]) / 
  sum(data$ipw[data$treatment == "Control"])

# True treatment effect (we know it's 10 from our simulation)
true_effect <- 10

# Create visualization of the confounded data
p1 <- ggplot(data, aes(x = age, y = outcome, color = treatment, size = 1)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("Control" = "#3498db", "Treatment" = "#e74c3c")) +
  labs(title = "A) Original Data",
       subtitle = paste0("Naïve estimate: ", round(naive_diff, 1), 
                         " (True effect: ", true_effect, ")"),
       x = "Age", y = "Outcome") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10))

# Visualization of the weighted data
p2 <- ggplot(data, aes(x = age, y = outcome, color = treatment, size = ipw)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("Control" = "#3498db", "Treatment" = "#e74c3c")) +
  labs(title = "B) IPTW",
       subtitle = paste0("IPTW estimate: ", round(weighted_diff, 1), 
                         " (True effect: ", true_effect, ")"),
       x = "Age", y = "Outcome") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10))

# Create a propensity score distribution plot
p3 <- ggplot(data, aes(x = age, fill = treatment)) +
  geom_histogram(alpha = 0.7, position = "identity", bins = 30) +
  scale_fill_manual(values = c("Control" = "#3498db", "Treatment" = "#e74c3c")) +
  labs(title = "C) Propensity Score Distribution",
       subtitle = "Treatment probability increases with age",
       x = "Age", y = "Count") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10))

# Create simplified explanation diagram
explanation_data <- data.frame(
  x = c(1, 1, 2, 2),
  y = c(1, 2, 1, 2),
  weight = c(1, 2, 2, 1),
  group = factor(c("A", "A", "B", "B")),
  label = c("Low weight\n(common)", "High weight\n(rare)", 
            "High weight\n(rare)", "Low weight\n(common)")
)

p4 <- ggplot(explanation_data, aes(x = x, y = y, size = weight, color = group)) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = label), hjust = -0.3, size = 3, color = "black") +
  scale_size_continuous(range = c(5, 15)) +
  scale_color_manual(values = c("A" = "#3498db", "B" = "#e74c3c")) +
  labs(title = "D) IPTW Concept",
       subtitle = "Rare combinations receive higher weights") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10)) +
  xlim(0.5, 4)

# Combine all plots
combined_plot <- grid.arrange(p1, p2, p3, ncol = 3)

# Add main title
title <- textGrob("Inverse Probability of Treatment Weighting (IPTW)",
                  gp = gpar(fontface = "bold", fontsize = 16))
subtitle <- textGrob("Creating a pseudo-population where treatment appears randomized",
                     gp = gpar(fontsize = 12))

final_plot <- grid.arrange(title, subtitle, combined_plot, 
                           heights = c(0.5, 0.3, 10))

# Save the plot
ggsave("iptw_visualization.png", final_plot, width = 12, height = 8, dpi = 300)

# Display the plot
print(final_plot)