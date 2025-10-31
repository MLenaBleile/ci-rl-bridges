# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)

set.seed(1971)

# Create a simulation of action-value updates for two treatments
n_trials <- 100
true_values <- c(treatment1 = 7.0, treatment2 = 5.0)  # True Qvalues

# Simulate rewards with noise
rewards <- data.frame(
  trial = 1:n_trials,
  treatment1 = rnorm(n_trials, mean = true_values["treatment1"], sd = 2),
  treatment2 = rnorm(n_trials, mean = true_values["treatment2"], sd = 2)
)

# Calculate running averages (Q-value estimates)
action_values <- rewards %>%
  mutate(
    q1 = cumsum(treatment1) / 1:n_trials,
    q2 = cumsum(treatment2) / 1:n_trials,
    n1 = 1:n_trials,
    n2 = 1:n_trials
  )

# Create a data frame for the epsilon-greedy visualization
epsilon_data <- data.frame(
  trial = 1:n_trials,
  epsilon = pmax(0.1, 1 - (1:n_trials) / 50)  # Decaying epsilon
)

# Combine data for plotting
combined_data <- action_values %>%
  left_join(epsilon_data, by = "trial")

# Create combined plot with dual y-axes
p_combined <- ggplot(combined_data, aes(x = trial)) +
  # True values as horizontal lines
  geom_hline(yintercept = true_values["treatment1"], 
             color = "black", linetype = "solid", size = 1, alpha = 0.7) +
  geom_hline(yintercept = true_values["treatment2"], 
             color = "black", linetype = "solid", size = 1, alpha = 0.7) +
  
  # Q value estimates
  geom_line(aes(y = q1), color = "black", size = 1.5, linetype = "dashed") +
  geom_line(aes(y = q2), color = "black", size = 1.5, linetype = "twodash") +
  
  # Individual rewards as points
  geom_point(aes(y = treatment1), color = "black", alpha = 0.3, size = 2, shape = 16) +
  geom_point(aes(y = treatment2), color = "black", alpha = 0.3, size = 2, shape = 17) +
  
  # Epsilon line on secondary axis (scaled to match primary axis range)
  geom_line(aes(y = epsilon * 10), color = "black", size = 1.5, linetype = "dotted") +
  
  # Annotations
  annotate("text", x = 90, y = true_values["treatment1"] + 0.3, 
           label = "E[R|a=1]", color = "black", fontface = "bold") +
  annotate("text", x = 90, y = true_values["treatment2"] - 0.5, 
           label = "E[R|a=2]", color = "black", fontface = "bold") +
  
  # Legend for line types
  annotate("text", x = 10, y = 11, label = "Treatment 1", color = "black", hjust = 0) +
  annotate("segment", x = 2, xend = 8, y = 11, yend = 11, 
           color = "black", size = 1, linetype = "dashed") +
  annotate("text", x = 10, y = 10.5, label = "Treatment 2", color = "black", hjust = 0) +
  annotate("segment", x = 2, xend = 8, y = 10.5, yend = 10.5, 
           color = "black", size = 1, linetype = "twodash") +
  annotate("text", x = 10, y = 10, label = "epsilon", color = "black", hjust = 0) +
  annotate("segment", x = 2, xend = 8, y = 10, yend = 10, 
           color = "black", size = 1, linetype = "dotted") +
  
  # Main plot setup
  scale_y_continuous(
    name = "Outcome Estimate",
    sec.axis = sec_axis(~ . / 10, name = "epsilon (Exploration Probability)")
  ) +
  labs(#title = "Action-Value Methods in Reinforcement Learning",
       #subtitle = "Outcome estimates converge to true values as exploration parameter decays",
       x = "Trial") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.title.y.right = element_text(color = "black"))

print(p_combined)
# Save the combined plot
ggsave("action_value_updates_combined.png", p_combined, 
       width = 8, height = 6, dpi = 300)