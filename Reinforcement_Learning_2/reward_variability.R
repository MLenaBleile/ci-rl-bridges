library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# Set seed for reproducibility
set.seed(42)

# Create data frame for MDP (fully observed)
mdp_data <- data.frame(
  Category = rep(c("State Features", "Action Selection", "Transition Dynamics", 
                   "Environmental Stochasticity", "Measurement Error"), 2),
  Type = c(rep("Observable", 5), rep("Unobservable", 5)),
  Value = c(30, 25, 20, 15, 10,  # Observable components in MDP
            0, 0, 0, 0, 0)      # Unobservable components in MDP (none)
)

# Create data frame for POMDP (partially observed)
pomdp_data <- data.frame(
  Category = rep(c("State Features", "Action Selection", "Transition Dynamics", 
                   "Environmental Stochasticity", "Measurement Error"), 2),
  Type = c(rep("Observable", 5), rep("Unobservable", 5)),
  Value = c(15, 25, 10, 5, 10,           # Observable components in POMDP
            15, 0, 10, 10, 0)            # Unobservable components in POMDP
)

# Combine data frames
mdp_data$Model <- "MDP (Fully Observed)"
pomdp_data$Model <- "POMDP (Partially Observed)"
plot_data <- rbind(mdp_data, pomdp_data)

# Convert to long format
plot_data_long <- plot_data %>%
  group_by(Model) %>%
  mutate(TotalValue = sum(Value)) %>%
  ungroup()

# Create the plot
p <- ggplot(plot_data_long, aes(x = Model, y = Value, fill = interaction(Category, Type, sep = " - "))) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.2) +
  scale_fill_manual(values = c(
    "State Features - Observable" = "#66c2a5",
    "State Features - Unobservable" = "#66c2a5",
    "Action Selection - Observable" = "#fc8d62",
    "Action Selection - Unobservable" = "#fc8d62",
    "Transition Dynamics - Observable" = "#8da0cb",
    "Transition Dynamics - Unobservable" = "#8da0cb",
    "Environmental Stochasticity - Observable" = "#e78ac3",
    "Environmental Stochasticity - Unobservable" = "#e78ac3",
    "Measurement Error - Observable" = "#a6d854",
    "Measurement Error - Unobservable" = "#a6d854"
  ), 
  breaks = c(
    "State Features - Observable",
    "State Features - Unobservable",
    "Action Selection - Observable",
    "Action Selection - Unobservable",
    "Transition Dynamics - Observable",
    "Transition Dynamics - Unobservable",
    "Environmental Stochasticity - Observable",
    "Environmental Stochasticity - Unobservable",
    "Measurement Error - Observable",
    "Measurement Error - Unobservable"
  )) +
  labs(
    title = "Reward Variability Partitioning: MDP vs POMDP",
    x = "",
    y = "Proportion of Reward Variability",
    fill = "Causal Factor"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )

# Create a second plot with patterns for observable/unobservable
p2 <- ggplot(plot_data, aes(x = Model, y = Value, fill = Category, alpha = Type)) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.2) +
  scale_fill_brewer(palette = "Set2") +
  scale_alpha_manual(values = c("Observable" = 1, "Unobservable" = 0.4)) +
  labs(
    title = "Reward Variability Partitioning: MDP vs POMDP",
    x = "",
    y = "Proportion of Reward Variability",
    fill = "Causal Factor",
    alpha = "Observability"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )

# Add text annotations to explain key points
p2 <- p2 + 
  annotate("text", x = 1, y = 110, 
           label = "All causal factors\nfully observable", 
           size = 3.5) +
  annotate("text", x = 2, y = 110, 
           label = "Some causal factors\npartially or entirely unobservable", 
           size = 3.5) +
  annotate("segment", x = 2, y = 105, xend = 2, yend = 85, 
           arrow = arrow(length = unit(0.2, "cm")), size = 0.5) +
  coord_cartesian(ylim = c(0, 120))

# Print the plot
print(p2)

# Create an alternative visualization with hatching for observability
# This requires the ggpattern package
if(require(ggpattern)) {
  p3 <- ggplot(plot_data, aes(x = Model, y = Value, fill = Category, pattern = Type)) +
    geom_bar_pattern(stat = "identity", position = "stack", 
                     color = "black", linewidth = 0.2,
                     pattern_color = "black", pattern_fill = "black",
                     pattern_angle = 45, pattern_density = 0.1, 
                     pattern_spacing = 0.025, pattern_key_scale_factor = 0.6) +
    scale_fill_brewer(palette = "Set2") +
    scale_pattern_manual(values = c("Observable" = "none", "Unobservable" = "stripe")) +
    labs(
      title = "Reward Variability Partitioning: MDP vs POMDP",
      x = "",
      y = "Proportion of Reward Variability",
      fill = "Causal Factor",
      pattern = "Observability"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12)
    )
  
  print(p3)
}

# To save the plot
ggsave("causal_partitioning.png", p2, width = 10, height = 7, dpi = 300)