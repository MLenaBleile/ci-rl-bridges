# Load necessary libraries
library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)
setwd("C:/Users/User/Documents/Optimal Control with Causal Agents/Chapter1")
# Set up theme for consistent styling
theme_set(theme_minimal(base_size = 14))
custom_theme <- theme(
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  legend.position = "none"
)

# Create a data frame for the potential outcomes visualization
potential_outcomes <- data.frame(
  x = c(1.5, 1, 2),
  y = c(1, 0, 0),
  label = c("Subject j", "Treatment a(1)", "Treatment a(2)"),
  node_type = c("subject", "treatment", "treatment")
)

# Create a data frame for the arrows
arrows <- data.frame(
  x = c(1.5, 1.5),
  y = c(0.7, 0.7),
  xend = c(1, 2),
  yend = c(0.1, 0.1),
  arrow_type = c("solid", "dashed")
)

# Create a data frame for outcome nodes
outcomes <- data.frame(
  x = c(1, 2),
  y = c(-1, -1),
  label = c("r|a(1) = 20", "r|a(2) = ?"),
  node_type = c("observed", "counterfactual")
)

# Create a data frame for outcome arrows
outcome_arrows <- data.frame(
  x = c(1, 2),
  y = c(-0.1, -0.1),
  xend = c(1, 2),
  yend = c(-0.9, -0.9),
  arrow_type = c("solid", "dashed")
)

# Create the basic plot
p <- ggplot() +
  # Plot nodes
  # geom_point(data = potential_outcomes, 
  #            aes(x = x, y = y, color = node_type), 
  #            size = 20, alpha = 0.7) +
  
  # Add node labels
  geom_text(data = potential_outcomes, 
            aes(x = x, y = y, label = label),
            fontface = "bold") +
  
  # Add arrows between nodes
  geom_segment(data = arrows, 
               aes(x = x, y = y, xend = xend, yend = yend, 
                   linetype = arrow_type),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               size = 1) +
  
  # Add outcome nodes
  # geom_point(data = outcomes, 
  #            aes(x = x, y = y, color = node_type), 
  #            size = 20, alpha = 0.7) +
  
  # Add outcome labels
  geom_text(data = outcomes, 
            aes(x = x, y = y, label = label),
            fontface = "bold") +
  
  # Add outcome arrows
  geom_segment(data = outcome_arrows, 
               aes(x = x, y = y, xend = xend, yend = yend, 
                   linetype = arrow_type),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               size = 1) +
  
  # Add explanatory annotations
  annotate("text", x = 2.2, y = -1, 
           label = "Counterfactual\n(unobserved)", 
           hjust = 0, fontface = "italic") +
  annotate("text", x = 0.2, y = -1, 
           label = "Factual\n(observed)", 
           hjust = 0, fontface = "italic") +
  
  # Set colors
  scale_color_manual(values = c("subject" = "#3498db", 
                                "treatment" = "#e74c3c",
                                "counterfactual" = "#7f8c8d",
                                "observed" = "#2ecc71")) +
  
  # Set line types
  scale_linetype_manual(values = c("solid" = "solid", "dashed" = "dashed")) +
  
  # Add figure title
  labs(title = "Potential Outcomes Framework",
       subtitle = "The fundamental problem of causal inference: We can observe only one potential outcome") +
  
  # Set plot bounds and aspect ratio
  coord_cartesian(xlim = c(-0.5, 4), ylim = c(-1.5, 1.5)) +
  theme_minimal()+
  # Apply custom theme
  custom_theme +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Save the plot
ggsave("potential_outcomes_framework.png", p, width = 10, height = 6, dpi = 300)

# Display the plot
print(p)