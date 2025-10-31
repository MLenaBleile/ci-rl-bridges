# Figure 3.3: Value Function and Q-Function Visualization
# Create a value function heat map for a simple MDP
library(dplyr)
library(ggplot2)

# Generate a simple state value function
states <- expand.grid(x = 1:5, y = 1:5)
states$state_idx <- 1:nrow(states)

# Define reward function: high in top right, low in bottom left, zero elsewhere
states$value <- with(states, {
  case_when(
    x == 5 & y == 5 ~ 100,    # Target: top right (high reward)
    x == 1 & y == 1 ~ -100,   # Trap: bottom left (penalty)
    TRUE ~ 0                   # All other states: zero reward
  )
})

# Plot the value function heatmap
fig_value_function <- ggplot(states, aes(x = x, y = y, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen", 
                       midpoint = 0, name = "Value") +
  coord_fixed() +
  labs(title = "Figure 3.3(a): State Value Function",
       subtitle = "Target (green) in top right, Trap (red) in bottom left",
       x = "State Dimension 1", y = "State Dimension 2") +
  theme(panel.grid = element_blank())

# Q-function visualization (state-action values)
# Create actions: up, right, down, left
directions <- data.frame(
  action = c("up", "right", "down", "left"),
  dx = c(0, 1, 0, -1),
  dy = c(1, 0, -1, 0)
)

# Generate Q-values based on distance to target from next state
q_values <- states %>%
  merge(directions) %>%
  mutate(
    # Calculate next state position
    next_x = pmin(pmax(x + dx, 1), 5),  # Stay within bounds
    next_y = pmin(pmax(y + dy, 1), 5),
    
    # Calculate distance to target from next state
    distance_to_target = sqrt((5 - next_x)^2 + (5 - next_y)^2),
    
    # Q-value is negative distance (closer to target = higher Q-value)
    # Add bonus for actions that move toward target
    q_value = -distance_to_target * 10 + case_when(
      action == "right" & x < 5 ~ 5,   # Bonus for moving right when not at right edge
      action == "up" & y < 5 ~ 5,      # Bonus for moving up when not at top edge
      action == "left" ~ -20,           # Penalty for moving left (toward trap)
      action == "down" ~ -20,           # Penalty for moving down (toward trap)
      TRUE ~ 0
    )
  )

# Get best action for each state
best_actions <- q_values %>%
  group_by(x, y) %>%
  slice(which.max(q_value)) %>%
  ungroup()

# Create arrows for best actions
arrows_data <- best_actions %>%
  mutate(
    xend = x + 0.4 * dx,
    yend = y + 0.4 * dy
  )

# Plot Q-function with best actions
fig_q_function <- ggplot(states, aes(x = x, y = y)) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradient2(low = "white", mid = "grey", high = "black", 
                       midpoint = 0, name = "Value") +
  geom_segment(data = arrows_data, 
               aes(xend = xend, yend = yend),
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "blue", size = 1) +
  coord_fixed() +
  labs(title = "5x5 Grid World",
       subtitle = "Trap at Bottom Left, Goal at Top Right",
       x = "State Dimension 1", y = "State Dimension 2") +
  theme(panel.grid = element_blank())

# Display plots
print(fig_value_function)
print(fig_q_function)