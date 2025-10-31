library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Set seed for reproducibility
set.seed(456)

# Define sectors and stocks
sectors <- c(rep("Pharma", 5), rep("Tech", 5), rep("Energy", 5), rep("Financial", 5))
stocks <- paste0(
  rep(c("PH", "TC", "EN", "FN"), each = 5),
  1:5
)

n_stocks <- length(stocks)

# True volatility values
# Baseline volatility for each sector
base_vol <- c(
  rep(0.20, 5),  # Pharma baseline
  rep(0.25, 5),  # Tech baseline
  rep(0.18, 5),  # Energy baseline 
  rep(0.22, 5)   # Financial baseline
)

# The India offshoring effect increases pharma volatility significantly
india_effect <- c(
  rep(0.20, 5),  # Strong effect on Pharma
  rep(0, 15)     # No effect on other sectors
)

# True volatility includes the India effect
true_vol <- base_vol + india_effect

# Model A: Overestimates political polarization effect across the board,
# but correctly captures the pharma/India effect
political_overestimation <- 0.15  # Overestimation across all sectors
model_a_vol <- base_vol + political_overestimation + india_effect

# Model B: Correctly estimates base volatility, but misses the India effect
model_b_vol <- base_vol

# Create the data frame
stock_data <- data.frame(
  stock = stocks,
  sector = sectors,
  true_vol = true_vol,
  model_a_vol = model_a_vol,
  model_b_vol = model_b_vol
)

# Calculate squared errors for each model
stock_data$model_a_se <- (stock_data$model_a_vol - stock_data$true_vol)^2
stock_data$model_b_se <- (stock_data$model_b_vol - stock_data$true_vol)^2

# Calculate MSE for each model
model_a_mse <- mean(stock_data$model_a_se)
model_b_mse <- mean(stock_data$model_b_se)

# Ensure Model B has lower MSE (adjust parameters if needed)
print(paste("Model A MSE:", round(model_a_mse, 4)))
print(paste("Model B MSE:", round(model_b_mse, 4)))

# Calculate resource allocation decisions based on volatility ranking
# Function to calculate optimal capital allocation based on volatility
# Higher volatility gets more capital (within constraints)
calculate_allocation <- function(volatility) {
  # Normalize to sum to 100%
  normalized <- volatility / sum(volatility)
  # Scale to get reasonable allocation percentages
  allocation <- normalized * 100
  return(allocation)
}

# Calculate optimal allocations
stock_data$true_allocation <- calculate_allocation(stock_data$true_vol)
stock_data$model_a_allocation <- calculate_allocation(stock_data$model_a_vol)
stock_data$model_b_allocation <- calculate_allocation(stock_data$model_b_vol)

# Calculate allocation errors
stock_data$model_a_alloc_error <- abs(stock_data$model_a_allocation - stock_data$true_allocation)
stock_data$model_b_alloc_error <- abs(stock_data$model_b_allocation - stock_data$true_allocation)

# Calculate total allocation error
model_a_alloc_error <- sum(stock_data$model_a_alloc_error)
model_b_alloc_error <- sum(stock_data$model_b_alloc_error)

print(paste("Model A Allocation Error:", round(model_a_alloc_error, 2)))
print(paste("Model B Allocation Error:", round(model_b_alloc_error, 2)))

# Reshape data for plotting
vol_data <- stock_data %>%
  select(stock, sector, true_vol, model_a_vol, model_b_vol) %>%
  pivot_longer(cols = c(true_vol, model_a_vol, model_b_vol),
               names_to = "model", values_to = "volatility")

allocation_data <- stock_data %>%
  select(stock, sector, true_allocation, model_a_allocation, model_b_allocation) %>%
  pivot_longer(cols = c(true_allocation, model_a_allocation, model_b_allocation),
               names_to = "model", values_to = "allocation")

# Order by sector and true volatility for better visualization
ordered_stocks <- stock_data %>%
  arrange(sector, desc(true_vol)) %>%
  pull(stock)

vol_data$stock <- factor(vol_data$stock, levels = ordered_stocks)
allocation_data$stock <- factor(allocation_data$stock, levels = ordered_stocks)
vol_data$sector <- factor(vol_data$sector, levels = c("Pharma", "Tech", "Energy", "Financial"))
allocation_data$sector <- factor(allocation_data$sector, levels = c("Pharma", "Tech", "Energy", "Financial"))

# Create better labels
vol_data$model <- factor(vol_data$model, 
                         levels = c("true_vol", "model_a_vol", "model_b_vol"),
                         labels = c("True Volatility", 
                                    "Model A (Political + Pharma)", 
                                    "Model B (Base Only)"))

allocation_data$model <- factor(allocation_data$model, 
                                levels = c("true_allocation", "model_a_allocation", "model_b_allocation"),
                                labels = c("True Optimal Allocation", 
                                           "Model A Allocation", 
                                           "Model B Allocation"))

# Plot colors
true_color <- "#800080"  # Purple
model_a_color <- "#9370DB"  # Light purple
model_b_color <- "#333333"  # Dark gray

# Create volatility estimation plot
vol_plot <- ggplot(vol_data, aes(x = stock, y = volatility, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  facet_grid(. ~ sector, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = c("True Volatility" = true_color, 
                               "Model A (Political + Pharma)" = model_a_color, 
                               "Model B (Base Only)" = model_b_color)) +
  labs(title = "Volatility Estimates by Sector",
       subtitle = paste("MSE: Model A =", round(model_a_mse, 4), 
                        ", Model B =", round(model_b_mse, 4), 
                        " (lower is better)"),
       x = "Stock",
       y = "Volatility",
       fill = "Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

# Create allocation plot
alloc_plot <- ggplot(allocation_data, aes(x = stock, y = allocation, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  facet_grid(. ~ sector, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = c("True Optimal Allocation" = true_color, 
                               "Model A Allocation" = model_a_color, 
                               "Model B Allocation" = model_b_color)) +
  labs(title = "Capital Allocation by Sector",
       subtitle = paste("Allocation Error: Model A =", round(model_a_alloc_error, 2), 
                        ", Model B =", round(model_b_alloc_error, 2), 
                        " (lower is better)"),
       x = "Stock",
       y = "Capital Allocation (%)",
       fill = "Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

# Combined plot
combined_plot <- vol_plot / alloc_plot +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Relative Ignorability: Political Event and Pharma Offshoring Example",
    subtitle = "Model A has higher MSE but leads to better capital allocation decisions",
    caption = "Note: Model A correctly captures the pharma sector volatility spike despite overestimating overall market volatility"
  )

# Display plot
print(combined_plot)

# Save the plot
ggsave("political_pharma_simulation.png", combined_plot, width = 12, height = 10, dpi = 300)