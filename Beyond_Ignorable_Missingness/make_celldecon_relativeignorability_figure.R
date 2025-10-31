library(ggplot2)
library(patchwork)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(cowplot)
library(dplyr)

# Define our color palette for consistency
treatment_color <- "skyblue"  # Dark purple for treatment
placebo_color <- "violet"    # Steel blue for placebo
model1_color <- "#800080"     # Purple
model2_color <- "violetred"     # Medium purple

# Create simple data frame for the model color bars
model_color_data <- data.frame(
  Model = c("Model 1\n(High RMSE)", "Model 2\n(Low RMSE)"),
  Color = c("#800080", "violetred"),  # Purple for Model 1, Violetred for Model 2
  xmin = c(0.5, 0.5),  # Span across both bars
  xmax = c(2.5, 2.5),
  y = c(-0.5, -0.5),
  height = c(0.3, 0.3)
)

# Create the simulated data for the bar charts
data_model1 <- data.frame(
  Group = c("Placebo", "Treatment"),
  Value = c(4, 2),
  Model = "Model 1\n(High RMSE)",
  Effect = "Est. Treatment Effect: -2"
)

data_model2 <- data.frame(
  Group = c("Placebo", "Treatment"),
  Value = c(8, 9),
  Model = "Model 2\n(Low RMSE)",
  Effect = "Est. Treatment Effect: +1"
)

data_truth <- data.frame(
  Group = c("Placebo", "Treatment"),
  Value = c(9, 7),
  Model = "Truth",
  Effect = "Treatment Effect: -2"
)

combined_data <- rbind(data_model1, data_model2, data_truth)
combined_data$Model <- factor(combined_data$Model, 
                              levels = c("Model 1\n(High RMSE)", "Model 2\n(Low RMSE)", "Truth"))
combined_data$Group <- factor(combined_data$Group, levels = c("Placebo", "Treatment"))


bars_plot <- ggplot(combined_data, aes(x = Group, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~Model, scales = "free", nrow = 1) +
  scale_fill_manual(values = c("Placebo" = placebo_color, "Treatment" = treatment_color)) +
  

  geom_rect(data = model_color_data, 
            aes(xmin = xmin, xmax = xmax, ymin = y, ymax = y + height),
            fill = model_color_data$Color,  # Directly provide fill colors
            inherit.aes = FALSE) +
  
  labs(
    title = "Relative Ignorability in Cell Deconvolution",
    subtitle = "Comparison of Treatment Effects Across Models",
    y = "Neutrophils (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "white", color = "gray80"),
    strip.text = element_text(face = "bold")
  ) +
  # Set y axis limits to make room for the color bars
  coord_cartesian(ylim = c(-1, max(combined_data$Value) * 1.1))

# Add effect size annotations
bars_plot <- bars_plot + 
  geom_text(data = combined_data %>% group_by(Model) %>% summarise(Effect = first(Effect)),
            aes(label = Effect, x = 1.5), y = max(combined_data$Value) * 1.05, 
            size = 3.5, inherit.aes = FALSE)
# Create DAGs using DiagrammeR
# Define DAG content for Model 1
dag1 <- grViz("
  digraph dag1 {
    graph [layout = dot, rankdir = LR]
    
    # Define node styles
    node [shape = rectangle, style = filled, fontname = 'Helvetica', fontsize = 12]
    
    # Define nodes
    Treatment [label = 'Treatment', fillcolor = 'skyblue', fontcolor = 'black']
    Accuracy [label = 'Deconvolution\naccuracy\n(70%)', fillcolor = '#800080', fontcolor = 'white']
    Neutrophil [label = 'Neutrophil\nproportion', fillcolor = 'darkgrey', fontcolor = 'white']
    
    # Define edges
    Treatment -> Neutrophil
    Accuracy -> Neutrophil
  }
")

# Define DAG content for Model 2
dag2 <- grViz("
  digraph dag2 {
    graph [layout = dot, rankdir = LR]
    
    # Define node styles
    node [shape = rectangle, style = filled, fontname = 'Helvetica', fontsize = 12]
    
    # Define nodes
    Treatment [label = 'Treatment', fillcolor = 'skyblue', fontcolor = 'black']
    Accuracy [label = 'Deconvolution\naccuracy\n(90%)', fillcolor = 'violetred', fontcolor = 'white']
    Neutrophil [label = 'Neutrophil\nproportion', fillcolor = 'black', fontcolor = 'white']
    
    # Define edges
    Treatment -> Accuracy
    Accuracy -> Neutrophil
    Treatment -> Neutrophil
  }
")

# Convert DiagrammeR graphs to images we can use
# Save the DAGs as temporary PNG files
dag1_file <- tempfile(fileext = ".png")
dag2_file <- tempfile(fileext = ".png")

DiagrammeRsvg::export_svg(dag1) %>%
  charToRaw() %>%
  rsvg::rsvg_png(file = dag1_file, width = 400, height = 200)

DiagrammeRsvg::export_svg(dag2) %>%
  charToRaw() %>%
  rsvg::rsvg_png(file = dag2_file, width = 400, height = 200)

# Create annotation text
model1_text <- ggdraw() + 
  draw_label("Model 1\nDeconvolution accuracy is \nrelatively ignorable \nwith respect to treatment", 
             fontface = "bold", color = model1_color, size = 11, hjust = 0)

model2_text <- ggdraw() + 
  draw_label("Model 2\nDeconvolution accuracy confounds\ntreatment effects", 
             fontface = "bold", color = model2_color, size = 11, hjust = 0)

# Load the DAG images
dag1_img <- ggdraw() + draw_image(dag1_file, scale = 0.9)
dag2_img <- ggdraw() + draw_image(dag2_file, scale = 0.9)

# Combine the DAGs with their annotations
dag1_panel <- plot_grid(dag1_img, model1_text, ncol = 1, rel_heights = c(3, 1))
dag2_panel <- plot_grid(dag2_img, model2_text, ncol = 1, rel_heights = c(3, 1))

# Combine the DAG panels
dags_combined <- plot_grid(dag2_panel, dag1_panel, ncol = 1)

# Add the title
# title <- ggdraw() + 
#   draw_label("Correct estimation of treatment effects is the goal", 
#              fontface = "bold", size = 20, hjust = 1)

# Final layout
final_plot <- plot_grid(
  plot_grid(bars_plot, dags_combined, rel_widths = c(3, 2), ncol = 2),
  ncol = 1, rel_heights = c(0.15, 0.85)
)

# Save the output
ggsave("celldecon_relative_ignorability.png", final_plot, width = 12, height = 8, dpi = 300)

# Display the plot
print(final_plot)
