# Load required libraries
library(ggplot2)
library(ggrepel)
library(dplyr)

# Create data frame of Roman city coordinates (approximate historical locations)
cities <- data.frame(
  name = c("Roma", "Neapolis", "Alexandria", "Carthago", "Lugdunum", "Byzantium", "Antiochia"),
  x = c(12.5, 14.3, 29.9, 10.3, 4.8, 28.9, 36.2),
  y = c(41.9, 40.8, 31.2, 36.8, 45.8, 41.0, 36.2),
  description = c("Rome: Capital of the Empire", 
                  "Naples: Important port city", 
                  "Alexandria: Cultural and trade hub in Egypt", 
                  "Carthage: Key North African city", 
                  "Lyon: Important city in Gaul (France)", 
                  "Byzantium (later Constantinople): Strategic eastern city", 
                  "Antioch: Major eastern capital in Syria")
)

# Create routes data frame
# Path 1: Optimal path Roma -> Lugdunum -> Byzantium -> Alexandria
# Path 2: Alternative path Roma -> Neapolis -> Carthago -> Alexandria
# Path 3: Suboptimal path Lugdunum -> Carthago -> Antiochia -> Alexandria
routes <- data.frame(
  path = c(rep("Optimal Full Path", 4), 
           rep("Alternative Full Path", 4),
           rep("Suboptimal Segment", 4)),
  from = c("Roma", "Lugdunum", "Byzantium", "Alexandria",
           "Roma", "Neapolis", "Carthago", "Alexandria",
           "Lugdunum", "Carthago", "Antiochia", "Alexandria"),
  to = c("Lugdunum", "Byzantium", "Alexandria", NA,
         "Neapolis", "Carthago", "Alexandria", NA,
         "Carthago", "Antiochia", "Alexandria", NA),
  segment = c(1, 2, 3, NA,
              1, 2, 3, NA, 
              1, 2, 3, NA)
)

# Create edges for plotting
edges <- routes %>%
  filter(!is.na(to)) %>%
  left_join(cities, by = c("from" = "name")) %>%
  rename(x1 = x, y1 = y) %>%
  left_join(cities, by = c("to" = "name")) %>%
  rename(x2 = x, y2 = y)

# Create route colors and linetypes
edges$color <- factor(edges$path, 
                      levels = c("Optimal Full Path", "Alternative Full Path", "Suboptimal Segment"))
edges$linetype <- ifelse(edges$path == "Optimal Full Path", "solid", 
                         ifelse(edges$path == "Alternative Full Path", "dotted", "dashed"))
edges$size <- ifelse(edges$path == "Optimal Full Path", 1.2, 0.8)

# Select key cities to highlight the optimality principle
key_cities <- c("Roma", "Lugdunum", "Alexandria")

# Mediterranean Sea polygon (simplified)
mediterranean <- data.frame(
  x = c(0, 0, 5, 10, 15, 20, 25, 30, 35, 40, 40, 35, 30, 25, 20, 15, 10, 5, 0),
  y = c(30, 35, 37, 39, 40, 40, 38, 36, 35, 34, 30, 32, 33, 32, 33, 34, 35, 33, 30)
)

# Plot
ggplot() +
  # Add map background (Mediterranean)
  geom_polygon(data = mediterranean, aes(x = x, y = y), fill = "lightblue", alpha = 0.5) +
  
  # Add edges
  geom_segment(data = edges, 
               aes(x = x1, y = y1, xend = x2, yend = y2, 
                   color = color, linetype = linetype, size = size),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  
  # Add cities
  geom_point(data = cities, aes(x = x, y = y), size = 4, color = "darkgray") +
  geom_point(data = filter(cities, name %in% key_cities), 
             aes(x = x, y = y), size = 5, color = "red") +
  
  # Add city labels
  geom_text_repel(data = cities, aes(x = x, y = y, label = name), 
                  size = 3.5, box.padding = 0.5, point.padding = 0.3) +
  
  # Customize appearance
  scale_color_manual(values = c("Optimal Full Path" = "blue", 
                                "Alternative Full Path" = "darkgray", 
                                "Suboptimal Segment" = "red")) +
  scale_linetype_identity() +
  scale_size_identity() +
  
  # Add title and annotations
  labs(title = "Principle of Optimality Example",
       subtitle = "If Roma→Lugdunum→Alexandria is optimal, then Lugdunum→Alexandria segment must also be optimal",
       caption = "Illustration of Bellman's Principle of Optimality using minimal travel time between cities",
       color = "Path Type") +
  
  # Add annotations explaining the principle
  annotate("text", x = 20, y = 45, 
           label = "Principle of Optimality:\nIf subpath Lugdunum→Alexandria were not optimal,\nwe could improve the full Roma→Lugdunum→Alexandria path", 
           size = 3.5, hjust = 0) +
  
  # Customize theme
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Save the plot if needed
ggsave("RL book/Figures/roman_empire_optimal_path.png", width = 10, height = 8)
