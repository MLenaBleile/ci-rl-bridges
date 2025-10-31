
setwd("~/Optimal Control with Causal Agents/Chapter2")
library(ggplot2)

# Create sample data 
patient_number <- 0:30

# Treatment A probabilities (increasing after adaptive phase)
prob_A <- c(rep(0.5, 11),  # Equal allocation before adaptive
            0.52, 0.56, 0.62, 0.68, 0.75, 0.82, 0.88, 0.92, 
            0.94, 0.95, 0.96, 0.96, 0.96, 0.96, 0.96, 0.96, 
            0.96, 0.96, 0.96, 0.96)

# Treatment B probabilities (mirror of Treatment A)
prob_B <- 1 - prob_A

# Combine into data frame
df <- data.frame(
  Patient = rep(patient_number, 2),
  Probability = c(prob_A, prob_B),
  Treatment = rep(c("Treatment A", "Treatment B"), each = length(patient_number))
)

# Create plot
p=ggplot(df, aes(x = Patient, y = Probability, linetype = Treatment)) +
  geom_line(linewidth = 0.8, color = "black") +
  geom_vline(xintercept = 10, linetype = "dashed", color = "black", linewidth = 0.5) +
  annotate("text", x = 10, y = 0.25, label = "Beginning Adaptive\nAssignment", 
           hjust = -0.1, size = 3.5, color = "black") +
  scale_linetype_manual(values = c("Treatment A" = "solid", "Treatment B" = "longdash"),
                        guide = guide_legend(override.aes = list(linewidth = 0.8))) +
  labs(x = "Patient Number", y = "Probability of treatment assignment") +
  theme_classic() +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.85, 0.9),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )

ggsave("adaptivetrial.png")



tb <- dplyr::tibble(ids = 1:50, score = seq(1,100,2))
