#Create simple data frame with our example
asthma_data <- data.frame(
  treatment = rep(c("a1", "a1", "a2", "a2"),2),
  outcome = c(c(20, 25, 50, 45),c(20, 25, 50+10, 45+10)+5),
  smoking_status = c(rep("S",4),rep("NS",4))
)

# Fit linear model
model <- lm(outcome ~ treatment, data = asthma_data)
summary(model)

model_interaction <- lm(outcome ~ treatment * smoking_status, 
                        data = asthma_data)

summary(model_interaction)
