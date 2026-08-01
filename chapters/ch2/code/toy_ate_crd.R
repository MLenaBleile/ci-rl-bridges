## Chapter 1
##Average Treatment Effect and Causal Risk Difference Example

# Incomplete potential outcomes data
df <- data.frame(
  Subject = c("Julius", "Octavian", "Claudius", "Augustus"),
  Treatment = c("a1", "a1", "a2", "a2"),
  R_a1 = c(20, 25, NA, NA),  # Observed only for a1 group
  R_a2 = c(NA, NA, 50, 45)   # Observed only for a2 group
)

# Impute missing values using group-wise means
mean_r_a1 <- mean(df$R_a1, na.rm = TRUE)  # mean for those who received a1
mean_r_a2 <- mean(df$R_a2, na.rm = TRUE)  # mean for those who received a2

df$R_a1[is.na(df$R_a1)] <- mean_r_a1
df$R_a2[is.na(df$R_a2)] <- mean_r_a2

# Compute causal risk difference for each subject
df$CRD <- df$R_a2 - df$R_a1

# Estimate ATE
ate <- mean(df$CRD)

# Output
print(df)
cat("\nEstimated ATE (R_a2 - R_a1):", round(ate, 2), "\n")
