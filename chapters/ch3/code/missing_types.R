#library(knitr)
#library(kableExtra)

set.seed(1998)
n <- 1000

df_oracle <- data.frame(
  Subject = 1:n,
  Treatment = sample(c("a1", "a2"), n, replace = TRUE),
  Smoking_Status = sample(c("Smoker", "Non-smoker"), n, 
                          replace = TRUE, prob = c(0.4, 0.6))
)


df_oracle$True_Outcome <- with(df_oracle,
                               ifelse(Treatment == "a1",
                                      ifelse(Smoking_Status == "Smoker", 20, 22),
                                      ifelse(Smoking_Status == "Smoker", 25, 55))
) + rnorm(n, 0, 3)

# MCAR
df_mcar <- df_oracle
missing_mcar <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.3, 0.7))
df_mcar$Smoking_Status[missing_mcar] <- NA


compute_cate <- function(data) {
  data_complete <- data[!is.na(data$Smoking_Status), ]
  
  ate_smokers <- mean(data_complete$True_Outcome[
    data_complete$Treatment == "a2" & data_complete$Smoking_Status == "Smoker"]) -
    mean(data_complete$True_Outcome[
      data_complete$Treatment == "a1" & data_complete$Smoking_Status == "Smoker"])
  
  ate_nonsmokers <- mean(data_complete$True_Outcome[
    data_complete$Treatment == "a2" & data_complete$Smoking_Status == "Non-smoker"]) -
    mean(data_complete$True_Outcome[
      data_complete$Treatment == "a1" & data_complete$Smoking_Status == "Non-smoker"])
  
  return(c(smokers = ate_smokers, nonsmokers = ate_nonsmokers))
}


true_cate <- compute_cate(df_oracle)


cate_mcar <- compute_cate(df_mcar)
result_mat = matrix(c(true_cate,cate_mcar), nrow=2,byrow=T, dimnames=list(c("True","MCAR"),names(true_cate)))
result_mat

# MAR
df_mar <- df_oracle
missing_mar <- ifelse(df_mar$Treatment == "a2",
                      sample(c(TRUE, FALSE), sum(df_mar$Treatment == "a2"),
                             replace = TRUE, prob = c(0.6, 0.4)),
                      sample(c(TRUE, FALSE), sum(df_mar$Treatment == "a1"),
                             replace = TRUE, prob = c(0.1, 0.9)))
df_mar$Smoking_Status[missing_mar] <- NA
cate_mar <- compute_cate(df_mar)

result_mat = matrix(c(true_cate,cate_mar), nrow=2,byrow=T, dimnames=list(c("True","MCAR"),names(true_cate)))
result_mat

# MNAR: missingness depends on the outcome itself: patients doing poorly
# are less likely to report their smoking status. Because deletion depends on
# the outcome even within treatment-by-smoking cells, the retained rows are no
# longer a random subsample of each cell, and the CATE estimates become biased.
df_mnar <- df_oracle
p_miss <- plogis(-1 + 0.25 * (30 - df_mnar$True_Outcome))
missing_mnar <- runif(n) < p_miss
df_mnar$Smoking_Status[missing_mnar] <- NA
cate_mnar <- compute_cate(df_mnar)
result_mat = matrix(c(true_cate,cate_mnar), nrow=2,byrow=T, dimnames=list(c("True","MCAR"),names(true_cate)))
result_mat



# Create results table
results <- data.frame(
  Subgroup = c("Smokers", "Non-smokers"),
  Truth = round(true_cate, 2),
  MCAR = round(cate_mcar, 2),
  `MCAR Bias` = round(cate_mcar - true_cate, 2),
  MAR = round(cate_mar, 2),
  `MAR Bias` = round(cate_mar - true_cate, 2),
  MNAR = round(cate_mnar, 2),
  `MNAR Bias` = round(cate_mnar - true_cate, 2),
  check.names = FALSE
)
print(results)