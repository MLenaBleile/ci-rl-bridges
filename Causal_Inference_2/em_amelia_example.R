
library(tidyverse)
library(patchwork)

# Use the exact simulation from the chapter
set.seed(1998)
n <- 1000

df_oracle <- tibble(
  Subject = 1:n,
  Treatment = sample(c("a1", "a2"), n, replace = TRUE),
  Smoking_Status = sample(c("Smoker", "Non-smoker"), n, 
                          replace = TRUE, prob = c(0.4, 0.6))
) %>%
  mutate(
    True_Outcome = case_when(
      Treatment == "a1" & Smoking_Status == "Smoker" ~ 20,
      Treatment == "a1" & Smoking_Status == "Non-smoker" ~ 22,
      Treatment == "a2" & Smoking_Status == "Smoker" ~ 25,
      Treatment == "a2" & Smoking_Status == "Non-smoker" ~ 55,
      TRUE ~ NA_real_
    ) + rnorm(n, 0, 3)
  )

df_mar <- df_oracle
missing_mar <- ifelse(df_mar$Treatment == "a2",
                      sample(c(TRUE, FALSE), sum(df_mar$Treatment == "a2"),
                             replace = TRUE, prob = c(0.6, 0.4)),
                      sample(c(TRUE, FALSE), sum(df_mar$Treatment == "a1"),
                             replace = TRUE, prob = c(0.1, 0.9)))
df_mar$Smoking_Status[missing_mar] <- NA


# EM Algorithm Example: Using the asthma dataset with MAR missingness
library(Amelia)
library(tidyverse)

# Use the previously created df_mar dataset with MAR missingness pattern
# (where df_mar has ~60% missing smoking status for a2, ~10% for a1)


amelia_imputed <- amelia(df_mar %>% 
                           select(Treatment, Smoking_Status, True_Outcome),
                         m = 5,  # Create 5 imputed datasets
                         noms = c("Treatment", "Smoking_Status"),  # Nominal variables
                         p2s = 0,  # Suppress output
                         seed = 1998)

# Analyze each Amelia imputation
amelia_models <- lapply(amelia_imputed$imputations, function(data) {
  lm(True_Outcome ~ Treatment * Smoking_Status, data = data)
})

# Pool Amelia results using MIcombine
library(mitools)
amelia_pooled <- MIcombine(amelia_models)


amelia_pooled
