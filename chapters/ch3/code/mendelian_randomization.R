# mendelian_randomization.R
set.seed(2024)
n <- 5000

# Unmeasured confounder (lifestyle factors)
xm <- rnorm(n, 0, 1)

# Genetic instrument: binary SNP (0 = reference, 1 = variant)
# Assigned independently of xm
xiv <- rbinom(n, 1, 0.3)

# LDL cholesterol: affected by both genetics and lifestyle
# xiv reduces LDL; xm (poor lifestyle) increases LDL
LDL <- 130 + (-15) * xiv + 20 * xm + rnorm(n, 0, 10)

# Coronary heart disease risk score
# True causal effect of LDL: 0.5 per unit
# xm also directly affects CHD (confounding)
CHD <- 10 + 0.5 * LDL + 15 * xm + rnorm(n, 0, 5)

data <- data.frame(xiv = xiv, LDL = LDL, CHD = CHD, xm = xm)

# Naive estimate: regress CHD on LDL
naive_model <- lm(CHD ~ LDL, data = data)
coef(naive_model)["LDL"]

library(ivreg)

# 2SLS: instrument LDL with xiv
iv_model <- ivreg(CHD ~ LDL | xiv, data = data)
summary(iv_model)

coef(iv_model)["LDL"]


# Manual implementation
# Stage 1: Regress endogenous variable on instrument
stage1 <- lm(LDL ~ xiv, data = data)
data$LDL_hat <- fitted(stage1)

# Stage 2: Regress outcome on fitted values
stage2 <- lm(CHD ~ LDL_hat, data = data)
coef(stage2)["LDL_hat"]



# F-statistic for instrument strength
summary(stage1)$fstatistic


# Weak-instrument-robust inference
library(ivmodel)

iv_obj <- ivmodel(Y = data$CHD, D = data$LDL, Z = data$xiv)
AR.test(iv_obj)