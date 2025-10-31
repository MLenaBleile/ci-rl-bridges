# Chapter 2
# Marginal Structural Model example: HIV/CD4 longitudinal data
# 
# 


# PACKAGES 
library(data.table)   # fast data manipulation
library(ipw)          # inverse probability of treatment weighting
library(geepack)      # GEE for MSM

set.seed(8675309)     # reproducibility

# DESIGN PARAMETERS ----------------------------------------------------
n <- 5000     # patients
K <- 6        # semi-annual visits (3 years)

# SIMULATE BASELINE CD4 -----------------------------------------------
baseline_cd4 <- rnorm(n, mean = 500, sd = 50)

# BUILD PERSON-PERIOD LONG FORMAT --------------------------------------
d <- data.table(
  id = rep(1:n, each = K),
  t  = rep(0:(K - 1), times = n),
  baseline_cd4 = rep(baseline_cd4, each=K)
)

# PREP LAGGED CD4 HOLDER
d[ , cd4_prev := ifelse(t == 0, baseline_cd4[id], NA_real_)]

# SIMULATE LONGITUDINAL TREATMENT & CD4 --------------------------------
for (k in 0:(K - 1)) {
  # ART more likely when current CD4 low
  d[t == k, art := rbinom(.N, 1, plogis(-3 + 0.01 * cd4_prev))]

  # CD4 trajectory: natural decline mitigated by ART
  d[t == k, cd4 := cd4_prev - rnorm(.N, 40, 30) + 50 * art]

  # propagate CD4 to next period
  if (k < K - 1) {
    d[t == k, cd4_next := cd4]
    d[t == k + 1, cd4_prev := d[t == k, cd4_next]]
  }
}
d[ , cd4_next := NULL]

# CALCULATE LAGGED TREATMENT (HISTORY) ---------------------------------
d[ , art_lag1 := shift(art, type = "lag", fill = 0), by = id]

# INVERSE-PROBABILITY WEIGHTS ------------------------------------------
wt <- ipwpoint(exposure    = art,
               family      = "binomial",
               link        = "logit",
               numerator   = ~ art_lag1,
               denominator = ~ art_lag1 + cd4_prev,
               data        = as.data.frame(d),
               trunc       = 0.01)

d[ , sw := wt$ipw.weights]

# DIAGNOSTICS ----------------------------------------------------------
print(summary(d$sw))

# CUMULATIVE ART EXPOSURE ----------------------------------------------
d[ , cum_art := cumsum(art), by = id]

# FIT MARGINAL STRUCTURAL MODEL ----------------------------------------
msm_fit <- geeglm(cd4 ~ cum_art + I(t/2) + baseline_cd4,
                  id      = id,
                  weights = sw,
                  corstr  = "independence",
                  data    = as.data.frame(d))

print(summary(msm_fit))

ar1_fit <- geeglm(cd4 ~ cum_art + I(t/2)+baseline_cd4,
                  id      = id,
                  #weights = sw,
                  corstr  = "ar1",
                  data    = as.data.frame(d))

print(summary(ar1_fit))

msm_ar1_fit <- geeglm(cd4 ~ cum_art + I(t/2)+baseline_cd4,
                      id      = id,
                      weights = sw,
                      corstr  = "ar1",
                      data    = as.data.frame(d))

print(summary(msm_ar1_fit))

# NAIVE (UNWEIGHTED) COMPARISON ----------------------------------------
naive_fit <- geeglm(cd4 ~ cum_art + I(t/2)+baseline_cd4,
                    id     = id,
                    corstr = "independence",
                    data   = d)


true_effect <- 50
est_msm     <- coef(msm_fit)["cum_art"]
est_ar1 <- coef(ar1_fit)["cum_art"]
est_ar1_msm <- coef(msm_ar1_fit)["cum_art"]
est_naive   <- coef(naive_fit)["cum_art"]

cat("True ART effect per timepoint:     ", true_effect, "\n")
cat("Estimated by MSM (IPTW adjusted with AR1):  ", round(est_ar1_msm, 2), "\n")
cat("Estimated by MSM (IPTW adjusted):  ", round(est_msm, 2), "\n")
cat("Estimated by MSM (AR1 only):  ", round(est_ar1, 2), "\n")
cat("Estimated by naive model:          ", round(est_naive, 2), "\n")
