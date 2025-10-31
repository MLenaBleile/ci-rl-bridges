# Chapter 2
# Marginal Structural Model example: Asthma exacerbations with time-varying treatment
# 

library(data.table)


set.seed(8675309)


n <- 5000     # patients
TT <- 6        # monthly visits (6 months)


# Weekly exacerbation count at baseline
baseline_exac <- rpois(n, lambda = 4) 
#+ rnorm(n, 0, 0.5)
baseline_exac <- pmax(baseline_exac, 0)  # ensure non-negative


d <- data.table(
  id = rep(1:n, each = TT),
  t  = rep(0:(TT - 1), times = n),
  baseline_exac = rep(baseline_exac, each = TT)
)


d[ , exac_prev := ifelse(t == 0, baseline_exac, NA_real_)]


for (k in 0:(TT - 1)) {
  # Intensified treatment more likely when exacerbations high
  # (doctors increase medication when patient is doing poorly)
  d[t == k, intensified_tx := rbinom(.N, 1, plogis(-2 + 0.4 * (exac_prev - 3)))]
  

  d[t == k, exac := exac_prev + rnorm(.N, 0.3, 1.2) - 2.0 * intensified_tx]
  d[t == k, exac := pmax(exac, 0)]  # can't have negative exacerbations
  

  if (k < TT - 1) {
    d[t == k, exac_next := exac]
    d[t == k + 1, exac_prev := d[t == k, exac_next]]
  }
}

d[ , exac_next := NULL]


d[ , tx_lag1 := shift(intensified_tx, type = "lag", fill = 0), by = id]


library(ipw)
library(geepack)
wt <- ipwpoint(exposure    = intensified_tx,
               family      = "binomial",
               link        = "logit",
               numerator   = ~ tx_lag1,
               denominator = ~ tx_lag1 + exac_prev,
               data        = as.data.frame(d),
               trunc       = 0.01)

d[ , sw := wt$ipw.weights]


cat("Summary of stabilized weights:\n")
print(summary(d$sw))

# cumulative treatment exposure
d[ , cum_tx := cumsum(intensified_tx), by = id]


# MSM with independence correlation structure
msm_fit <- geeglm(exac ~ cum_tx + I(t/2) + baseline_exac,
                  id      = id,
                  weights = sw,
                  corstr  = "independence",
                  data    = as.data.frame(d))

# AR(1) correlation without weights (confounded)
ar1_fit <- geeglm(exac ~ cum_tx + I(t/2) + baseline_exac,
                  id      = id,
                  corstr  = "ar1",
                  data    = as.data.frame(d))

# MSM with AR(1) correlation (properly accounts for both)
msm_ar1_fit <- geeglm(exac ~ cum_tx + I(t/2) + baseline_exac,
                      id      = id,
                      weights = sw,
                      corstr  = "ar1",
                      data    = as.data.frame(d))


naive_fit <- geeglm(exac ~ cum_tx + I(t/2) + baseline_exac,
                    id     = id,
                    corstr = "independence",
                    data   = d)


true_effect <- -2.0  # negative because treatment REDUCES exacerbations
est_msm     <- coef(msm_fit)["cum_tx"]
est_ar1     <- coef(ar1_fit)["cum_tx"]
est_ar1_msm <- coef(msm_ar1_fit)["cum_tx"]
est_naive   <- coef(naive_fit)["cum_tx"]

results=data.frame(`True Effect`=true_effect,
           `MSM with AR(1)`=est_ar1_msm,
           `AR(1) Only` = est_ar1,
           `MSM Only` = est_msm,
           Naive = est_naive)

print(results)
