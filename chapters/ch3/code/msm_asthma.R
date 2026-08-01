# msm_asthma.R
# Marginal structural model with stabilized IPT weights for time-varying
# treatment, applied to a simulated asthma exacerbation cohort.

set.seed(8675309)

n  <- 5000   # patients
TT <- 6      # monthly visits

# ---- Baseline exacerbation count ----
baseline_exac <- pmax(rpois(n, lambda = 4) + rnorm(n, 0, 0.5), 0)

# Long-format panel: one row per (patient, month)
d <- data.frame(
  id            = rep(1:n, each = TT),
  t             = rep(0:(TT - 1), times = n),
  baseline_exac = rep(baseline_exac, each = TT)
)

# Pre-allocate columns we'll fill in the simulation loop
d$exac_prev      <- NA_real_
d$intensified_tx <- NA_integer_
d$exac           <- NA_real_

# At the first visit, prior exacerbation count equals baseline
d$exac_prev[d$t == 0] <- d$baseline_exac[d$t == 0]

# ---- Time-varying simulation loop ----
# At each visit k:
#  - treatment more likely when prior exacerbations are high
#  - observed exac depends on prior exac, current treatment, and noise
#  - exac at k becomes exac_prev at k+1
for (k in 0:(TT - 1)) {
  idx <- d$t == k

  # Treatment assignment (intensify more often when patient is doing poorly)
  d$intensified_tx[idx] <- rbinom(
    n    = sum(idx),
    size = 1,
    prob = plogis(-2 + 0.4 * (d$exac_prev[idx] - 3))
  )

  # Observed exacerbation count, floored at 0
  d$exac[idx] <- pmax(
    d$exac_prev[idx] + rnorm(sum(idx), 0.3, 1.2) - 2.0 * d$intensified_tx[idx],
    0
  )

  # Carry exac forward as the next visit's exac_prev
  if (k < TT - 1) {
    d$exac_prev[d$t == k + 1] <- d$exac[d$t == k]
  }
}

# ---- One-step treatment lag, by patient ----
# Ensure (id, t) order, then shift intensified_tx within each id, filling the
# first row of each patient's series with 0.
d <- d[order(d$id, d$t), ]
d$tx_lag1 <- ave(d$intensified_tx, d$id,
                 FUN = function(x) c(0, x[-length(x)]))

# ---- Stabilized IPT weights via ipwpoint ----
library(ipw)
library(geepack)

wt <- ipwpoint(exposure    = intensified_tx,
               family      = "binomial",
               link        = "logit",
               numerator   = ~ tx_lag1,
               denominator = ~ tx_lag1 + exac_prev,
               data        = d)

d$sw <- wt$ipw.weights

cat("Summary of stabilized weights:\n")
print(summary(d$sw))

# Cumulative treatment exposure within each patient
d$cum_tx <- ave(d$intensified_tx, d$id, FUN = cumsum)

# ---- Four comparison models ----
# 1. MSM with independence correlation
msm_fit <- geeglm(exac ~ cum_tx + I(t/2) + baseline_exac,
                  id      = id,
                  weights = sw,
                  corstr  = "independence",
                  data    = d)

# 2. AR(1) correlation, no weights (still confounded)
ar1_fit <- geeglm(exac ~ cum_tx + I(t/2) + baseline_exac,
                  id     = id,
                  corstr = "ar1",
                  data   = d)

# 3. MSM with AR(1) correlation (handles both confounding and within-patient
#    correlation)
msm_ar1_fit <- geeglm(exac ~ cum_tx + I(t/2) + baseline_exac,
                      id      = id,
                      weights = sw,
                      corstr  = "ar1",
                      data    = d)

# 4. Naive: no weights, independence correlation
naive_fit <- geeglm(exac ~ cum_tx + I(t/2) + baseline_exac,
                    id     = id,
                    corstr = "independence",
                    data   = d)

# ---- Report ----
true_effect <- -2.0  # negative: treatment reduces exacerbations
est_msm     <- coef(msm_fit)["cum_tx"]
est_ar1     <- coef(ar1_fit)["cum_tx"]
est_ar1_msm <- coef(msm_ar1_fit)["cum_tx"]
est_naive   <- coef(naive_fit)["cum_tx"]

results <- data.frame(
  `True Effect`    = true_effect,
  `MSM with AR(1)` = est_ar1_msm,
  `AR(1) Only`     = est_ar1,
  `MSM Only`       = est_msm,
  Naive            = est_naive,
  check.names      = FALSE
)

print(results)
