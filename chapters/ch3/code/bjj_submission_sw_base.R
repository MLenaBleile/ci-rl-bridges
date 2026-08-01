# bjj_submission_sw_base.R
# Estimates per-submission success rates when fatigue confounds BOTH which
# submission gets attempted AND whether it works. Compares the naive
# (unweighted) success rate against the stabilized-weight (IPTW) estimator.

set.seed(1998)

# ---- Base success rates for each submission, when fresh ----
base_success <- c(AnkleLock = 0.7, Triangle = 0.9,
                  Kimura = 0.5, BuggyChoke = 0.05)

# Fatigue multiplies the success probability of every submission by this factor
fatigue_penalty <- 0.5

# Submission counts when not fatigued
observations <- c(rep("AnkleLock", 50), rep("Triangle", 70),
                  rep("Kimura", 50), rep("BuggyChoke", 1))

# Submission counts when fatigued (buggy chokes spike)
fatigued_observations <- c(rep("AnkleLock", 40), rep("Triangle", 50),
                           rep("Kimura", 30), rep("BuggyChoke", 20))

# Repeat the same training pattern over many sessions, so that the rare
# fresh buggy-choke attempts accumulate enough data to estimate
sessions <- 500

fatigue <- rep(c(rep(0, length(observations)),
                 rep(1, length(fatigued_observations))), times = sessions)
submission <- factor(rep(c(observations, fatigued_observations),
                         times = sessions),
                     levels = names(base_success))
n <- length(submission)

# ---- Outcome: fatigue lowers the success rate of EVERY submission ----
# This second arrow is what makes fatigue a confounder rather than a
# harmless driver of action selection: it moves the action and the reward.
success_prob <- base_success[as.character(submission)] *
                ifelse(fatigue == 1, fatigue_penalty, 1)
outcome <- rbinom(n, size = 1, prob = success_prob)

# ---- The estimand ----
# Success rate each submission would achieve if it were always attempted,
# averaged over the marginal fatigue distribution.
p_fatigued    <- mean(fatigue)
true_marginal <- base_success *
                 ((1 - p_fatigued) + p_fatigued * fatigue_penalty)

# ---- Naive estimate: raw success rate per submission ----
# Confounded: buggy chokes are attempted almost exclusively when fatigued,
# so their raw rate reflects the fatigued state rather than the submission.
naive_est <- tapply(outcome, submission, mean)

# ---- Propensity scores via multinomial logistic regression ----
# Multinomial because the ACTION (submission choice) has four levels; the
# type of the outcome plays no part in choosing the propensity model.
library(nnet)
ps_model <- multinom(submission ~ fatigue, trace = FALSE)
ps <- predict(ps_model, type = "probs")   # n x 4 matrix of class probabilities

# ---- Stabilized weights ----
# Propensity of the actually-chosen action for each attempt
submission_int <- as.integer(submission)
pi_x <- ps[cbind(1:n, submission_int)]

# Marginal probability of each submission level
pi_marginal <- table(submission) / n
pi_a <- as.numeric(pi_marginal[as.character(submission)])

# Stabilized weights, then weighted success rate per submission
sw           <- pi_a / pi_x
weighted_est <- tapply(outcome * sw, submission, sum) /
                tapply(sw, submission, sum)

# ---- Compare estimates ----
result <- rbind(
  `True marginal` = true_marginal,
  Naive           = naive_est[names(base_success)],
  Stabilized      = weighted_est[names(base_success)]
)

print(round(result, 4))
