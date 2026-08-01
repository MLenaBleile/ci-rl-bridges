# doubly_robust_example.py
"""
Doubly Robust Estimation for Off-Policy Evaluation

This script demonstrates the doubly robust estimator from Equation 7.4:

    V_hat_DR(x_o) = (1/n) sum_j [omega_hat_j * (r_j - Q_hat(x_{o,j}, a_j))
                                + sum_a pi(a|x_{o,j}) Q_hat(x_{o,j}, a)]

We provide both a from-scratch implementation that directly maps to the
equation, and an equivalent using EconML's DRLearner for practitioners.
"""

import numpy as np
from sklearn.linear_model import LogisticRegression, Ridge


# =============================================================================
# From-Scratch Implementation (Equation 7.4)
# =============================================================================

def doubly_robust_ate(R, A, pi_hat, Q_hat_1, Q_hat_0):
    """
    Doubly robust estimator for the Average Treatment Effect.

    Implements Equation 7.4 for binary actions A in {0, 1}.

    Parameters
    ----------
    R : array of shape (n,)
        Observed rewards r_j
    A : array of shape (n,)
        Observed actions a_j in {0, 1}
    pi_hat : array of shape (n,)
        Estimated propensity scores pi_hat(a=1|x_o)
    Q_hat_1 : array of shape (n,)
        Estimated outcome E[R|x_o, a=1]
    Q_hat_0 : array of shape (n,)
        Estimated outcome E[R|x_o, a=0]

    Returns
    -------
    tau_dr : float
        Doubly robust ATE estimate E[R|do(a=1)] - E[R|do(a=0)]
    """
    # Clip propensity scores to avoid extreme weights (Equation 7.8)
    pi_clipped = np.clip(pi_hat, 0.01, 0.99)

    # Importance weights omega_hat_j
    # For treated: omega = 1/pi_hat; for control: omega = 1/(1-pi_hat)
    omega_1 = 1 / pi_clipped
    omega_0 = 1 / (1 - pi_clipped)

    # Observed Q value
    Q_hat_obs = np.where(A == 1, Q_hat_1, Q_hat_0)

    # Doubly robust components (Equation 7.4)
    # Residual term: omega_hat_j * (r_j - Q_hat(x_{o,j}, a_j))
    residual_1 = A * omega_1 * (R - Q_hat_obs)
    residual_0 = (1 - A) * omega_0 * (R - Q_hat_obs)

    # Direct term: Q_hat(x_o, 1) - Q_hat(x_o, 0)
    direct_term = Q_hat_1 - Q_hat_0

    # DR estimate for each observation, then average
    tau_hat = (residual_1 - residual_0) + direct_term
    tau_dr = tau_hat.mean()

    return tau_dr


def demonstrate_double_robustness():
    """
    Demonstrate the 'double' in doubly robust: consistency holds if
    EITHER the propensity model OR the outcome model is correct.
    """
    np.random.seed(1998)
    n = 5000

    # Observed covariates that confound treatment and outcome
    X_o = np.random.randn(n, 2)

    # Treatment depends on X_o (no unobserved confounding)
    logit = 0.5 * X_o[:, 0] + 0.3 * X_o[:, 1]
    pi_true = 1 / (1 + np.exp(-logit))
    A = np.random.binomial(1, pi_true)

    # True CATE: tau(x) = 2 (constant treatment effect)
    tau_true = 2.0

    # Outcome: R = tau * A + X_o @ beta + epsilon
    R = tau_true * A + 1.5 * X_o[:, 0] + 0.8 * X_o[:, 1] + np.random.randn(n)

    # === Fit models ===

    # Correct propensity model: trained on true covariates
    pi_model_correct = LogisticRegression().fit(X_o, A)
    pi_hat_correct = pi_model_correct.predict_proba(X_o)[:, 1]

    # Wrong propensity model: trained on noise (ignores true confounders)
    X_noise_pi = np.random.randn(n, 2)
    pi_model_wrong = LogisticRegression().fit(X_noise_pi, A)
    pi_hat_wrong = pi_model_wrong.predict_proba(X_noise_pi)[:, 1]

    # Correct outcome model: trained on true covariates
    Q_model_correct = Ridge().fit(np.column_stack([X_o, A]), R)
    Q_hat_1_correct = Q_model_correct.predict(np.column_stack([X_o, np.ones(n)]))
    Q_hat_0_correct = Q_model_correct.predict(np.column_stack([X_o, np.zeros(n)]))

    # Wrong outcome model: trained on noise (ignores true confounders)
    X_noise_Q = np.random.randn(n, 2)
    Q_model_wrong = Ridge().fit(np.column_stack([X_noise_Q, A]), R)
    Q_hat_1_wrong = Q_model_wrong.predict(np.column_stack([X_noise_Q, np.ones(n)]))
    Q_hat_0_wrong = Q_model_wrong.predict(np.column_stack([X_noise_Q, np.zeros(n)]))

    # === Compute DR estimates under each scenario ===

    tau_both = doubly_robust_ate(R, A, pi_hat_correct,
                                 Q_hat_1_correct, Q_hat_0_correct)

    tau_pi_only = doubly_robust_ate(R, A, pi_hat_correct,
                                    Q_hat_1_wrong, Q_hat_0_wrong)

    tau_Q_only = doubly_robust_ate(R, A, pi_hat_wrong,
                                   Q_hat_1_correct, Q_hat_0_correct)

    tau_neither = doubly_robust_ate(R, A, pi_hat_wrong,
                                    Q_hat_1_wrong, Q_hat_0_wrong)

    # Naive estimator (no correction for confounding)
    tau_naive = R[A == 1].mean() - R[A == 0].mean()

    print("True ATE:                        {:.3f}".format(tau_true))
    print("Naive (confounded):              {:.3f}".format(tau_naive))
    print("-" * 45)
    print("DR (both correct):               {:.3f}".format(tau_both))
    print("DR (propensity correct only):    {:.3f}".format(tau_pi_only))
    print("DR (outcome correct only):       {:.3f}".format(tau_Q_only))
    print("DR (neither correct):            {:.3f}".format(tau_neither))


# =============================================================================
# EconML Implementation (Practical Usage)
# =============================================================================

def econml_example():
    """
    Equivalent implementation using EconML's DRLearner.

    In practice, DRLearner provides cross-fitting, confidence intervals,
    and flexible model specification that the from-scratch version lacks.
    """
    from econml.dr import DRLearner
    from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor

    np.random.seed(1998)
    n = 5000

    # Same data generating process
    X_o = np.random.randn(n, 2)
    logit = 0.5 * X_o[:, 0] + 0.3 * X_o[:, 1]
    pi_true = 1 / (1 + np.exp(-logit))
    A = np.random.binomial(1, pi_true)
    tau_true = 2.0
    R = tau_true * A + 1.5 * X_o[:, 0] + 0.8 * X_o[:, 1] + np.random.randn(n)

    # DRLearner with flexible nonparametric models
    dr_learner = DRLearner(
        model_propensity=RandomForestClassifier(n_estimators=100, max_depth=5),
        model_regression=RandomForestRegressor(n_estimators=100, max_depth=5),
        cv=5
    )
    dr_learner.fit(R, A, X=X_o)

    tau_hat = dr_learner.effect(X_o)

    print("\nEconML DRLearner Results:")
    print("True ATE:      {:.3f}".format(tau_true))
    print("Estimated ATE: {:.3f}".format(tau_hat.mean()))


if __name__ == "__main__":
    demonstrate_double_robustness()
    econml_example()