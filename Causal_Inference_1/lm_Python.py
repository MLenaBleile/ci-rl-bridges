# lm_example.py

import pandas as pd
import numpy as np
import statsmodels.api as sm
from statsmodels.formula.api import ols

# Create the asthma data matching the R example structure
# R: rep(c("a1", "a1", "a2", "a2"),2)
treatment = ["a1", "a1", "a2", "a2", "a1", "a1", "a2", "a2"]

# R: c(c(20, 25, 50, 45), c(20, 25, 50+10, 45+10)+5)
# First group: [20, 25, 50, 45]
# Second group: [20+5, 25+5, 60+5, 55+5] = [25, 30, 65, 60]
outcome = [20, 25, 50, 45, 25, 30, 65, 60]

# R: c(rep("S",4), rep("NS",4))
smoking_status = ["S", "S", "S", "S", "NS", "NS", "NS", "NS"]


asthma_data = pd.DataFrame({
    'treatment': treatment,
    'outcome': outcome,
    'smoking_status': smoking_status
})


# Fit model with interaction term
# R syntax: outcome ~ treatment * smoking_status
# Python statsmodels uses same formula syntax with C() for categorical variables

model_interaction = ols('outcome ~ C(treatment) * C(smoking_status)', 
                        data=asthma_data).fit()
    


# Display results matching R's summary() output format
print(model_interaction.summary())


