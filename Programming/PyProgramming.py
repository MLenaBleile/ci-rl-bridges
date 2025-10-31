x = [10, 20, 30]
print(x[1:2])
print(x[1:3])

import numpy as np # linear algebra
import pandas as pd # data processing

df=pd.DataFrame({
    'patient_id': ["A","B","C","D"],
    'A': [8,3,1,0],
    'B': [4,2,9,0]

})

print(df.A[0:5])

subset_df = df[df['A'] == 1]  
print(f"Original df shape: {df.shape}")
print(f"Subset shape: {subset_df.shape}")



new_df = df
new_df.A[0] = 7
print(df)

from copy import deepcopy
deepcopy_df = deepcopy(df)
deepcopy_df.B[0] = 7
print(df)

import gymnasium as gym
env = gym.make('CartPole-v1', render_mode=None)

# The modern Gymnasium API returns 2 values from reset
observation, info = env.reset(seed=42)
