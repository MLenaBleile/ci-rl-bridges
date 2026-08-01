# value_iteration.py
# Companion script for the Value Iteration listing in Chapter 5.

import numpy as np

def value_iteration(states, actions, transition_prob, rewards, gamma=0.9, theta=1e-6):
    # Initialize value function
    V = np.zeros(len(states))

    while True:
        delta = 0
        for s in states:
            v = V[s]
            # Compute the maximum expected value over all actions
            V[s] = max(
                sum(transition_prob[s, a, s_next] *
                    (rewards[s_next] + gamma * V[s_next])
                    for s_next in states)
                for a in actions
            )
            delta = max(delta, abs(v - V[s]))

        # Check for convergence
        if delta < theta:
            break

    # Derive the optimal policy
    policy = np.zeros(len(states), dtype=int)
    for s in states:
        policy[s] = np.argmax([
            sum(transition_prob[s, a, s_next] *
                (rewards[s_next] + gamma * V[s_next])
                for s_next in states)
            for a in actions
        ])

    return V, policy


# Example usage: a simple MDP with 4 states and two actions
np.random.seed(1998)

states = range(4)
actions = range(2)

transition_prob = np.random.rand(4, 2, 4)

# Reward is a function of the arrived-at state only, per the book's
# convention rho(x')
rewards = np.random.rand(4)

# Normalize transition probabilities
transition_prob /= transition_prob.sum(axis=2, keepdims=True)

optimal_values, optimal_policy = value_iteration(states, actions, transition_prob, rewards)

print("Optimal values:", np.round(optimal_values, 4))
print("Optimal policy:", optimal_policy)
