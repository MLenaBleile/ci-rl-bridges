"""
TD(0) Learning for Asthma Treatment
Parallel implementation to MSM approach using the Marcus example
"""

import numpy as np
import pandas as pd

np.random.seed(42)

# Simulation parameters
n_patients = 200
n_visits = 3  # Each patient has 3 sequential visits

# Initialize value function estimates
# States discretized by peak flow ranges
state_bins = [0, 180, 220, 260, 500]
n_states = len(state_bins) - 1
V = np.zeros(n_states)  # Value function for each state

# TD(0) hyperparameters
alpha = 0.1  # Learning rate
gamma = 0.9  # Discount factor

print("TD(0) Learning for Asthma Treatment")
print("=" * 50)
print(f"Number of patients: {n_patients}")
print(f"Visits per patient: {n_visits}")
print(f"Learning rate (alpha): {alpha}")
print(f"Discount factor (gamma): {gamma}")
print(f"State bins (peak flow): {state_bins}")
print()

def discretize_state(peak_flow):
    """Convert continuous peak flow to discrete state index"""
    for i in range(len(state_bins) - 1):
        if state_bins[i] <= peak_flow < state_bins[i + 1]:
            return i
    return n_states - 1

def simulate_patient_trajectory(patient_id):
    """
    Simulate one patient trajectory through n_visits
    Returns: list of (state_idx, action, reward, next_state_idx) tuples
    """
    trajectory = []
    
    # Initial peak flow based on severity
    peak_flow = np.random.uniform(160, 280)
    
    for visit in range(n_visits):
        # Current state
        x_j = discretize_state(peak_flow)
        
        # Treatment policy: Higher probability of steroids for lower peak flow
        # This mimics the confounded observational data structure
        prob_steroid = 1.0 / (1.0 + np.exp(0.03 * (peak_flow - 200)))
        a_j = 1 if np.random.rand() < prob_steroid else 0
        
        # Reward: Negative of exacerbation count (we want to minimize)
        # Treatment effect depends on severity
        baseline_exacerbations = max(0, (250 - peak_flow) / 10)
        treatment_effect = -a_j * (5 + 0.02 * (250 - peak_flow))
        noise = np.random.normal(0, 2)
        exacerbations = max(0, baseline_exacerbations + treatment_effect + noise)
        r_j = -exacerbations  # Negative because we want to maximize reward
        
        # Next peak flow (state transition)
        # Treatment improves peak flow, but with natural variation
        improvement = a_j * np.random.uniform(10, 30)
        natural_variation = np.random.normal(0, 10)
        peak_flow_next = np.clip(peak_flow + improvement + natural_variation, 150, 350)
        x_next = discretize_state(peak_flow_next)
        
        trajectory.append((x_j, a_j, r_j, x_next))
        
        # Update for next visit
        peak_flow = peak_flow_next
        
        # Check if trajectory terminated early
        if visit < n_visits - 1 and np.random.rand() < 0.05:
            break
    
    return trajectory

# Store all trajectories for comparison with MSM
all_data = []

print("Running TD(0) Learning...")
print()

# Main TD learning loop
for patient_id in range(n_patients):
    trajectory = simulate_patient_trajectory(patient_id)
    
    # TD(0) updates for this trajectory
    for j, (x_j, a_j, r_j, x_next) in enumerate(trajectory):
        # TD error: delta_j = r_{j+1} + gamma * V(x_{j+1}) - V(x_j)
        td_error = r_j + gamma * V[x_next] - V[x_j]
        
        # Update: V(x_j) <- V(x_j) + alpha * delta_j
        V[x_j] = V[x_j] + alpha * td_error
        
        # Store for comparison
        all_data.append({
            'patient_id': patient_id,
            'visit': j,
            'state': x_j,
            'action': a_j,
            'reward': r_j,
            'next_state': x_next,
            'td_error': td_error,
            'value_before': V[x_j] - alpha * td_error,
            'value_after': V[x_j]
        })
    
    # Diagnostic output every 50 patients
    if (patient_id + 1) % 50 == 0:
        print(f"Processed {patient_id + 1} patients")
        print(f"Current value estimates: {V}")
        print(f"Mean absolute TD error: {np.mean([abs(d['td_error']) for d in all_data[-50*n_visits:]]):.3f}")
        print()

print("\nFinal Results")
print("=" * 50)
print("\nLearned Value Function V(x) by State:")
for i in range(n_states):
    flow_range = f"[{state_bins[i]}, {state_bins[i+1]})"
    print(f"State {i} (peak flow {flow_range}): V = {V[i]:.3f}")

# Convert to DataFrame for analysis
df = pd.DataFrame(all_data)


print("\n\nValue Function Convergence Check:")
print("-" * 50)
# Check convergence by looking at recent updates
recent_updates = df.tail(100)
value_changes = recent_updates['value_after'] - recent_updates['value_before']
print(f"Mean value change (last 100 updates): {value_changes.mean():.6f}")
print(f"Max absolute value change (last 100 updates): {value_changes.abs().max():.6f}")



# Save results
# df.to_csv('td_asthma_results.csv', index=False)

