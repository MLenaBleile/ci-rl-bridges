# This Python 3 environment comes with many helpful analytics libraries installed
# It is defined by the kaggle/python Docker image: https://github.com/kaggle/docker-python
# For example, here's several helpful packages to load
import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from IPython.display import clear_output
import time

# Environment class
class GridWorldEnv:
    def __init__(self,  grid_size=5):
        self.actions= ['right', 'down', 'left', 'up']
        self.action_to_delta = {'right': (0, 1), 'down': (1, 0), 'left': (0, -1), 'up': (-1, 0)}
        self.action_space = len(self.actions)
        self.grid_size = grid_size
        self.obs_space = 2  # x, y position
        
    def reset(self):
        self.pos = (0, 0)
        self.mode = np.random.choice([0, 1])  # Relatively ignorable hidden variable
        self.history = [self.pos]  # Track trajectory
        return np.array(self.pos)
    
    def step(self, action_idx):
        action = self.actions[action_idx]
        dx, dy = self.action_to_delta[action]
        
        new_x = np.clip(self.pos[0] + dx, 0, self.grid_size - 1)
        new_y = np.clip(self.pos[1] + dy, 0, self.grid_size - 1)
        self.pos = (new_x, new_y)
        self.history.append(self.pos)
        
        reward = -0.1
        done = False
        
        if self.pos == (0, self.grid_size - 1):  # Goal (top right)
            reward = 10
            done = True
        elif self.pos == (self.grid_size - 1, 0):  # Trap (bottom left)
            reward = -10 
            done = True
            
        return np.array(self.pos), reward, done
    
    def render(self, show_history=True, figsize=(6, 6)):
        """Visualize the current state of the grid"""
        fig, ax = plt.subplots(1, 1, figsize=figsize)
        
        # Draw grid
        for i in range(self.grid_size + 1):
            ax.plot([i, i], [0, self.grid_size], 'k-', linewidth=1)
            ax.plot([0, self.grid_size], [i, i], 'k-', linewidth=1)
        
        # Draw special cells
        # Goal (top right) - green
        goal_rect = patches.Rectangle((self.grid_size - 1, 0), 1, 1, 
                                      linewidth=2, edgecolor='green', 
                                      facecolor='lightgreen', alpha=0.5)
        ax.add_patch(goal_rect)
        ax.text(self.grid_size - 0.5, 0.5, 'GOAL', ha='center', va='center', 
                fontsize=10, fontweight='bold', color='darkgreen')
        
        # Trap (bottom left) - red
        trap_rect = patches.Rectangle((0, self.grid_size - 1), 1, 1, 
                                      linewidth=2, edgecolor='red', 
                                      facecolor='lightcoral', alpha=0.5)
        ax.add_patch(trap_rect)
        ax.text(0.5, self.grid_size - 0.5, 'TRAP', ha='center', va='center', 
                fontsize=10, fontweight='bold', color='darkred')
        
        # Draw history (trajectory)
        if show_history and len(self.history) > 1:
            for i in range(len(self.history) - 1):
                y1, x1 = self.history[i]
                y2, x2 = self.history[i + 1]
                ax.plot([x1 + 0.5, x2 + 0.5], [self.grid_size - y1 - 0.5, self.grid_size - y2 - 0.5], 
                       'b-', linewidth=2, alpha=0.3)
        
        # Draw current position (agent)
        agent_y, agent_x = self.pos
        agent_circle = patches.Circle((agent_x + 0.5, self.grid_size - agent_y - 0.5), 
                                     0.3, color='blue', zorder=10)
        ax.add_patch(agent_circle)
        
        # Set axis properties
        ax.set_xlim(0, self.grid_size)
        ax.set_ylim(0, self.grid_size)
        ax.set_aspect('equal')
        ax.invert_yaxis()  # Invert y-axis so (0,0) is at top-left visually
        ax.set_xlabel('Column (y)')
        ax.set_ylabel('Row (x)')
        ax.set_title(f'Grid World - Position: {self.pos}, Mode: {self.mode}')
        
        # Add grid coordinates
        for i in range(self.grid_size):
            for j in range(self.grid_size):
                ax.text(j + 0.5, self.grid_size - i - 0.5, f'({i},{j})', 
                       ha='center', va='center', fontsize=8, color='gray', alpha=0.5)
        
        plt.tight_layout()
        return fig



# Interactive wrapper
class GridWorldWrapper:
    def __init__(self, env):
        self.env = env
        self.total_reward = 0
        
    def play_interactive(self):
        """Interactive mode - print instructions and take manual input"""
        state = self.env.reset()
        self.total_reward = 0
        done = False
        step_count = 0
        
        print("=== Grid World Interactive Mode ===")
        print("Actions: 0=Right, 1=Down, 2=Left, 3=Up")
        print("Goal: Reach top-right corner (0, grid_size-1)")
        print("Avoid: Bottom-left trap (grid_size-1, 0)")
        print("=" * 40)
        clear_output(wait=True)
        self.env.render()
        plt.show()
        
        while not done:
            action_str = input(f"\nStep {step_count + 1} - Current position {self.env.pos}. Enter action (0-3) or 'q' to quit: ")
            
            if action_str.lower() == 'q':
                print("Quitting game.")
                break
                
            try:
                action = int(action_str)
                if action not in range(4):
                    print("Invalid action! Use 0-3")
                    continue
            except ValueError:
                print("Invalid input! Enter a number 0-3")
                continue
            
            state, reward, done = self.env.step(action)
            self.total_reward += reward
            step_count += 1
            
            print(f"Action: {self.env.actions[action]} -> Position: {state}, Reward: {reward:.2f}, Total: {self.total_reward:.2f}")
            
            clear_output(wait=True)
            self.env.render()
            plt.show()
            
            if done:
                print(f"\n{'='*40}")
                if reward > 0:
                    print("SUCCESS! You reached the goal!")
                else:
                    print("FAILED! You hit the trap!")
                print(f"Total reward: {self.total_reward:.2f}")
                print(f"Steps taken: {step_count}")
                print(f"{'='*40}")

# Create environment
env = GridWorldEnv(grid_size=5)
wrapper = GridWorldWrapper(env)
    
    # Option 1: Run with random policy
print("Running episode with random policy...")

wrapper.play_interactive()
