import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import time

# Environment class
class GridWorldEnv:
    def __init__(self, grid_size=5):
        self.actions= ['right', 'down', 'left', 'up']
        self.action_to_delta = {'right': (0, 1), 'down': (1, 0), 'left': (0, -1), 'up': (-1, 0)}
        self.action_space = len(self.actions)
        self.grid_size = grid_size
        self.obs_space = 2  # x, y position
        
    def reset(self):
        self.pos = (0, 0)
        self.history = [self.pos]
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
    
    def render(self, ax=None, show_history=True):
        """Visualize the current state of the grid"""
        if ax is None:
            fig, ax = plt.subplots(1, 1, figsize=(7, 7))
        else:
            ax.clear()
        
        # Draw grid
        for i in range(self.grid_size + 1):
            ax.plot([i, i], [0, self.grid_size], 'k-', linewidth=1)
            ax.plot([0, self.grid_size], [i, i], 'k-', linewidth=1)
        
        # Draw special cells
        # Goal at (0, grid_size-1) = top-right
        goal_row, goal_col = 0, self.grid_size - 1
        goal_rect = patches.Rectangle((goal_col, goal_row), 1, 1, 
                                      linewidth=2, edgecolor='green', 
                                      facecolor='lightgreen', alpha=0.5)
        ax.add_patch(goal_rect)
        ax.text(goal_col + 0.5, goal_row + 0.5, 'GOAL', ha='center', va='center', 
                fontsize=10, fontweight='bold', color='darkgreen')
        
        # Trap at (grid_size-1, 0) = bottom-left
        trap_row, trap_col = self.grid_size - 1, 0
        trap_rect = patches.Rectangle((trap_col, trap_row), 1, 1, 
                                      linewidth=2, edgecolor='red', 
                                      facecolor='lightcoral', alpha=0.5)
        ax.add_patch(trap_rect)
        ax.text(trap_col + 0.5, trap_row + 0.5, 'TRAP', ha='center', va='center', 
                fontsize=10, fontweight='bold', color='darkred')
        
        # Draw history (trajectory)
        if show_history and len(self.history) > 1:
            for i in range(len(self.history) - 1):
                row1, col1 = self.history[i]
                row2, col2 = self.history[i + 1]
                ax.plot([col1 + 0.5, col2 + 0.5], [row1 + 0.5, row2 + 0.5], 
                       'b-', linewidth=3, alpha=0.4, marker='o', markersize=4)
        
        # Draw current position (agent)
        agent_row, agent_col = self.pos
        agent_circle = patches.Circle((agent_col + 0.5, agent_row + 0.5), 
                                     0.35, color='blue', zorder=10, linewidth=2, edgecolor='darkblue')
        ax.add_patch(agent_circle)
        
        # Set axis properties
        ax.set_xlim(0, self.grid_size)
        ax.set_ylim(0, self.grid_size)
        ax.set_aspect('equal')
        ax.set_xlabel('Column', fontsize=12)
        ax.set_ylabel('Row', fontsize=12)
        ax.set_title(f'Grid World - Position: (row={self.pos[0]}, col={self.pos[1]}), Steps: {len(self.history)-1}', 
                    fontsize=14, fontweight='bold')
        
        # Invert y-axis so row 0 is at top
        ax.invert_yaxis()
        
        # Add grid coordinates
        for i in range(self.grid_size):
            for j in range(self.grid_size):
                ax.text(j + 0.5, i + 0.5, f'({i},{j})', 
                       ha='center', va='center', fontsize=9, color='gray', alpha=0.6)
        
        return ax

# Interactive wrapper
class GridWorldWrapper:
    def __init__(self, env):
        self.env = env
        self.total_reward = 0
        
    def play_interactive(self):
        """Interactive mode - print instructions and take manual input"""
        # Turn on interactive mode
        plt.ion()
        
        state = self.env.reset()
        self.total_reward = 0
        done = False
        step_count = 0
        
        print("=== Grid World Interactive Mode ===")
        print("Actions: 0=Right, 1=Down, 2=Left, 3=Up")
        print(f"Goal: Reach (0, {self.env.grid_size-1}) - TOP RIGHT [Green]")
        print(f"Trap: Avoid ({self.env.grid_size-1}, 0) - BOTTOM LEFT [Red]")
        print("=" * 40)
        
        # Create figure once
        fig, ax = plt.subplots(1, 1, figsize=(7, 7))
        
        while not done:
            # Update display
            self.env.render(ax=ax)
            plt.draw()
            plt.pause(0.01)  # Small pause to update display
            
            action_str = input(f"\n Position {self.env.pos} | Step {step_count + 1} | Total Reward: {self.total_reward:.2f}\nEnter action (0=Right, 1=Down, 2=Left, 3=Up) or 'q' to quit: ")
            
            if action_str.lower() == 'q':
                print("Quitting game.")
                plt.close()
                break
                
            try:
                action = int(action_str)
                if action not in range(4):
                    print(" Invalid action! Use 0-3")
                    continue
            except ValueError:
                print(" Invalid input! Enter a number 0-3")
                continue
            
            state, reward, done = self.env.step(action)
            self.total_reward += reward
            step_count += 1
            
            print(f"Action: {self.env.actions[action]} -> New Position: {state}, Reward: {reward:.2f}")
            
        # Final render
        self.env.render(ax=ax)
        plt.draw()
        plt.pause(0.01)
        
        print(f"\n{'='*40}")
        if self.total_reward > 0:
            print("SUCCESS! You reached the goal!")
        else:
            print("FAILED! You hit the trap!")
        print(f"Total reward: {self.total_reward:.2f}")
        print(f"Steps taken: {step_count}")
        print(f"{'='*40}")
        
        # Keep window open
        plt.ioff()
        plt.show()
    
    
env = GridWorldEnv(relative_ignorability=True, grid_size=5)
wrapper = GridWorldWrapper(env)
    

wrapper.play_interactive()
    
    
