#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Feb 23 21:12:26 2025

@author: ambrarustemi
"""

import numpy as np
from scipy.optimize import fsolve
import matplotlib.pyplot as plt
import pandas as pd
import statsmodels.api as sm

# Model Parameters
sigma = 1  # Intertemporal elasticity of substitution
kappa = 0.3  # Slope of the Phillips curve
beta = 0.995  # Discount factor
phi_pi = 1.5  # Taylor rule coefficient for inflation
phi_y = 0.1  # Taylor rule coefficient for output
rho_mu = 0.7  # Autocorrelation coefficient for inflation shock

# Define the matrices A, M, and D
A = np.array([[1, 0, 0, 0],
              [0, 1, 0, 1/sigma],
              [-kappa, 0, 1, 0],
              [0, 0, 0, 1]])

M = np.array([[0, 0, 0, 0],
              [0, 1, 0, 0],
              [0, 0, beta, 0],
              [0, 0, 0, 0]])

D = np.array([[rho_mu, 0, 0, 0],
              [0, 0, 0, 0],
              [0, 0, 0, 0],
              [0, phi_y, phi_pi, 0]])

def steady_state(z_guess, A, M, D):
    """
    Computes the steady state of the model.
    """
    RHS = np.dot(M, z_guess) + np.dot(D, z_guess)
    z_t = np.linalg.solve(A, RHS)
    return z_t - z_guess

# Solve for steady state
z_guess = np.zeros(4)
z_steady = fsolve(steady_state, z_guess, args=(A, M, D))
print("Steady State:", z_steady)

def linear_time_iteration(A, M, D, F_guess, tol=1e-8, max_iter=1000):
    """
    Solves for matrices F and Q using the linear time iteration algorithm.
    """
    F = F_guess.copy()
    for i in range(max_iter):
        F_new = np.linalg.solve(A - M @ F, D)
        if np.max(np.abs(F - F_new)) <= tol:
            print(f"Converged after {i+1} iterations.")
            break
        F = F_new
    else:
        print("Warning: Maximum iterations reached without convergence.")
    Q = np.linalg.inv(A - M @ F)
    return F, Q

# Solve for F and Q
F_guess = np.zeros((4, 4))
F, Q = linear_time_iteration(A, M, D, F_guess)
print("Matrix F:\n", F)
print("Matrix Q:\n", Q)

# Extract MSV solution vectors
C_mu, C_y, C_pi = F[:, 0], F[:, 1], F[:, 2]
C_eps_mu, C_eps_i = Q[:, 0], Q[:, 1]

print("C_mu  =", "  ".join(f"{num:8.4f}" for num in C_mu))
print("C_y   =", "  ".join(f"{num:8.4f}" for num in C_y))
print("C_pi  =", "  ".join(f"{num:8.4f}" for num in C_pi))
print("C_eps_mu =", "  ".join(f"{num:8.4f}" for num in C_eps_mu))
print("C_eps_i  =", "  ".join(f"{num:8.4f}" for num in C_eps_i))


# Impulse response computation
T = 30
mu_shock = 0.01
eps_i_shock = 0.01
z_mu = np.zeros((4, T))
z_eps_i = np.zeros((4, T))
z_mu[:, 0] = C_eps_mu * mu_shock
z_eps_i[:, 0] = C_eps_i * eps_i_shock

for t in range(1, T):
    z_mu[:, t] = F @ z_mu[:, t-1]
    z_eps_i[:, t] = F @ z_eps_i[:, t-1]

# Plot impulse responses
variables = ['mu_t', 'Y_t', 'pi_t', 'i_t']

# Figure 1: Shock to mu_t
import matplotlib.pyplot as plt

variables = ['mu_t', 'Y_t', 'pi_t', 'i_t']

# Figure 1: Shock to mu_t
fig1, axs1 = plt.subplots(2, 2, figsize=(12, 8))
for i, var in enumerate(variables):
    axs1[i//2, i%2].plot(z_mu[i, :], label='Shock to mu_t', linewidth=4, color='purple')
    axs1[i//2, i%2].set_title(var)
    axs1[i//2, i%2].legend()
plt.tight_layout()
plt.show()

# Figure 2: Shock to eps_t^i
fig2, axs2 = plt.subplots(2, 2, figsize=(12, 8))
for i, var in enumerate(variables):
    axs2[i//2, i%2].plot(z_eps_i[i, :], label='Shock to eps_t^i', linewidth=4, color='orange')
    axs2[i//2, i%2].set_title(var)
    axs2[i//2, i%2].legend()
plt.tight_layout()
plt.show()


#EXERCISE 12

# Time series simulation (Exercise 11)
T_sim = 500  # Number of periods
np.random.seed(42)  # Set seed for reproducibility

# Draw random shocks
eps_mu_series = np.random.normal(0, 1, T_sim)  # Shocks for inflation (ϵµ_t)
eps_i_series = np.random.normal(0, 1, T_sim)  # Shocks for interest rate (ϵi_t)

# Initialize state variables
z_series = np.zeros((4, T_sim))  # Matrix to store simulated variables: [mu_t, Y_t, pi_t, i_t]

# Simulate the model
for t in range(1, T_sim):
    z_series[:, t] = F @ z_series[:, t - 1] + Q @ np.array([eps_mu_series[t], eps_i_series[t], 0, 0])

# Save Y_t and π_t in a DataFrame
df_sim = pd.DataFrame({
    'Y_t': z_series[1, :],  # Output (Y_t)
    'pi_t': z_series[2, :]  # Inflation (π_t)
})

# Print the first few rows of the DataFrame
print("Simulated Time Series Data (Exercise 11):")
print(df_sim.head())

# Compute expected inflation
E_pi = (F[2, :] @ df_sim[['mu_t', 'Y_t', 'pi_t', 'i_t']].T).T
df_sim['E_pi_t+1'] = np.append(E_pi[1:], np.nan)

# Forecast errors
df_sim['forecast_error'] = df_sim['pi_t'].shift(-1) - df_sim['E_pi_t+1']

# OLS regression
X = sm.add_constant(df_sim['pi_t'])
y = df_sim['forecast_error']
model = sm.OLS(y, X, missing='drop').fit()
print(model.summary())





