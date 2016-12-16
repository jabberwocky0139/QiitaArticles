import numpy as np
from scipy.integrate import simps
import matplotlib.pyplot as plt
import seaborn


L, N = 1, 200
# x, dx = np.linspace(-L/2, 3*L/2, N), L / N
x, dx = np.linspace(0, L, N), L / N
K = np.eye(N, N)
K_sub = np.vstack((K[1:], np.array([0] * N)))
K = dx**-2 * (2 * K - K_sub - K_sub.T)

# def pot(arr_x):
#     ans = []
#     for i in arr_x:
#         if 0 <= i <= L:
#             ans.append(0)
#         else:
#             ans.append(1e9)
#     return ans

V = np.diag([0] * N)
H = (K + V) / 2
w, v = np.linalg.eigh(H)

plt.figure(figsize=(6, 4))
# plt.plot(x, v.T[0] / simps(v.T[0]**2, x)**0.5, label="ground state")
# plt.plot(x, v.T[1] / simps(v.T[1]**2, x)**0.5, label="1st excited state")
# plt.plot(x, v.T[2] / simps(v.T[2]**2, x)**0.5, label="2nd excited state")
# plt.plot(x, 2 * np.sin(np.pi * x /L) / np.sqrt(2 * L), '--', label="analytic(ground)")
# plt.plot(x, -2 * np.sin(2 * np.pi * x /L) / np.sqrt(2 * L), '--', label="analytic(1st)")
# plt.plot(x, -2 * np.sin(3 * np.pi * x /L) / np.sqrt(2 * L), '--', label="analytic(2nd)")

# plt.xlabel(r'$x$', fontsize=18)
# plt.ylabel(r'$\psi$', fontsize=18)
# plt.legend(loc="lower left")

# a = [0, 10]
# b = [0, 0]
# plt.plot([0, 0], a, '-k', linewidth=4)
# plt.plot([1, 1], a, '-k', linewidth=4)
# plt.plot([0, 1], b, '-k', linewidth=4)
# plt.xlim(-L/2, 3*L/2)
# plt.ylim(-1.7, 2)

n = np.arange(1, 11)
plt.plot(w[:10], label='numerical')
plt.plot(n**2 * np.pi**2 / (2 * L**2), '--', label='analytic')
plt.xlabel(r'$n$', fontsize=18)
plt.ylabel(r'$E_n$', fontsize=18)
plt.legend(loc="best")
plt.show()

