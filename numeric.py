import numpy as np
import matplotlib.pyplot as plt
import seaborn as sbn


def f(x):
    return np.exp(-x**2)

L, N = 7, 100
x = np.linspace(-L/2, L/2, N)
psi = f(x)

K = np.eye(N, N)
K_sub = np.vstack((K[1:], np.array([0] * N)))
K = -2 * K + K_sub + K_sub.T

dx = L/N
psi_2dot = dx**-2 * np.dot(K, psi)


plt.plot(x, psi, label="psi")
plt.plot(x, psi_2dot, label="psi_2dot")
plt.xlim(-3.5, 3.5)
plt.ylim(-2.5, 2)

plt.legend()
plt.show()


