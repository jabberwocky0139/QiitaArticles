import numpy as np
from scipy.integrate import simps, quad
import matplotlib.pyplot as plt
import seaborn


# def pot(arr_x):
#     ans = []
#     for i in arr_x:
#         if 0 <= i <= L:
#             ans.append(0)
#         else:
#             ans.append(1e9)
#     return ans

def finite1():

    L, N = 2, 200
    x, dx = np.linspace(0, L, N), L / N
    
    K = np.eye(N, N)
    K_sub = np.vstack((K[1:], np.array([0] * N)))
    K = dx**-2 * (2 * K - K_sub - K_sub.T)

    
    V = np.diag([0] * (N // 2) + [100] * (N // 2))
    H = (K + V) / 2
    w, v = np.linalg.eigh(H)

    plt.figure(figsize=(6, 4))

    ## 固有関数
    plt.plot(x, np.abs(v.T[0] / simps(v.T[0]**2, x)**0.5), label="ground state")
    plt.plot(x, v.T[1] / simps(v.T[1]**2, x)**0.5, label="1st excited state")
    plt.plot(x, -v.T[2] / simps(v.T[2]**2, x)**0.5, label="2nd excited state")
    # plt.plot(x, 2 * np.sin(np.pi * x /L) / np.sqrt(2 * L), '--', label="analytic(ground)")
    # plt.plot(x, -2 * np.sin(2 * np.pi * x /L) / np.sqrt(2 * L), '--', label="analytic(1st)")
    # plt.plot(x, -2 * np.sin(3 * np.pi * x /L) / np.sqrt(2 * L), '--', label="analytic(2nd)")

    plt.xlabel(r'$x$', fontsize=18)
    plt.ylabel(r'$\psi$', fontsize=18)
    plt.legend(loc="lower left")

    ## ポテンシャル
    a = [0, 10]
    b = [0, 0]
    plt.plot([0.01, 0.01], [0, 10], '-k', linewidth=4)
    plt.plot([1, 1], [0, 2], '-k', linewidth=4)
    plt.plot([0, 1], [0, 0], '-k', linewidth=4)
    plt.plot([1, 2], [2, 2], '-k', linewidth=4)
    plt.xlim(0, L)
    plt.ylim(-1.7, 3)
    plt.text(1.1, 2.1, r'$V = 100$', ha = 'center', va = 'bottom')
    print(w[:3])

def finite2():

    L, N = 1, 400
    x, dx = np.linspace(-L, L, N), 2 * L / N
    
    K = np.eye(N, N)
    K_sub = np.vstack((K[1:], np.array([0] * N)))
    K = dx**-2 * (2 * K - K_sub - K_sub.T)

    V = np.diag([150] * (N // 4) + [0] * (N // 2) + [150] * (N // 4))
    H = K / 2 + V
    w, v = np.linalg.eigh(H)

    plt.figure(figsize=(6, 4))

    ## 固有関数
    plt.plot(x, np.abs(v.T[0] / simps(v.T[0]**2, x)**0.5), label="ground state")
    plt.plot(x, v.T[1] / simps(v.T[1]**2, x)**0.5, label="1st excited state")
    plt.plot(x, v.T[2] / simps(v.T[2]**2, x)**0.5, label="2nd excited state")

    # B = 1.335
    A = 835.7
    B = quad(lambda x: (A * np.exp(17.09 * x))**2, a = -np.inf, b=-L/2)[0] + quad(lambda x: (np.cos(2.815 * x))**2, a = -L/2, b = L/2)[0] + quad(lambda x:(A * np.exp(-17.09 * x))**2, a = L/2, b = np.inf)[0]
    B = B**0.5
    print(B)
    
    plt.plot(x, np.cos(2.815 * x) / B, '--', label="analytical : cos(kx)")
    plt.plot(x, A * np.exp(-17.09 * x) / B, '--', label="analytical : exp(-kappa x)")
    plt.plot(x, A * np.exp(17.09 * x) / B, '--', label="analytical : exp(kappa x)")
    # plt.plot(x, -2 * np.sin(2 * np.pi * x /L) / np.sqrt(2 * L), '--', label="analytic(1st)")
    # plt.plot(x, -2 * np.sin(3 * np.pi * x /L) / np.sqrt(2 * L), '--', label="analytic(2nd)")

    plt.xlabel(r'$x$', fontsize=18)
    plt.ylabel(r'$\psi$', fontsize=18)
    plt.legend(loc="lower left")

    ## ポテンシャル
    a = [0, 10]
    b = [0, 0]
    plt.plot([-1, -0.5], [2, 2], '-k', linewidth=4)
    plt.plot([-0.5, -0.5], [2, 0], '-k', linewidth=4)
    plt.plot([-0.5, 0.5], [0, 0], '-k', linewidth=4)
    plt.plot([0.5, 0.5], [0, 2], '-k', linewidth=4)
    plt.plot([0.5, 1], [2, 2], '-k', linewidth=4)
    plt.xlim(-L, L)
    plt.ylim(-1.5, 2.5)
    plt.text(0.6, 2.1, r'$V = 150$', ha = 'center', va = 'bottom', fontsize=12)
    print(w[:3])


finite2()
## エネルギー固有値
# n = np.arange(1, 11)
# plt.plot(w[:10], label='numerical')
# plt.plot(n**2 * np.pi**2 / (2 * L**2), '--', label='analytic')
# plt.xlabel(r'$n$', fontsize=18)
# plt.ylabel(r'$E_n$', fontsize=18)
plt.legend(loc="upper center")
plt.show()

