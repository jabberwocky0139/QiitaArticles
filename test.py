import math
import math as m
import numpy as np
from numpy.random import rand
from scipy.integrate import dblquad
from scipy.integrate import quad
import matplotlib.pyplot as plt

def f(x):
    return m.sin(x)


def g(x):
    return np.sin(x)


def h(x, y):
    return np.exp(-(x**2 + y**2))


def Z(m, l, z):
    fact = math.sqrt(math.factorial(m) / math.factorial(l)) / math.factorial(m - l)
    exp = z**(m - l) * math.exp(-0.25 * z**2)
    return fact * exp

def Z_modified(m, l, z):
    ans = 1
    for k in range(m - l):
        ans *= (m - k)**0.5 / (m - l - k) * z * math.exp(-0.25 * z**2 / (m - l))
    return ans

print()
m, l, z = 300, 70, 6.9
print(Z_modified(m, l, z))
print(Z(m, l, z))

# N, L = 2000, 100
# x, dx = np.linspace(-L / 2, L / 2, N), L / N

# a = [[rand() for i in range(N)] for j in range(N)]


# def V(h):
#     c = 2
#     return np.tanh(h - c) + np.tanh(c)

# x = np.linspace(0, 5, 100)

# plt.xticks([])
# plt.yticks([])
# plt.xlabel(r'$h$')
# plt.ylabel(r'$V(h)$')
# plt.plot(x, V(x))
# plt.show()

# --- 微分 ---
# def myImp():
#     diff = []
#     for i in x:
#         diff.append((f(i + dx) - f(i)) / dx)

# # g = np.vectorize(f)

# def npImp():
#     diff = np.gradient(g(x), dx)

# --- 積分 ---
# N, L = 100, 1
# eps = 1e-5
# x_arr = np.linspace(0 + eps, 1 - eps, N)
# dx = L / N


# def integrate(g=lambda x: m.sqrt(1 - x**2), h=lambda x: x):
#     ans = 0
#     for i in x_arr:
#         ans += h(i)**2 / g(i) * dx
#     return ans


# print("before: {0}".format(integrate()))
# print("after: {0}".format(integrate(g=lambda x: 0.5 * m.sqrt(2-x**2),
#                                     h=lambda x: 1-x**2)))
# print("correct: {0}".format(quad(lambda x: x**2 / m.sqrt(1 - x**2), 0, 1)[0]))


# def myImp2():
#     y, dy = np.linspace(-L/2, L/2, N), L / N
#     ans = 0
#     for index, X in enumerate(x):
#         for Y in y[:index+1]:
#             ans += dx * dy * h(X, Y)
#     print(ans)

# def spImp():
#     ans = dblquad(
#         h, a=-np.inf, b=np.inf, gfun=lambda x: -np.inf, hfun=lambda x: x)[0]
#     print(ans)


# --- 行列積 ---
# a = np.array(rand(N, N))
# b = np.array(rand(N, N))
# c = np.array([[0] * N for _ in range(N)])

# def myImp():
#     for i in range(N):
#         for j in range(N):
#             for k in range(N):
#                 c[i][j] = a[i][k] * b[k][j]

# def npImp():
#     c = np.dot(a, b)

# --- 固有値方程式 ---
# L, N = 10, 80
# x, dx = np.linspace(-L/2, L/2, N), L / N

# K = np.eye(N)
# K_sub = np.vstack((K[1:], np.array([0] * N)))
# K = dx**-2 * (2 * K - K_sub - K_sub.T)

# # ポテンシャル項
# V = np.diag(np.linspace(-L/2, L/2, N)**2)

# # エルミート行列の固有値方程式
# H = (K + V) / 2
# w, v = np.linalg.eigh(H)

# I = np.eye(N)
# H = [[0 for i in range(N)] for j in range(N)]
# for i in range(N):
#     H[i][i] = 2.0/(2*dx**2) + 0.5*(-L/2+dx*i)**2
#     if(0 <= i+1 < N):
#         H[i][i+1] = -1.0/(2*dx**2)
#     if(0 <= i-1 < N):
#         H[i][i-1] = -1.0/(2*dx**2)
# H = np.array(H)

# # Jacobi法
# flag = True
# while(flag):
#     # 非対角成分の最大値及びインデックスを調べる
#     maxValue = 0
#     cI, rI = None, None
#     for j in range(N):
#         for i in range(j):
#             if(maxValue < abs(H[i][j])):
#                 maxValue = abs(H[i][j])
#                 rI, cI = i, j

#     # 収束判定
#     if(maxValue < 1e-4):
#         flag = False
#     # print(maxValue)

#     # 回転行列の用意
#     theta = None
#     if(H[cI][cI] == H[rI][rI]):
#         theta = m.pi/4
#     else:
#         theta = 0.5*m.atan(2.0*H[rI][cI]/(H[cI][cI]-H[rI][rI]))
#         J = np.eye(N)
#         J[rI][rI] = m.cos(theta)
#         J[cI][cI] = m.cos(theta)
#         J[rI][cI] = m.sin(theta)
#         J[cI][rI] = -m.sin(theta)

#     # 行列演算
#     H = np.array(np.matrix(J.T)*np.matrix(H)*np.matrix(J))
#     I = np.array(np.matrix(I)*np.matrix(J))

# # 固有値・固有ベクトルの格納
# v, w = I.transpose(), []
# for i in range(N):
#     w.append([H[i][i], i])
# w.sort()
