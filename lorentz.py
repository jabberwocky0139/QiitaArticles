import numpy as np
from scipy.integrate import odeint, simps
import matplotlib.pyplot as plt
import seaborn as sbn


def duffing(var, t, gamma, a, b, F0, omega, delta):
    """
    var = [x, p]
    dx/dt = p
    dp/dt = -gamma*p + 2*a*x - 4*b*x**3 + F0*cos(omega*t + delta)
    """
    x_dot = var[1]
    p_dot = -gamma * var[1] + 2 * a * var[0] - 4 * b * var[0]**3 + F0 * np.cos(omega * t + delta)

    return np.array([x_dot, p_dot])
    
F0, gamma, omega, delta = 10, 0.1, np.pi/3, 1.5*np.pi
a, b = 1/4, 1/2
var, var_lin = [[0, 1]] * 2

t = np.arange(0, 20000, 2*np.pi/omega)
t_lin = np.linspace(0, 100, 10000)

var = odeint(duffing, var, t, args=(gamma, a, b, F0, omega, delta))
var_lin = odeint(duffing, var_lin, t_lin, args=(gamma, a, b, F0, omega, delta))

x, p = var.T[0], var.T[1]
x_lin, p_lin = var_lin.T[0], var_lin.T[1]

plt.plot(x, p, ".", markersize=4)
plt.show()


plt.plot(t_lin, x_lin)
var_lin = odeint(duffing, [0.1, 1], t_lin, args=(gamma, a, b, F0, omega, delta))
x_lin, p_lin = var_lin.T[0], var_lin.T[1]
plt.plot(t_lin, x_lin)
plt.show()

# fig = plt.figure()
# ax = fig.gca(projection='3d')
# ax.plot(v[:, 0], v[:, 1], v[:, 2])
# plt.show()
