import numpy as np
from scipy.integrate import odeint, simps
from scipy import constants
import matplotlib.pyplot as plt
import seaborn as sbn


def manybody(var, t, m3, m4, m5, G):
    """
    var = [x3, y3, x3_dot, y3_dot, x4, y4, x4_dot, y4_dot, x5, y5, x5_dot, y5_dot]
    dx3/dt = p3
    dpx3/dt = -2*G*m3*x3(m4*(r34)**-1.5 + m5*(r35)**-1.5)
    dy3/dt = p3
    dpy3/dt = -2*G*m3*y3(m4*(r34)**-1.5 + m5*(r35)**-1.5)
    ...
    """

    def r(x1, y1, x2, y2):
        return np.sqrt((x1 - x2)**2 + (y1- y2)**2)**-3

    # (x3, y3)
    x3 = var[2]
    px3 = -G * (m4 * r(var[0], var[1], var[4], var[5]) * (var[0] - var[4]) + m5 * r(var[0], var[1], var[8], var[9]) * (var[0] - var[8]))
    y3 = var[3]
    py3 = -G * (m4 * r(var[0], var[1], var[4], var[5]) * (var[1] - var[5]) + m5 * r(var[0], var[1], var[8], var[9]) * (var[1] - var[9]))
    
    
    # (x4, y4)
    x4 = var[6]
    px4 = -G * (-m3 * r(var[4], var[5], var[0], var[1]) * (var[0] - var[4]) + m5 * r(var[4], var[5], var[8], var[9]) * (var[4] - var[8]))
    y4 = var[7]
    py4 = -G * (-m3 * r(var[4], var[5], var[0], var[1]) * (var[1] - var[5]) + m5 * r(var[4], var[5], var[8], var[9]) * (var[5] - var[9]))
    

    # (x5, y5)
    x5 = var[10]
    px5 = -G * (-m3 * r(var[8], var[9], var[0], var[1]) * (var[0] - var[8]) - m4 * r(var[8], var[9], var[4], var[5]) * (var[4] - var[8]))
    y5 = var[11]
    py5 = -G * (-m3 * r(var[8], var[9], var[0], var[1]) * (var[1] - var[9]) - m4 * r(var[8], var[9], var[4], var[5]) * (var[5] - var[9]))
    

    return np.array([x3, y3, px3, py3, x4, y4, px4, py4, x5, y5, px5, py5])



m3, m4, m5 = 3, 4, 5
G = 1
var = np.array([0, 4, 0, 0, -3, 0, 0, 0, 0, 0, 0, 0])
t = np.linspace(0, 70, 3e7)

var = odeint(manybody, var, t, args=(m3, m4, m5, G), full_output=False)

plt.plot(var[:, 0][::1000], var[:, 1][::1000], label="3")
plt.plot(var[:, 4][::1000], var[:, 5][::1000], label="4")
plt.plot(var[:, 8][::1000], var[:, 9][::1000], label="5")

plt.legend(loc="best")
plt.axis("scaled")
plt.xlim(-6, 4)
plt.ylim(-4, 6)
plt.show()

# fig = plt.figure()
# ax = fig.gca(projection='3d')
# ax.plot(v[:, 0], v[:, 1], v[:, 2])
# plt.show()
