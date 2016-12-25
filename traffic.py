import numpy as np
from scipy.integrate import odeint
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import seaborn as sbn


def trafficJam(f, t, a, c, L):
    """
    f = [x, v]
    """
    x, v = f[:N], f[N:]

    def V(h):
        return np.tanh(h - c) + np.tanh(c)

    x_dot = v
    x = np.append(x, [x[0]])
    diff_x = np.diff(x)
    diff_x = np.array([i if i > 0 else L + i for i in diff_x])

    v_dot = a * (V(diff_x) - v)

    return np.append(x_dot, v_dot)


N, a, c, L = 30, 1.3, 2, 60
x = np.arange(N) * L / N
v = [(1 + np.tanh(c)) / 2] + ([1 + np.tanh(c)] * (N - 1))
t = np.arange(0, 200, 0.1)

var = odeint(trafficJam, np.append(x, v), t, args=(a, c, L), full_output=False)
x_arr, v_arr = var[:, :N], var[:, N:]

r = L / (2 * np.pi)
fig = plt.figure(figsize=(6, 6))
sbn.set_style('ticks')
ims = []
for index, x in enumerate(x_arr):
    theta = x / r
    im = plt.plot(r * np.cos(theta), r * np.sin(theta), '.k', markersize=12)
    ims.append(im)

Writer = animation.writers['ffmpeg']
# writer = Writer(fps=15, bitrate=1800)
writer = Writer(fps=60)
ani = animation.ArtistAnimation(fig, ims, interval=10)

# plt.legend()
ani.save("output.mp4", writer=writer)
# plt.show()

# plt.legend(loc="best")
# plt.axis("scaled")
# plt.xlim(-6, 4)
# plt.ylim(-4, 6)
# plt.show()

# fig = plt.figure()
# ax = fig.gca(projection='3d')
# ax.plot(v[:, 0], v[:, 1], v[:, 2])
# plt.show()
