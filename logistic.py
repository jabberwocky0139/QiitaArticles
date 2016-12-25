import numpy as np
import matplotlib.pyplot as plt
import seaborn as sbn
# from collections import deque


def logistic(a):
    x = [0.8]
    for i in range(400):
        x.append(a * x[-1] * (1 - x[-1]))
    return x[-100:]

for a in np.linspace(2.0, 4.0, 1000):
    x = logistic(a)
    plt.plot([a]*len(x), x, "c.", markersize=2.5)

plt.show()
