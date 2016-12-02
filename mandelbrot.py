import numpy as np
import matplotlib.pyplot as plt
import seaborn as sbn

N, M = 50, 500

def mandel(X, Y):
    a, b = [0] * 2
    for i in range(N):
        a, b = a**2 - b**2 + X, 2 * a * b + Y
    return (a**2 + b**2)**-100
        
x, y = [np.linspace(-2, 2, M)] * 2
X, Y = np.meshgrid(x, y)
plt.pcolor(X, Y, mandel(X, Y))
plt.colorbar()
plt.axis("equal")
plt.show()


