import numpy as np
from scipy.fftpack import fft, ifft, fftfreq
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import seaborn as sbn


xN, tN, L = 1024, 2**10, 50
dx, dt = 1 / xN, 1 / tN
x, t = np.linspace(-L/2, L/2, xN), np.linspace(0, 1, tN)

height = 50

def V(x):
    if 0.5 < x < 1.5:
        return height
    else:
        return 0

V = np.vectorize(V)

# def V(x):
#     alpha = 1e-1
#     return 10 * np.exp(-(x - 0.5)**2/(2 * alpha**2))/np.sqrt(2 * np.pi)/alpha


def f(x):
    alpha = 0.3
    C = np.sqrt(2 * np.pi) * alpha
    return np.exp((-(x + 1.5)**2 + 1j * 1 * x) / (2 * alpha**2)) / C

k = 2 * np.pi * fftfreq(xN, d=1/xN) / L
wave_packet = f(x)
expK = np.exp(-0.5j * k**2 * dt)
expV = np.exp(1j * V(x) * dt)
plt.plot(x, np.real(wave_packet * wave_packet.conjugate()), label='initial')


fig = plt.figure()
ims = []
for _ in range(16 * 2):
    for i in range(30):
        wave_packet = ifft(fft(wave_packet * expV) * expK)

    y = [np.real(wave_packet * wave_packet.conjugate())[300:-300], V(x)[300:-300]/height]
    im = plt.plot([x[300:-300], x[300:-300]], y, 'k')
    ims.append(im)
    # wave_packet /= np.sqrt(simps(np.abs(wave_packet)**2, x))


# plt.plot(x, np.real(wave_packet * wave_packet.conjugate()), label='after')
# plt.plot(x, V(x)/height, label='potential')

Writer = animation.writers['ffmpeg']
# writer = Writer(fps=15, bitrate=1800)
writer = Writer(fps=30)
ani = animation.ArtistAnimation(fig, ims, interval=10)

# plt.legend()
ani.save("output.mp4", writer=writer)
# plt.show()
