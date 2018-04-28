import numpy as np
import math as m
from random import randint, random
import matplotlib.pyplot as plt
from tqdm import tqdm

class Spin(object):
    def __init__(self):
        self.N = 64
        self.J = 1.0
        self.T = 3
        # スピンマップをランダムに初期化
        # self._spin_map = np.random.choice([-1, 1], size=(self.N, self.N))
        self._spin_map = np.ones((self.N, self.N))
        self._spin_map_p = [None] * 4
        self.Energy = None
        self.Mag = None

    def get_spin(self, x, y):
        if x == len(self._spin_map[0]):
            x = 0
        if y == len(self._spin_map[0]):
            y = 0
        return self._spin_map[x][y]

    def set_energy(self):
        self._spin_map_p[0] = np.vstack((self._spin_map[1:], self._spin_map[0]))
        self._spin_map_p[1] = np.vstack((self._spin_map[-1], self._spin_map[:-1]))
        self._spin_map_p[2] = np.hstack((self._spin_map[:, -1:], self._spin_map[:, :-1]))
        self._spin_map_p[3] = np.hstack((self._spin_map[:, 1:], self._spin_map[:, :1]))
        self.Energy = -self.J * np.sum(np.sum(self._spin_map_p, 0) * self._spin_map)

    def set_mag(self):
        self.Mag = abs(np.sum(self._spin_map)) / (self.N**2)

    def flip_spin(self):
        # 弄るサイト
        X = randint(0, self.N-1)
        Y = randint(0, self.N-1)

        EnergyBefore = -self.J * self._spin_map[X][Y] * (
            self.get_spin(X+1, Y) +
            self.get_spin(X-1, Y) +
            self.get_spin(X, Y+1) +
            self.get_spin(X, Y-1))

        if EnergyBefore > 0 or m.exp(2 * EnergyBefore / self.T) > random():
            # 反転したらエネルギー的に安定
            # もしくは不安定でも確率的に反転
            self._spin_map[X][Y] = -self._spin_map[X][Y]


def main():
    hoge = Spin()
    # plt.imshow(hoge._spin_map)
    # plt.show()

    mag = []
    T_arr = np.linspace(0.01, 5, 50)
    for T in tqdm(T_arr):
        hoge.T = T
        for i in range(100000):
            hoge.flip_spin()

        # plt.imshow(hoge._spin_map)
        # plt.show()
        hoge.set_mag()
        mag.append(hoge.Mag)

    # plt.imshow(hoge._spin_map)
    # plt.show()

    plt.plot(T_arr, mag, 'x')
    plt.show()

main()








