<!---
	NumPy・SciPyによる高速化の実力を見る
-->

# あらまし
以前の記事でNumPy・SciPyの高速化にまつわる事柄を書きました:

[NumPyを用いた数値計算の高速化 : 基礎](http://qiita.com/jabberwocky0139/items/c3620fb2f011f20a633b)

[NumPy・SciPyを用いた数値計算の高速化 : 応用その1](http://qiita.com/jabberwocky0139/items/a9751d11caa64bc19226)

[NumPy・SciPyを用いた数値計算の高速化 : 応用その2](http://qiita.com/jabberwocky0139/items/26451d7942777d0001f1)

ホントに早くなってるの？ちゃんと調べてみましょう.

## 調査方法
Pythonによるオレオレ実装と比較します. 速度よりシンプルさを重視した実装との比較なので正当な評価とは言い難いかもしれません. Pythonはanaconda3, 時間計測にはIPythonの`%timeit`を使用します. 

--実行環境--

OS : Ubuntu16.04 LTS 64bit
Python : anaconda3-4.1.1
CPU : Intel Corei5 3550


# 行列積
線形代数の演算はいろいろありますが, 簡単な行列積で試してみましょう. 200×200の行列を2つ用意して積を計算します:
```py3
import numpy as np
from numpy.random import rand

N = 200
a = np.array(rand(N, N))
b = np.array(rand(N, N))
c = np.array([[0] * N for _ in range(N)])
```

行列積の定義のまま実装すると
```py3
for i in range(N):
	for j in range(N):
		for k in range(N):
			c[i][j] = a[i][k] * b[k][j]
```
でしょうか. 実行時間は**<font color="HotPink">7.7s</font>**.

NumPyのユニバーサル関数を用いると
```py3
c = np.dot(a, b)
```
速度以前に単純なのがいいですね. 実行時間はなんと**<font color="DodgerBlue">202us</font>**. 何倍とか言うのもおこがましいくらい一瞬ですね. これがMKLのBLASによるマルチコア処理の威力です. 1000×1000でも**<font color="DodgerBlue">22.2ms</font>**. ここまでくるとforループの実装では手に終えなくなります.

# 微分
$\sin x$を微分してみましょう. 空間の分割数は100000にしています:
```py3
import math as m

def f(x):
    return m.sin(x)
	
N, L = 100000, 2 * m.pi
x, dx = np.linspace(0, L, N), L / N
```

微分の定義をそのまま. 精度はこの際気にせずいきましょう:
```py3
diff = []
for i in x:
	diff.append((f(i + dx) - f(i)) / dx)
```
実行時間は**<font color="HotPink">91.9ms</font>**. もしかしたら`append`はあんまり速くないかもしれないですね. 一方NumPyでは

```py3
def g(x):
    return np.sin(x)
	
diff = np.gradient(g(x), dx)
```
シンプルな上に, 戻り値の要素数も減りません(数値計算に際してはこれがどれだけ有り難いか！). 実行時間は**<font color="DodgerBlue">8.25ms</font>**. 軽く10倍程度は速いですね. ちなみに

```py3
g = np.vectorize(f)
```
とすることで, 引数や戻り値をndarray仕様に変換することもできます. 

# 積分
ちょっとコアな積分を用意しましょう:
$$
\int_{-\infty}^{\infty} dx\int_{-\infty}^{x} dy\; e^{-(x^2 + y^2)}
$$
$y$の積分区間に$x$が含まれているような二重積分です. 被積分関数と空間の分割数を
```py3
def h(x, y):
    return np.exp(-(x**2 + y**2))

N, L = 2000, 100
x, dx = np.linspace(-L/2, L/2, N), L / N
y, dy = np.linspace(-L/2, L/2, N), L / N
```
としましょう. 二重積分を定義通り実装すると
```py3
ans = 0
for index, X in enumerate(x):
	for Y in y[:index+1]:
		ans += dx * dy * h(X, Y)
```
でしょうか. 実行時間は**<font color="HotPink">5.89s</font>**. しかしこれはあまりにも精度が悪いです. 自前で書くならSimpson積分にすべきですが, その場合はコードがそこそこ煩雑になります. また, Simpson積分でも広義積分には「空間を十分広く取る」という対応しかできません. 一方SciPyの`dblquad`ではその全ての問題を解決できています:

```py3
from scipy.integrate import dblquad
ans = dblquad(h, a = -np.inf, b = np.inf, gfun = lambda x : -np.inf, hfun = lambda x : x)[0]
```
$x$の積分区間が$[a, b]$, $y$の積分区間が$[{\rm gfun}, {\rm hfun}]$です. 適応型積分で, 誤差の範囲も指定できます. `dblquad`の戻り値はtupleで, 絶対誤差も一緒に返って来るようです. 実行時間は**<font color="DodgerBlue">51.1ms</font>**. もはや何も言うことはありません. 最高です.

# 固有値方程式
(これはおまけです. 固有値方程式の数値解法は少々マニアックなので...)

調和振動子系のSchroedinger方程式を解きましょう. 自前で実装するとなるとJacobi法でしょうか. 興味があれば調べてみてください:
```py3
I = np.eye(N)
H = [[0 for i in range(N)] for j in range(N)]
for i in range(N):
    H[i][i] = 2.0/(2*dx**2) + 0.5*(-L/2+dx*i)**2
    if(0 <= i+1 < N):
        H[i][i+1] = -1.0/(2*dx**2)
    if(0 <= i-1 < N):
        H[i][i-1] = -1.0/(2*dx**2)
H = np.array(H)

# Jacobi法
flag = True
while(flag):
    # 非対角成分の最大値及びインデックスを調べる
    maxValue = 0
    cI, rI = None, None
    for j in range(N):
        for i in range(j):
            if(maxValue < abs(H[i][j])):
                maxValue = abs(H[i][j])
                rI, cI = i, j
                
    # 収束判定
    if(maxValue < 1e-4):
        flag = False
	# print(maxValue)
    
    # 回転行列の用意
    theta = None
    if(H[cI][cI] == H[rI][rI]):
        theta = m.pi/4
    else:
        theta = 0.5*m.atan(2.0*H[rI][cI]/(H[cI][cI]-H[rI][rI]))
        J = np.eye(N)
        J[rI][rI] = m.cos(theta)
        J[cI][cI] = m.cos(theta)
        J[rI][cI] = m.sin(theta)
        J[cI][rI] = -m.sin(theta)
    
    # 行列演算
    H = np.array(np.matrix(J.T)*np.matrix(H)*np.matrix(J))
    I = np.array(np.matrix(I)*np.matrix(J))
    
# 固有値・固有ベクトルの格納
v, w = I.transpose(), []
for i in range(N):
    w.append([H[i][i], i])
w.sort()
```
非対角項の最大値が十分小さくなったところで収束です. 固有値が昇順になっていなかったりと, あまり便利ではありません. 書くのもかったるいです. 実行時間は**<font color="HotPink">15.6s</font>**. これ以上分割数を大きくするともう苦しいです. NumPyでは

```py3
# 系の設定
L, N = 10, 80
x, dx = np.linspace(-L/2, L/2, N), L / N

# 運動項K
K = np.eye(N, N)
K_sub = np.vstack((K[1:], np.array([0] * N)))
K = dx**-2 * (2 * K - K_sub - K_sub.T)

# ポテンシャル項
V = np.diag(np.linspace(-L/2, L/2, N)**2)

# エルミート行列の固有値方程式
# wが固有値, vが固有ベクトル
H = (K + V) / 2
w, v = np.linalg.eigh(H)
```
実行時間は**<font color="DodgerBlue">1.03ms</font>**. 固有値方程式くらい複雑なアルゴリズムになるともう色んな意味で勝ち目が無いですね. 圧倒的です.


# おわりに
このくらいで十分でしょう. NumPy・SciPyがいかに高速に動作するか, またシンプルに記述できるかがわかると思います. もしC/C++で上のような計算を外部ライブラリに頼らずに書こうと思うと, それぞれの章の最初で書いたようなコードになります. また, C/C++の外部ライブラリは経験上, 触りにくいものが多い印象です. 

あくまで参考ですが速度差をまとめると以下のような具合です:

行列積 : **<font color="HotPink">7.7s</font>** → **<font color="DodgerBlue">202us</font>**

微分 : **<font color="HotPink">91.9ms</font>** → **<font color="DodgerBlue">8.25ms</font>**

(2重)積分 : **<font color="HotPink">5.89s</font>** → **<font color="DodgerBlue">51.1ms</font>**

固有値方程式 : **<font color="HotPink">15.6s</font>** → **<font color="DodgerBlue">1.03ms</font>**

圧倒的です. 計算屋さんがPythonを使う価値は十二分にあると思います. 
