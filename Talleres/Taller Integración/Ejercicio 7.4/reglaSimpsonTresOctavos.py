import numpy as np
import math


fx = lambda x: math.e **(-x**2)


def simpson38(f,a,b):
    m1 = (2 * a + b)/3
    m2 = (a + 2 * b)/3
    integral = ( b - a )/8 * (f(a) + 3 * f(m1) + 3 *f(m2) + f(b))
    return integral


a = 0
b = 1.5
tramos = 8
h = (b - a) / tramos
suma = 0

for i in range(tramos):
    b = a + h
    area = simpson38(fx, a, b)
    suma = suma + area
    a = b

# SALIDA
print("Simpson3/8")
print('tramos:', tramos)
print('Integral: ', (2/np.sqrt(math.pi)) * suma)
