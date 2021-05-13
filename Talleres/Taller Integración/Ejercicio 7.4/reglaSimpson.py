import numpy as np
import math

fx = lambda x: math.e **(-x**2)


def Simpson(a,b,tramos):
    h = (b-a)/tramos
    xi = a
    area = 0
    for i in range(0,tramos,2):
        deltaA = (h/3)*(fx(xi)+4*fx(xi+h)+fx(xi+2*h))
        area = area + deltaA
        xi = xi + 2*h
    return area

a = 0
b = 1.5
tramos = 8

print("Simpson")
print('tramos:', tramos)
print('Integral: ', (2/np.sqrt(math.pi)) * Simpson(a,b,tramos))