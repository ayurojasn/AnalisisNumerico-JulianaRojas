import numpy as np
import math


fx = lambda x:  math.e **(-x**2)


def trapecio(a,b,tramos):
    h = (b-a)/tramos
    xi = a
    suma = fx(xi)
    for i in range(0,tramos-1,1):
        xi = xi + h
        suma = suma + 2*fx(xi)
    suma = suma + fx(b)
    area = h*(suma/2)   
    return area


a = 0
b = 1.5
tramos = 8



# SALIDA
print("Trapecio")
print('tramos: ', tramos)
print('Integral: ', (2/np.sqrt(math.pi)) * trapecio(a,b,tramos))