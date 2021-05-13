import numpy as np
import math

def f(x): return math.e **(-x**2)
 

def BooleRule(a, b,n):
    h = ((b - a) / n)
    sum = 0
    bl = (7 * f(a) + 32 * f(a + h) + 12 *
        f(a + 2 * h)+32 * f(a + 3 * h)+7 *
        f(a + 4 * h))* 2 * h / 45
    sum = sum + bl
    return sum
 


a = 0
b = 1.5
tramos = 5



print("Bool")
print('tramos:', tramos)
print('Integral: ', (2/np.sqrt(math.pi)) * BooleRule(a, b,tramos))

 
