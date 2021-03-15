#Pontificia Universidad Javeriana
#Facultad de Ingeniería
#Carrera de Ingeniería de Sistemas
#Docente: Eddy Herrera Daza
#Presentado por: Paula Juliana Rojas, Camilo Andrés Buitrago, David Santiago Meneses y Juan Carlos Suárez

import math
from decimal import *
from bokeh.plotting import figure, output_file, show

limite = 5000

#Primera Función despejada para y
def f1(x):
    return (10 - x**2)/x

#Segunda función despejada para y
def f2(x):
    return (-1 + math.sqrt(1 + 684*x))/(6*x)

#Resta de las dos funciones anteriores, así su raíz será la solución del sistema
def f(x):
    return ((-1 + math.sqrt(1 + 684*x))/(6*x))-((10 - x**2)/x)

#Derivada de la función anterior
def df(x):
    return  -(math.sqrt(1 + 684*x) - 1)/(6*x**2) + (10 - x**2)/x**2 + 57/(x *math.sqrt(1 + 684 *x)) + 2

#Implementación del algoritmo de Newton
def newton(funcion,derivada,x,e,max):
    #Para graficación
    output_file("grafico.html")
    fig = figure()
    y_vals = []
    y_vals.append(x)
    for i in range(max):
        resul = funcion(x)
        if abs(resul) < e:
            print(f'Se emplearon {i} iteraciones para hallar el resultado')
            x_vals = list(range(i+1))
            fig.line(x_vals, y_vals, line_width=2)
            show(fig)
            return x
        else:
            resul_derivada = derivada(x)
            if resul_derivada == 0:
                print(f'El método no logró hallar el resultado')
                return 0
            x = x - resul/resul_derivada
            y_vals.append(x)
    print(f'Se excedió el número máximo de iteraciones permitidas')
    return 0


if __name__ == "__main__": 
    #Se coloca la precisión requerida, 2^-16
    getcontext().prec = 16
    #Se halla el punto en x
    interseccion_x = newton(f,df,4,2e-16,5000)
    #Se imprimen las coordenadas de la solución
    print(f'Las curvas se intersectan en el punto {Decimal(interseccion_x)},{Decimal(f2(interseccion_x))}')
 