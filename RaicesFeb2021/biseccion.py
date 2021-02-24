import numpy as np
from bokeh.plotting import figure, output_file, show
from decimal import *
import math
limite = 5000
def uno(x):
    return (np.cos(x)*np.cos(x)) - x**2 

def dos(x): 
    return (x*np.sin(x))-1  

def tres(x):
    return ((2*x**2) - (4/3*x) + (8/27))**(1/3)

def cuatro(x): 
    e = math.e
    return ((68.1*9.81)/x)*(1-e**(-0.146843*x))-40

def cinco(x): 
    return (x**3)-(2*x)-5


def  bisection(f, x0, x1, tol,grafica, num_eq):
    iterador = 0
    y_vals = []
    x2=0
    errores = []
    while abs(x1-x0)>=tol and iterador < limite:
        errores.append(abs(x1-x0))
        x2=((x0+x1)/2)
        y_vals.append(x2)
        if f(x2)==0:
            resultado = []
            resultado.append(iterador)
            resultado.append(x2)
            x_vals = list(range(len(y_vals)))
            output_file(grafica + f"-{tol}.html")
            fig = figure()
            fig.line(x_vals, y_vals, line_width=2)
            show(fig)
            return resultado
        else:
            if f(x0)*f(x2)>0: 
                x0=x2
            else:
                x1=x2
        iterador+=1
    
    x_vals = list(range(len(y_vals)))
    output_file(grafica + f"-{tol}.html")
    fig = figure()
    fig.line(x_vals, y_vals, line_width=2)
    #show(fig)

    x_errores = list(range(len(errores)))
    output_file(f'erroresBisection{num_eq}-{tol}.html')
    fig2 = figure()
    fig2.line(x_errores, errores, line_width=2)
    show(fig2)

    resultado = []
    resultado.append(iterador)
    resultado.append(x2)
    return resultado
        
            
if __name__ == "__main__": 
    getcontext().prec = 56
    tols = [10**-8, 10**-16, 10**-32, 10**-56]
    x = 0.7
    i=1
    print("Para La funcion 1: \n ")

    for tol in tols:  
        retorno = bisection(uno, 0, 2,tol,"bisection1", 1)
        i+=1
        iteraciones = retorno[0]
        raiz = retorno[1]
        print(f'En {iteraciones} iteraciones y tolerancia de {tol} se obtuvieron las raices {Decimal(raiz)} y {-1*Decimal(raiz)}')


    print("Para La funcion 2: \n ")
    for tol in tols:  
        retorno = bisection(dos, -1, 2,tol,"bisection2", 2)
        iteraciones = retorno[0]
        raiz = retorno[1]
        print(f'En {iteraciones} iteraciones y tolerancia de {tol} se obtuvieron las raices {Decimal(raiz)}')


    print("Para La funcion 3: \n ")
    for tol in tols:  
        retorno = bisection(tres, -1, 0.7,tol,"bisection3", 3)
        iteraciones = retorno[0]
        raiz = retorno[1]
        print(f'En {iteraciones} iteraciones y tolerancia de {tol} se obtuvieron las raices {Decimal(raiz)}')


    print("Para La funcion 4: \n ")
    for tol in tols:  
        retorno = bisection(cuatro, 10, 16,tol,"bisection4", 4)
        iteraciones = retorno[0]
        raiz = retorno[1]
        print(f'En {iteraciones} iteraciones y tolerancia de {tol} se obtuvieron las raices {Decimal(raiz)}')


    print("Para La funcion 5: \n ")
    for tol in tols:  
        retorno = bisection(cinco, 1, 4,tol,"bisection5", 5)
        iteraciones = retorno[0]
        raiz = retorno[1]
        print(f'En {iteraciones} iteraciones y tolerancia de {tol} se obtuvieron las raices {Decimal(raiz)}')
        