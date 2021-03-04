#función f(x)=e^x cosx
import math

def aproximacionTaylor(x):
    
    resReal = math.e ** x * (math.cos(x))
    resAprox = 0.0
    grado = 1
   
    x =  0.005
    
    for i in range(grado):
        resAprox = resAprox + (((math.e ** x * (math.cos(x))) * (x ** i)) / math.factorial(i))
        
    print("El resultado real para x = 0.005 es igual a: ", round(resReal, 9))

    x =  0.0001
    
    for i in range(grado):
        resAprox = resAprox + (((math.e ** x * (math.cos(x))) * (x ** i)) / math.factorial(i))
        
    print("El resultado real para x = 0.0001 es igual a: ", round(resReal, 9))

    x =  0.999999999
    
    for i in range(grado):
        resAprox = resAprox + (((math.e ** x * (math.cos(x))) * (x ** i)) / math.factorial(i))
        
    print("El resultado real para x = 0.999999999 es igual a: ", round(resReal, 9))

if __name__ == "__main__":
    aproximacionTaylor(1)
