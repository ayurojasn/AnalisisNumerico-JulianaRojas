import math

def interseccion(x0,x1,e):
    
    error = 1.0
    cont = 0
    while(error > e):
        x1 = x1 - diff(x1-1)*((x1-1) - (x1-2)) / (diff(x1-1) - diff(x1-2))
        error = abs((f(x1) - g(x1)))
        cont += 1
        
        print("Numero de iteraciones: " + str(cont))
        print("Error: "+ str(error))
        print("f(x)= "+ str(f(x1)))
        print("g(x)= "+ str(g(x1))+"\n")
      
   
    return 0, 0 

def f(x):
    return (x**2)
    
def g(x):
    return 1 + math.cos(x)
    
def diff(x):
    return f(x)-g(x)
    
def main():
    interseccion(2,1,1e-9)
        
if __name__ == "__main__":
    main()
