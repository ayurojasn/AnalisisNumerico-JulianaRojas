from scipy import optimize
import sympy as sym

x=sym.Symbol('x')
y=sym.Symbol('y')

resp=sym.solve([x**2+x*y-10, y+3*x*y**2-57], dict=True)
print(resp)



