from scipy import optimize

def f(x):

    return ((x**3) - (2*x**2)+((4*x)/3)-8/27)

root = optimize.brentq(f,-1,0.7)

#root = optimize.brentq(f,0.7,1)

print(root)

