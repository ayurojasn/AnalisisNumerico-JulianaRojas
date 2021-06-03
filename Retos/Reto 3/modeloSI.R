library(readxl)
library(clusteval)


data <- read_excel("C:\\Users\\david\\OneDrive\\Escritorio\\ebola.xlsx", sheet = "Guinea2")

SI <- function(t, x, parametros){
  with(as.list(c(parametros, x)),{
    dS <- - r*beta*S*I/(S+I)
    dI <- + r*beta*S*I/(S+I)
    derivadas <- c(dS, dI)
    return(list(derivadas))
  })
}

library(deSolve)
#Definimos los parametros del modelo, r y beta
parametros <- c(r=2, beta=.14)
#Definimos los valores iniciales, S e I
v_iniciales <- c(S=5019222, I=482)
#Definimos  t 
dt <- seq(0, 20, 0.1)
#Mediante"ode"resolvemos y generamos un data frame 
simulacion.si <- as.data.frame(ode(y=v_iniciales,
                                   times=dt,func=SI,parms=parametros))
#attach:referencia directa a las columnas en simulacion.si 
attach(simulacion.si)
#Calculamos del tamaño de la población: N
N <- sum(v_iniciales)
#Representamos graficamente 
plot(dt, S, type="l", col="blue", 
     ylim=c(0,sum(v_iniciales)), 
     xlab="tiempo (en meses)", 
     ylab="número de individuos")
lines(dt, I, type="l", col="red")
x = seq(1,length(data$"Infectados M"),1)
y = data$"Infectados M"
lines(x,y,type ="l",col="orange")
x = seq(1,length(data$"Suceptibles M"),1)
y = data$"Suceptibles M"
lines(x,y,type ="l",col="purple")
title("Modelo SI: r = 2, beta = 0.14, S = 5000000, I = 482")

library(deSolve)
library(EpiDynamics)
#Error
parametros <- c(r=2, beta=.14)
v_iniciales <- c(S=5019222, I=482)
dt <- seq(0, 20, 0.1)
simulacion.si <- as.data.frame(ode(y=v_iniciales,
                                   times=dt,func=SI,parms=parametros))
paramm <- c(beta=.14, gamma = 0)
v_0 <- c(S=501922, I=482, R = 0)
result = EpiDynamics::SIR(pars = paramm, init = v_0, time = dt)
sol = ode(y=v_iniciales, times=dt, func=SI,parms=parametros, method = "euler")
valODESuceptibles = c(sol[,2])
valEpiSuceptibles = c(result$results[,2])
errores = c()
i = 1
max = length(valODESuceptibles)
while(i <= max){
  errorActual =(abs(valODESuceptibles[i] - valEpiSuceptibles[i] )/valEpiSuceptibles[i]) 
  errores[i] = errorActual
  i = i + 1
}
plot(dt, errores, type="l", col= "red", xlab = "Tiempo (en Horas)", ylab = "Error relativo")
title("Error relativo para Suceptibles")

valODEInfectados = c(sol[,3])
valEpiInfectados = c(result$results[,3])
errores = c()
i = 1
max = length(valODEInfectados)
while(i <= max){
  errorActual = (abs(valODEInfectados[i] - valEpiInfectados[i] )/valEpiInfectados[i])
  errores[i] = errorActual
  i = i + 1
}
plot(dt, errores, type="l", col= "blue", xlab = "Tiempo (en Horas)", ylab = "Error relativo")
title("Error relativo para Infectados")