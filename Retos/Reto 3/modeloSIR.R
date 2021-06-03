library(readxl)
library(clusteval)
data <- read_excel("C:\\Users\\david\\OneDrive\\Escritorio\\ebola.xlsx", sheet = "Guinea2")
SIR <- function(t, x, parametros){
  with(as.list(c(parametros, x)),{
    dS <- -(beta*S*I)
    dI <- +(beta*S*I)- gamma*I
    dR <- gamma*I
    
    derivadas <- c(dS, dI, dR)
    return(list(derivadas))
  })
}
library(deSolve)
parametros <- c(beta= .014, gamma = .00014)

v_iniciales <- c(S=5019222, I=482, R=195)

dt <- seq(0, 20, 0.1)

sol = ode(y=v_iniciales, times=dt, func=SIR,parms=parametros, method = "euler")

result = EpiDynamics::SIR(pars = parametros, init = v_iniciales, time = dt)

simulacion.si <- as.data.frame(sol)
attach(simulacion.si)
N <- sum(v_iniciales)
plot(dt, S, type="l", col="blue", ylim=c(0,sum(v_iniciales)), xlab = "Tiempo (meses)", ylab="Numero de individuos")
lines(dt, I, type="l", col="red")
lines(dt, R, type="l", col="green")
lines(dt, result$results[,2], type = "l", col = "brown")
lines(dt, result$results[,3], type = "l", col = "orange")
lines(dt, result$results[,4], type = "l", col = "purple")
x = seq(1,length(data$"Infectados M"),1)
y = data$"Infectados M"
lines(x,y,type ="l",col="cyan")
x = seq(1,length(data$"Suceptibles M"),1)
y = data$"Suceptibles M"
lines(x,y,type ="l",col="pink")
x = seq(1,length(data$"Recuperados M"),1)
y = data$"Recuperados M"
lines(x,y,type ="l",col="blue")
title("Modelo SIR")


library(deSolve)
library(EpiDynamics)

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

valODERecuperados = c(sol[,4])
valEpiRecuperados = c(result$results[,4])
errores = c()
i = 1
max = length(valODERecuperados)
while(i <= max){
  errorActual = (abs(valODERecuperados[i] - valEpiRecuperados[i] )/valEpiRecuperados[i])
  errores[i] = errorActual
  i = i + 1
}
plot(dt, errores, type="l", col= "blue", xlab = "Tiempo (en Horas)", ylab = "Error relativo")
title("Error relativo para Recuperados")