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
library(EpiDynamics)
parametros <- c(beta= .014, gamma = .00014)

v_iniciales <- c(S=5019222, I=482, R=195)

dt <- seq(0, 20, 0.1)

sol = ode(y=v_iniciales, times=dt, func=SIR,parms=parametros, method = "rk4")

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