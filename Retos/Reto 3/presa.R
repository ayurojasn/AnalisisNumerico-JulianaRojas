install.packages(deSolve)
install.packages(lattice)
library("xlsx")
library(deSolve)
library(lattice)
library(readxl)
## Modelo Lotka-Volterra presa-depredador
# N: presa
# P: depredador


data <- read_excel("C:\\Users\\USUARIO\\Downloads\\Libro1.xlsx", sheet = "Fox-Rabbits")

LVM <-function(t, y, p) {
  N<-y[1]
  P<-y[2]
  with(as.list(p), {
    dNdt <- r*N - a*P*N
    dPdt <- f*P*N - b*P
    return(list(c(dNdt, dPdt)))
  })
}


r<- 0.5
a<- 0.01
f<- 0.02
b<- 0.3



p<- c(r=r, a=a, b=b, f=f)
y0<- c(N = 25, P = 5)
times<- seq(0, 80, 1)
LVM.out<- ode(y=y0, times, LVM, p)

errorExample <- data[1]
errorData <- abs(data - LVM.out)
errorData[1] <- errorExample




#Real Data
matplot(data[,1], (data[,2:3]), type="l", 
        xlim=c(0,80), ylim=c(0,200), xlab="Tiempo", ylab="Tama�o poblacional Real")
legend("topright", legend=c("Presa", "Depredador"),col=c("black", "red"), lty=1:2, cex=0.8, box.lty=0)
plot(LV.out[,2], LV.out[,3],type="l",x)

#Fake Data
matplot(LVM.out[,1], (LVM.out[,2:3]), type="l", 
        xlim=c(0,80), ylim=c(0,200), xlab="Tiempo", ylab="Tama�o poblacional Fake")
legend("topright", legend=c("Presa", "Depredador"),col=c("black", "red"), lty=1:2, cex=0.8, box.lty=0)
plot(LV.out[,2], LV.out[,3],type="l",x)
