library(readxl)
library(clusteval)

#Lectura de Datos
data <- read_excel("C:\\Users\\USER\\OneDrive\\Memoria de Trabajo\\Reto2\\data\\datos.xls", sheet = "Sobral")

#Graficar del conjunto de datos conocidos
x = (data$"Dia Juliano"+(data$"Hora"/2400))
y = data$"Temp. Interna (ºC)"
plot(x,y, type = "points", main = "Temperatura Sobral", ylab = "Temperatura (ºC)", xlab = "Día y Hora")
#plot(x,y, type = "line", main = "Temperatura Sobral", ylab = "Temperatura (ºC)", xlab = "Día y Hora")

#Selección de datos para pruebas y datos para interpolación
x_interpolacion = c()
y_interpolacion = c()

i = 1
j = 1
for(dato in y){
  if(i%%6 != 0){
    x_interpolacion[j] = x[i]
    y_interpolacion[j] = y[i]
    j = j+1
  }
  i = i+1
}

#Interpolación
#Spline
spline = spline(x_interpolacion, y_interpolacion)
#lines(spline, col="red")

#Interpolación Lineal
lineal = approx(x_interpolacion, y_interpolacion, method = "linear", n = length(x_interpolacion))
lines(lineal, col="green")

#Calculo de Errores
errorRelativoSpline = c()
errorRelativoLineal = c()
errorAbsolutoSpline = c()
errorAbsolutoLineal = c()

funcionSpline = splinefun(x_interpolacion, y_interpolacion)
funcionLineal = approxfun(x_interpolacion, y_interpolacion)

i = 1
for(val in x){
  errorRelativoSpline[i] = abs(y[i] - funcionSpline(val))/y[i]
  errorRelativoLineal[i] = abs(y[i] - funcionLineal(val))/y[i]
  errorAbsolutoLineal[i] = abs(y[i] - funcionLineal(val))
  errorAbsolutoSpline[i] = abs(y[i] - funcionSpline(val))
  
  i = i+1
}

#Presentación de Errores
writeLines("Errores")

cat("Método de Splines")
cat("Error Máximo Absoluto: ", max(errorAbsolutoSpline))
cat("Error Máximo Relativo: ", max(errorRelativoSpline))
cat("Error Mínimo Absoluto: ", min(errorAbsolutoSpline[errorAbsolutoSpline != 0]))
cat("Error Mínimo Relativo: ", min(errorRelativoSpline[errorAbsolutoSpline != 0]))
cat("Media del Error Absoluto", mean(errorAbsolutoSpline))
cat("Media del Error Relativo: ", mean(errorRelativoSpline))
jacSpline = c()
i=1
for(val in x){
  jacSpline[i] = funcionSpline(val)
  i = i + 1
}
cat("Índice de Jaccard: ", cluster_similarity(y, jacSpline, similarity="jaccard", method="independence"))

cat("Método de Interpolación Lineal")
cat("Error Máximo Absoluto: ", max(errorAbsolutoLineal))
cat("Error Máximo Relativo: ", max(errorRelativoLineal))
cat("Error Mínimo Absoluto: ", min(errorAbsolutoLineal[errorAbsolutoLineal != 0]))
cat("Error Mínimo Relativo: ", min(errorRelativoSpline[errorAbsolutoLineal != 0]))
cat("Media del Error Absoluto", mean(errorAbsolutoLineal))
cat("Media del Error Relativo: ", mean(errorRelativoLineal))
jacLineal = c()
i = 1
for(val in x){
  jacLineal[i] = funcionLineal(val)
  i = i + 1
}
cat("Índice de Jaccard: ", cluster_similarity(y, jacLineal, similarity="jaccard", method="independence"))
