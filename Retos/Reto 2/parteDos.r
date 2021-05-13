library(readxl)
library(clusteval)

#Lectura de Datos
data <- read_excel("C:\\Users\\USER\\OneDrive\\Memoria de Trabajo\\Reto2\\data\\coordenadas.xls")
x = data$"X"
y = data$"Y"
ciudades = data$"Estaciones"

#Encontrar el indice de la ciudad de Sobral
i = 1
for(val in ciudades){
  if(val == "Sobral"){
    break
  }
  i = i+1
}

#Capturar las coordenadas de Sobral
x_sobral = x[i]
y_sobral = y[i]

#Encontrar las distancias
indice_sobral = i
i = 1
min = 10000
indice_ganador = 0
writeLines("Distancias")
for(val in x){
  if(i != indice_sobral){
    distancia = sqrt((x[i]-x_sobral)^2 + (y[i]-y_sobral)^2)
    cat(ciudades[i], " ", distancia, "\n")
    if(min > distancia){
      min = distancia
      indice_ganador = i
    }
  }
  i = i + 1
}
cat("La ciudad más cercana a Sobral es: ", ciudades[indice_ganador])

#IObtención e interpolación de datos de datos de la ciudad real
dataSobral <- read_excel("C:\\Users\\USER\\OneDrive\\Memoria de Trabajo\\Reto2\\data\\datos.xls", sheet = "Sobral")
x_sobral = (dataSobral$"Dia Juliano"+(dataSobral$"Hora"/2400))
y_sobral = dataSobral$"Temp. Interna (ºC)"
plot(x_sobral, y_sobral, type = "points", main = "Temperaturas", ylab = "Temperatura (ºC)", xlab = "Día de Medicion")

#Obtención de los datos de la ciudad más cercana
dataQuiteria <- read_excel("C:\\Users\\USER\\OneDrive\\Memoria de Trabajo\\Reto2\\data\\datos.xls", sheet = "Santa Quitéria")
x_quiteria = (dataQuiteria$"Dia Juliano"+(dataQuiteria$"Hora"/2400))
y_quiteria = dataQuiteria$"Temp. Interna (ºC)"


#Eliminación de un sexto de los datos de la ciudad más cercana
x_interpolacion = c()
y_interpolacion = c()

i = 1
j = 1
for(dato in y_quiteria){
  if(i%%6 != 0){
    x_interpolacion[j] = x_quiteria[i]
    y_interpolacion[j] = y_quiteria[i]
    j = j+1
  }
  i = i+1
}

#Splines
spline = spline(x_interpolacion, y_interpolacion)
lines(spline, col="red")

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
funcionReal = approxfun(x_sobral, y_sobral)

i = 1
for(val in x_sobral){
  errorRelativoSpline[i] = abs(funcionReal(val) - funcionSpline(val))/funcionReal(val)
  errorRelativoLineal[i] = abs(funcionReal(val) - funcionLineal(val))/funcionReal(val)
  errorAbsolutoLineal[i] = abs(funcionReal(val) - funcionLineal(val))
  errorAbsolutoSpline[i] = abs(funcionReal(val) - funcionSpline(val))
  
  i = i+1
}

#Impresión de Errores
writeLines("Errores")

cat("Método de Splines")
cat("Error Máximo Absoluto: ", max(errorAbsolutoSpline))
cat("Error Máximo Relativo: ", max(errorRelativoSpline))
cat("Error Mínimo Absoluto: ", min(errorAbsolutoSpline[errorAbsolutoSpline != 0]))
cat("Error Mínimo Relativo: ", min(errorRelativoSpline[errorAbsolutoSpline != 0]))
cat("Media del Error Absoluto", mean(errorAbsolutoSpline))
cat("Media del Error Relativo: ", mean(errorRelativoSpline))

cat("Método de Interpolación Lineal")
cat("Error Máximo Absoluto: ", max(errorAbsolutoLineal))
cat("Error Máximo Relativo: ", max(errorRelativoLineal))
cat("Error Mínimo Absoluto: ", min(errorAbsolutoLineal[errorAbsolutoLineal != 0]))
cat("Error Mínimo Relativo: ", min(errorRelativoSpline[errorAbsolutoLineal != 0]))
cat("Media del Error Absoluto", mean(errorAbsolutoLineal))
cat("Media del Error Relativo: ", mean(errorRelativoLineal))

