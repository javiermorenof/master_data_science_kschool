###ESTADISTICA DESCRIPTIVA EJEMPLO1#######
setwd("C:/Users/ernesto.correa/Dropbox/kschool_master/Ejercicios ")
getwd()

datos<- read.table("datos_eje1.txt", header = TRUE) #CARGAMOS LOS DATOS
datos

#PREGUNTA
#Realiza una tabla de frecuencias absolutas y otra de frecuencias relativas para la variable Calificacion. 
#Almacena las tablas anteriores en dos variables y llámalas absolutas y relativas.

#SOLUCION

#Calculamos las frecuencias absolutas usamos la función table() 
absolutas <- table(datos$Calificacion)
absolutas


#Calculamos las frecuencias relativas usamos la función prop.table() "note que primero se deben calcular las frecuencias absolutas"
relativas <- prop.table(absolutas)
relativas

#DIAGRAMAS DE BARRAS PARA CALIFICACION barplot()
barplot(absolutas, col=rainbow(4),xlab="Calificaciones",ylab="Frecuencias absolutas")

#DIAGRAMAS DE BARRAS PARA SEXO barplot()
barplot(table(datos$Sexo), col=rainbow(2),xlab="Sexo",ylab="Frecuencias absolutas")

#DIAGRAMA DE SECTORES PARA CALIFICACION
pie(absolutas, col =rainbow(4))

#HISTOGRAMA PARA LA VARIABLE EDAD
hist(datos$Edad,col = "lightblue",xlab = "Edades", ylab = "Frecuencia", main = "Histograma para Edad")

#GRAFICO BOXPLOT
boxplot(datos$Edad, range = 1.5)
boxplot(datos$Edad, range = 0.5)        #cambio del alargamiento de los bigotes
boxplot(formula = Edad ~ Calificacion, data =  datos)

#Medidas de posición
summary(datos$Puntuacion)

#Por separado
#dato minimo
min(datos$Puntuacion)

#dato maximo
max(datos$Puntuacion)

#cuartiles
quantile(datos$Puntuacion, probs = c(0,0.25,0.50, 0.75,1))

#media aritmetica
mean(datos$Puntuacion)

#mediana 
median(datos$Puntuacion)

#moda NO HAY FUNCION IMPLEMENTADA
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(datos$Edad)

#ORDENAR DATOS
#DECRECIENTE
sort(datos$Edad, decreasing = FALSE)

#CRECIENTE
sort(datos$Edad, decreasing = TRUE)

#Medidas de dispersión
#Rango 

M<-max(datos$Puntuacion)
m<-min(datos$Puntuacion)
R<- M-m
R
#varianza
V<-var(datos$Edad, y=NULL, na.rm = FALSE)
s<-sd(datos$Edad, na.rm = TRUE)
V
s
#Coeficiente de variación
media<-mean(datos$Edad)
CV_Edad <- s/media
CV_Edad

#Medidas de Forma
#coeficiente de asimetria

install.packages("e1071")   # Es necesario si aún no se ha instalado
library(e1071)
skewness(datos$Edad)

#coeficiente de kurtosis 

kurtosis(datos$Edad) #comparar con el histograma de las frecuencias absolutas de Edad

