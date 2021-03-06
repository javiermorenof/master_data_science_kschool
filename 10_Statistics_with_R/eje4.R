###########ESTIMADORES PUNTUALES Y POR INTERVALOS DE CONFIANZa
datos<-read.table("datos_eje4.txt", header = TRUE)
datos
#####
library(MASS)
library("stats")
library(stats4)
#############
#Estimacion parametros puntuales
#Antes de hacer todo esto debo saber de qu� tipo son los datos: haber hecho el qqplot, asimetria, kurtosis, etc. todo para comprobar que siguen una distribuci�n normal.
#Ajuste normal lo que hace es te calcula la media poblacional y la desviaci�n est�ndar poblacional, no la muestral.
ajuste.normal<-fitdistr(datos$x,"normal")
ajuste.normal   #da los estimadores junto con los errores estandarme
#Si compruebo ahora los datos con la media muestral y la desviaci�n est�ndar muestral veo que la media es igual, ya que la media muestral es buen estimador para la poblaci�n, pero veo que la desviaci�n est�ndar es ligeramente diferente
mean(datos$x)
sd(datos$x)

mu<-ajuste.normal$estimate[1]            #asignacion de parametros
sigma<-ajuste.normal$estimate[2]
mu
sigma

#Pueba de bondad del ajuste
#AQu� creo una normal de 100 elementos de media mu (como la que estim� mi modelo) y de desviaci�n sigma
modelo<-rnorm(100, mean = mu, sd = sigma)
modelo
#Veo que efectivmaente mis datos siguen una normal
qqplot(datos$x,  modelo, labels=FALSE)
#Aqu� hago lo mismo, pero con una normal de media 0 y desviaci�n tipica 1
qqnorm(datos$x)

####### Estimadores por intervalos (ci = cota inferior; cs = cota superior)para la media
#n el ejemplo anterior se ha calculado el valor de la media, peor qu� pasa si no quieor un n�mero concreto y quiero estimar por intervalos de confianza?
#Se estima con lo que se ver� a continuaci�n
#COmo aqu� uso qnorm estoy suponiendo que s� la  varianza de la poblaci�n.
#PEro estoy haciendo un supuesto fuerte: estoy suponiendo que sd es la varianza de la poblaci�n.
#�Qu� pasa si digo que yo tengo solamente la varianza de la muestra y que no s� la de la poblaci�n?
#Pues pasa que tengo que usar la t de STudent:

#EJemplo asumiendo que s� la desviaci�n est�ndar poblacionar
ci<-mean(datos$x)-qnorm(0.975,0,1)*sd(datos$x)/sqrt(100) #cota inferior conf=95% var conocida
cs<-mean(datos$x)+qnorm(0.975,0,1)*sd(datos$x)/sqrt(100) #cota superior conf=95% var conocida
c(ci,cs)

#Ejemplo asumiendo que realmente no la s� y que uso la t de student. Esto es lo correcto.
t.test(x= datos$x, y=modelo, alternative = "greater", mu = 0) #si no ponemos alternative, por defecto lo har? a 2 colas
t.test(x= datos$x, y=modelo, mu = 0)
