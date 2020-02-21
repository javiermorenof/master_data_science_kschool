###########ESTIMADORES PUNTUALES Y POR INTERVALOS DE CONFIANZa
datos<-read.table("datos_eje4.txt", header = TRUE)
datos
#####
library(MASS)
library("stats")
library(stats4)
#############
#estimacion parametros puntuales
ajuste.normal<-fitdistr(datos$x,"normal")
ajuste.normal   #da los estimadores junto con los errores estandarme
mean(datos$x)
sd(datos$x)

mu<-ajuste.normal$estimate[1]            #asignacion de parametros
sigma<-ajuste.normal$estimate[2]
mu
sigma
#bondad del ajuste

modelo<-rnorm(100, mean = mu, sd = sigma)
modelo
qqplot(datos$x,  modelo, labels=FALSE)
qqnorm(datos$x)

####### Estimadores por intervalos
#para la media

ci<-mean(datos$x)-qnorm(0.975,0,1)*sd(datos$x)/sqrt(100) #cota inferior conf=95% var conocida
cs<-mean(datos$x)+qnorm(0.975,0,1)*sd(datos$x)/sqrt(100) #cota superior conf=95% var conocida
c(ci,cs)

#p-valor
t.test(x= datos$x, y=modelo, alternative = "greater", mu = 0) #si no ponemos alternative, por defecto lo hará a 2 colas
t.test(x= datos$x, y=modelo, mu = 0)
