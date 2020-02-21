##################################################
# Distribuciones
##################################################


library(help = "stats")
library("stats")
library(stats4)
#################################################
##############################################
#DADO
dadoBueno=sample(c('1','2','3','4','5','6'), 100, replace = TRUE); 
table(dadoBueno);
dadoBueno
barplot(table(dadoBueno))

dadoTrucoNum=sample(c(1:6), 1000, replace = TRUE,prob = c(2,3,1,9,8,14))
table(dadoTrucoNum);
dadoTrucoNum
barplot(table(dadoTrucoNum))
summary(table(dadoTrucoNum))

################################################
#Variable discreta
estado_informe<-c(1,2,1,2,2,2,3,3,1,4,2,2,2,3,1,4,3,2,1,1,1)
table(estado_informe)
estado_informe<-factor(estado_informe, labels=c("correcto","salvedades1","salvedades2","incorrecto"))
table(estado_informe)
prop.table(table(estado_informe))
round(100*prop.table(table(estado_informe)),1)
barplot(table(estado_informe))
barplot(table(estado_informe),col=c("blue","grey","brown","green"),main="Informe Auditoria")
##############################################

#--Funciones de probabilidad, densidad distribucion.


dbinom(2,size=10,prob=0.2) #Probabilidad que una binomial(10,0.2) tome el valor 2 es,
pbinom(2,size=10,prob=0.2) #Probabilidad que una binomial(10,0.2) tome un valor inferior a 2
qbinom(0.9,size=10,prob=0.2) # que valor de una binomial(10,0.2) presenta una probabilidad acumulada de 0.9 ?
rbinom(2000,size=10,prob=0.2) # generar 2000 valores aleatorios de una distribucion binomial(10,0.2)



################################################

#Normal
a<-rnorm(1000, mean = 0, sd = 1)
hist(a)
boxplot(a)
stem(a)
summary(a)
mean(a);
sd(a);
var(a);
quantile(a, probs = seq(0, 1, 0.25))
quantile(a, probs = c(0.1, 0.5, 1))

##############################################

#Varios graficos en una misma ventana

par(mfrow=c(1,1))      # un solo gráfico por ventana: la opción por defecto
par(mfrow=c(2,1))      # Dibuja una matriz de gráficos 2x1: un gráfico debajo de otro
par(mfrow=c(2,3))      # Matriz de gráficos 2 x 3 : dos filas por tres columnas

#Un ejemplo:

x = rnorm(200)          # Se generan 200 valores de una normal estandarizada
par(mfrow=c(2,2))      # Se crea una matriz de gráficos 2 x 2
plot(x)                # Dibujo de x frente al índice 1 a 200
hist(x)              # Histograma de x
boxplot(x)             # Diagrama de caja de x
qqnorm(x)            # Gráfico cuantil-cuantil de x frente a la distribución
dev.off()


###########################################################
p<-c(0.01,0.025,0.05,0.10,0.9,0.95,0.975,0.99)
q<-c(-3,-2,-1,0,1,2,3)
x<-seq(from = -4, to =4, by =0.001)
##############################################
#Funcion de distribución
 
pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) #####calculando probabilidades
qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)

##############GRAFICAMOS UNA FUNCION DE PROBABILIDAD DE LA NORMAL
b<-dnorm(x, mean = 0, sd = 1, log = FALSE)   
plot(x,b,"l")
##############################################

#EJERCICIO AVES  
###################################

##############################################

#exp
x<-sort(rexp(100,2))
y<-dexp(x,2)
plot(x,y,"l")

##############################################

#ANALISIS NORMALIDAD DE UNA MUESTRA ALEATORIA

##############################################
#Teorema Central Limite
TamanoMuestra=1000;NumMuestras=100;
muestra=array(0,c(TamanoMuestra,NumMuestras)) 
SumaMuestra=rep(0,TamanoMuestra)# Definir vector con 0's TamanoMuestra veces


for (i in 1:NumMuestras) {
  muestra[,i]=runif(TamanoMuestra,2,4) #llenar columna i con num. aleat. unif.
  SumaMuestra=SumaMuestra+muestra[,i]
}

hist(SumaMuestra)
qqnorm(SumaMuestra)
qqplot(SumaMuestra, dist= "norm", labels=FALSE)   #en el segundo argumento puedo poner otra distibución teórica


##############################################