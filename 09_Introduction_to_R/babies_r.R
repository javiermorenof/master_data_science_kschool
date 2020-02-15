#Se selecciona el directorio de trabajo
getwd()

#Se selecciona el directorio de trabajo
setwd("~/Downloads")

#Cargamos la librer?a tidyverse
library(tidyverse)

#Dos opciones para leer el archivo babies 'read.delim' o 'read_tsv' en tidyverse
babies <- read.delim("babies.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)

#Vemos las primeras l?neas
head(babies)

#Vemos su estructura
str(babies)

#Calculemos la media de peso de madres fumadores y la media de peso de madres no fumadoras
smoke_y <- babies[babies$smoke == 1,]
smoke_n <- babies[babies$smoke == 0,]
mean(smoke_y$bwt)
mean(smoke_n$bwt)

#Otra manera:
mean(babies$bwt[babies$smoke==0])
mean(babies$bwt[babies$smoke==1])

#Como se puede ver, en el segundo paso no se pone coma. ?Por qu?? En el primero pasamos un dataframe, as? que nos obliga a indicar dimensiones. 
#En la segunda forma realmente queremos un vector de T/F donde ya hemos seleccionado la columna y tenemos ya un vector, no un dataframe

#Ejemplo con tidyverse
library(tidyverse)
babies %>% group_by(smoke) %>% summarize(avg = mean(bwt))

#Vamos a plotear la relación entre la gestación y el peso al nace
#Primero limpio el dataset quitando los valores deconocidos que tienen valor 999
babies_clean <- babies %>% filter(gestation < 999)
#Realizo el plot
plot(x=babies_clean$gestation, y=babies_clean$bwt)
abline(a=-10.0642, b=0.4643)
#Realizo la regresión lineal
summary(lm(bwt~gestation, data=babies_clean))

#Realizo box plots
boxplot(bwt~smoke, data=babies_clean[babies_clean$smoke != 9,])

#Miramos un histograma. Parece que sigue una distribución normal
hist(babies$bwt)

library(NHANES)
library(tidyverse)
data("NHANES")

str(NHANES)
NHANES %>% group_by(Gender, AgeDecade, .drop=TRUE) %>% summarize(avgSys=mean(BPSysAve, na.rm=TRUE), avgDia=mean(BPDiaAve, na.rm=TRUE))


