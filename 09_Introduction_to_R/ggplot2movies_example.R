#Importamos el database
library(ggplot2movies)
data(movies)

#Cargamos tidyverse
library(tidyverse)
library(ggplot2)

#Vemos los datos
View(movies)

#Mostrar si hay una relaci�n entre budget y rating para pel�culas que no son cortos
colnames(movies)

movies_2 <- movies %>%
  filter(length>60 & !is.na(budget))

movies_2 %>%
  ggplot(aes(log(budget), rating))+
  geom_point()

#Regresi�n lineal
summary(lm(rating~budget, data=movies))

#Creamos una matriz de correlaci�n
library(corrplot)
movies_3 <- movies_2 %>% select(c(2,3,4,5,6))
correlacion<-round(cor(movies_3),digits=2)
corrplot.mixed(correlacion)
