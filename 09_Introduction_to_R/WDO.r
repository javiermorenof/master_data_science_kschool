#################################################################
# RWANDA ggplot2
#################################################################

# Loading data from the WDB web using an API
# These are data from WDB
install.packages('WDI')
library(WDI)

WDIsearch(string = "life.*expectancy", field = "name", cache = NULL)

# This function is specific for WDB, gets
df.le = WDI(country = "all", indicator = c("SP.DYN.LE00.IN"), start = 1900, 
            end = 2012)
head(df.le)
is.data.frame(df.le)
levels(factor(df.le$country))
levels(factor(df.le$year))

library(ggplot2)
g <- df.le %>% ggplot(aes(x = year, y = SP.DYN.LE00.IN)) + 
  geom_boxplot(aes(group=year))
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g

? geom_boxplot
#Con esta función da aquellos paises que más allá de 1988 la esperanza de vida era menor de 40
subset(df.le, year > 1988 & SP.DYN.LE00.IN < 40)

#Vamos a averiguar cuál es el país que en la década de los 70 descenció radicalmente la esperanza de vida
subset(df.le, year > 1970 & SP.DYN.LE00.IN < 30)

#Vamos a añadir a nuestro plot la esperanza de vida de Rwanda dibujada en rojo para ver si coincide con os patrones
g = g + geom_line(data = subset(df.le, country == "Rwanda"), aes(x = year, y = SP.DYN.LE00.IN), 
                  col = "red")
g

#Vamos a realizar lo mismo para Sierra Leona
g = g + geom_line(data = subset(df.le, country == "Sierra Leone"), aes(x = year, y = SP.DYN.LE00.IN), 
                  col = "orange")
g

#Vamos a realizar lo mismo para Sierra Leona
g = g + geom_line(data = subset(df.le, country == "Cambodia"), aes(x = year, y = SP.DYN.LE00.IN), 
                  col = "yellow")
g


# H01: Genocide > 1994 x
# H02: AIDS epidemy also in Kenya, South Africa, Uganda, etc
g = g + geom_line(data = subset(df.le, country =="Kenya"), aes(x = year, y = SP.DYN.LE00.IN), 
                  col = "green")
g

g = g + geom_line(data = subset(df.le, country =="South Africa"), aes(x = year, y = SP.DYN.LE00.IN), 
                  col = "green")
g

g = g + geom_line(data = subset(df.le, country =="Uganda"), aes(x = year, y = SP.DYN.LE00.IN), 
                  col = "green")
g

#H03: Civil War
g = g + geom_line(data = subset(df.le, country =="Bangladesh"), aes(x = year, y = SP.DYN.LE00.IN), 
                  col = "blue")
g

g = g + geom_line(data = subset(df.le, country =="Iraq"), aes(x = year, y = SP.DYN.LE00.IN), 
                  col = "red")
g

g = g + geom_line(data = subset(df.le, country =="Iran, Islamic Rep."), aes(x = year, y = SP.DYN.LE00.IN), 
                  col = "red",lty=2)
g

g = g + geom_line(data = subset(df.le, country =="Afghanistan"), aes(x = year, y = SP.DYN.LE00.IN), 
                  col = "violet")
g

g = g + geom_line(data = subset(df.le, country =="Cambodia"), aes(x = year, y = SP.DYN.LE00.IN), 
                  col = "pink")
g
