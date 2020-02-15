#Life expentancy Exercise
library(tidyverse)

#Miramos di el archivo está en la carpeta que buscamos
getwd()
df <- read_csv("life-expectancy-and-fertility-two-countries-example.csv", col_names=TRUE)

#Miramos el data.frame
view(df)
head(df)

#Tenemos que pasarlo a tidy data, de wide a long con pivot_longer
tidy_dat <- df %>% 
  pivot_longer(-country,
               names_to = c("year","f_t"),
               names_pattern = "(.{4})_(.*)",
               names_ptypes = list(year = integer()),
               values_to="value")
tidy_dat

#Ahora vamos a pasar a wide la columna "f/t"
tidy_dat2 <- tidy_dat %>%
  #Selecciono todas las columnas
  pivot_wider(everything(),
              #Los nombres van a venir de f_t
              names_from = f_t,
              #Los valores se tomarán de value
              values_from = value)

tidy_dat2

#REpresentamos gráficamente la fertilidad
library(ggplot2)
fert <- tidy_dat2 %>%
  ggplot(aes(year,fertility, col=country))+
  geom_line()

life_ex <- tidy_dat2 %>%
  ggplot(aes(year,life_expectancy, col=country))+
  geom_line()

life_ex

#Ponemos ambos gráficos en un mismo grid
library(gridExtra)
grid.arrange(fert,life_ex, nrow=2)

#Ejemplo gapminder
data(gapminder)
str(gapminder)

years <- c(1962,1980,1990,200,2012)
continents <- c("Europe","Asia")

gapminder2 <- gapminder %>%
  filter((year %in% years) & (continent %in% continents)) %>%
  ggplot(aes(fertility,life_expectancy,col=continent))+
  geom_point() +
  #Mediante facet_wrap hago que me los agrupe y separe en grupos mediante year
  facet_wrap(~year)

gapminder2

#Puedo convertirlo en gif mediante gganimate
library(gganimate)
gapminder3 <- gapminder %>%
  filter((year %in% years) & (continent %in% continents)) %>%
  ggplot(aes(fertility,life_expectancy,col=continent))+
  geom_point(size=5) +
  transition_states(year, transition_lenght = 1, state_lenght =1)+
  ggtitle("Year {closest_state}")

animate(gapminder3)