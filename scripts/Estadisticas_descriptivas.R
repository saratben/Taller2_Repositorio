############Estadíisticas descriptivas 

##Instalamos paquetes 
require("pacman")
p_load("tidyverse","rvest", "stargazer","vtable") ##cargamos los paquetes.

#Estadísticas descriptivas para la base de train2 (missings con valor de la media)
sumtable(train2)

#Gráfica de distribución del ingreso (en pesos colombianos)

##MANUALMNETE 

tot_pobres <-transform(train2, count(train2$Pobres)) 
  
  mydata$sum <- mydata$x1 + mydata$x2
mydata$mean <- (mydata$x1 + mydata$x2)/2


p1 <- ggplot(df, aes(x = wage, fill = .data[[var]])) +
  geom_density(alpha = 0.4) +
  labs(x = "Salario por hora (USD)", 
       title = paste("Distribución del salario por", var),
       y = "Densidad") +
  theme_bw() +
  theme(legend.position = "bottom")



p2 <- ggplot(df, aes(y = .data[[var]])) +
  geom_bar(aes(x = (..count..)/sum(..count..)),
           fill = "darkblue") +
  labs(title = paste("Distribución de la variable", var),
       x = "Proporción (%)") +
  scale_x_continuous(labels = scales::percent) +
  theme_bw() 








#Gráfica de distribución de pobreza 

g1 <- ggplo


p2 <- ggplot(train2, aes(y = Pobre)) +
  geom_bar(aes(x = (..count..)/sum(..count..)),
           fill = "darkblue") +
  labs(title = paste("Distribución de la variable pobreza"),
       x = "Proporción (%)") +
  scale_x_continuous(labels = scales::percent) +
  theme_bw() 

p2
library(gridExtra)
variables_categoricas <- names(df[,sapply(df, is.factor)])

for (var in variables_categoricas) {
  p1 <- ggplot(df, aes(x = wage, fill = .data[[var]])) +
    geom_density(alpha = 0.4) +
    labs(x = "Salario por hora (USD)", 
         title = paste("Distribución del salario por", var),
         y = "Densidad") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  p2 <- ggplot(df, aes(y = .data[[var]])) +
    geom_bar(aes(x = (..count..)/sum(..count..)),
             fill = "darkblue") +
    labs(title = paste("Distribución de la variable", var),
         x = "Proporción (%)") +
    scale_x_continuous(labels = scales::percent) +
    theme_bw() 
  
  grid.arrange(p1, p2, ncol = 2)
}
