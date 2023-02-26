############Estadíisticas descriptivas 

##Instalamos paquetes 
require("pacman")
p_load("tidyverse","rvest", "stargazer","vtable") ##cargamos los paquetes.

#Estadísticas descriptivas para la base de train2 (missings con valor de la media)
sumtable(train2)

#Gráfica de distribución del ingreso (en pesos colombianos)


ggplot(train2, aes(x = Ingtot_hogar)) +
  geom_histogram(fill = "darkblue") +
  theme_bw() +
  labs(x = "Ingreso total por hogar (COP)", y = "Cantidad", title="Distribución del Ingreso Total de los Hogares")

#Gráfico de la distribución de pobreza 

ggplot(train2, aes(y=Pobre)) + geom_bar(aes(x = (..count..)/sum(..count..)),
  fill = "darkblue") + labs(title ="Distribución de la variable de Pobreza del Hogar", x = "Proporción (%)") +
  scale_x_continuous(labels = scales::percent) +
  theme_bw()


#Label variables 

train2$Pobre <- factor(train2$Pobre,
                       levels=c(0,1),
                       labels= c("no", "si"))
