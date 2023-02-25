##Modelos de predicción indirecta

#Para los modelos solo usamos como predictores  las variables que el training set y test set tienen en común

#Crear variable valor arriendo: tenemos el valor de arriendo estimado para los hogares que no pagan arriendo y el valor de arriendo de los hogares que sí pagan arriendo.

test1 <- test1 %>% mutate(valor_arriendo = P5130+P5140)
test2 <- test2 %>% mutate(valor_arriendo = P5130+P5140)
test3 <- test3 %>% mutate(valor_arriendo = P5130+P5140)

train1 <- train1 %>% mutate(valor_arriendo = P5130+P5140)
train2 <- train2 %>% mutate(valor_arriendo = P5130+P5140)
train3 <- train3 %>% mutate(valor_arriendo = P5130+P5140)

