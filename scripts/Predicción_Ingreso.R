##Modelos de predicción indirecta
require(pacman)
p_load(tidyverse, stargazer)
#Para los modelos solo usamos como predictores  las variables que el training set y test set tienen en común

#Crear variable valor arriendo: tenemos el valor de arriendo estimado para los hogares que no pagan arriendo y el valor de arriendo de los hogares que sí pagan arriendo.

test1 <- test1 %>% mutate(valor_arriendo = P5130+P5140)
#test2 <- test2 %>% mutate(valor_arriendo = P5130+P5140)
#test3 <- test3 %>% mutate(valor_arriendo = P5130+P5140)

train1 <- train1 %>% mutate(valor_arriendo = P5130+P5140)
#train2 <- train2 %>% mutate(valor_arriendo = P5130+P5140)
#train3 <- train3 %>% mutate(valor_arriendo = P5130+P5140)

#Planteamos diferentes modelos de regresión para predecir el salario
train1 <- train1 %>% mutate(Ingtotug = Ingtotug+1)
train1 <- train1 %>% mutate(cuartos_pc = Cuartos/Tot_personas)
train1 <- train1 %>% mutate(arriendo_tamano = valor_arriendo/Cuartos)
#train1 <- train1 %>% mutate(log_ingreso = log10(Ingtotug))

test1 <- test1 %>% mutate(cuartos_pc = Cuartos/Tot_personas)
test1 <- test1 %>% mutate(arriendo_tamano = valor_arriendo/Cuartos)

modelo1 <- lm(Ingtotug ~ valor_arriendo + Tot_personas, data= train1)
modelo2 <-  lm(Ingtotug ~ valor_arriendo*Tot_personas + valor_arriendo, data= train1)
modelo3 <-  lm(Ingtotug ~ valor_arriendo*Tot_personas + valor_arriendo + cuartos_pc, data= train1)
modelo4 <-  lm(Ingtotug ~ valor_arriendo*Tot_personas + valor_arriendo + arriendo_tamano, data= train1)
modelo5 <-  lm(Ingtotug ~  valor_arriendo + arriendo_tamano, data= train1)
stargazer( modelo1, modelo2, modelo3, modelo4, modelo5, type="text")


#Para ver que tan desbalanceada está el training set (la muestra con la que vamos a entrenar el modelo)
prop.table(table(train_hogares$Pobre))
#.   0        1 
# 0.799806 0.200194 

#Hay muy pocas observaciones con clasificación "Pobre"

#MODELOS DE PRUEBA 1: OVERSAMPLING/UPSAMPLING

p_load(AER, caret, MLmetrics, tidymodels, themis, smotefamily)

#Upsample
train1$Pobre <- factor(train1$Pobre)
train1_up <- upSample(x = select(train1, -Pobre), 
                     y = train1$Pobre, list = F, yname = "Pobre")
train1_up$Pobre <- factor(train1_up$Pobre)

prop.table(table(train1_up$Pobre))
## 0   1 
# 0.5 0.5 
## Se agregaron 98912 observaciones

modelo1UP <- train(Ingtotug ~ valor_arriendo + Tot_personas,
                    data= train1_up,
                    method = "glm")

y_hatu1<- predict(modelo1UP, train1_up)
y_probu1 <- as.numeric(y_hatu1 < 289878.2)
y_probu1
acc_u1<- Accuracy(y_pred=y_probu1, y_true= train1_up$Pobre)
acc_u1
#0.5

modelo2UP <- train(Ingtotug ~ valor_arriendo*Tot_personas + valor_arriendo + arriendo_tamano,
                   data= train1_up,
                   method = "glm")

y_hatu2<- predict(modelo2UP, train1_up)
y_probu2 <- as.numeric(y_hatu2 < 289878.2)
y_probu2
acc_u2<- Accuracy(y_pred=y_probu2, y_true=train1_up$Pobre)
acc_u2

# 0.5000303

modelo3UP <- train(Ingtotug ~ arriendo_tamano + valor_arriendo,
                   data= train1_up,
                   method = "glm")

y_hatu3<- predict(modelo3UP, train1_up)
y_probu3 <- as.numeric(y_hatu3 < 289878.2)
y_probu3
acc_u3<- Accuracy(y_pred=y_probu3, y_true=train1_up$Pobre)
acc_u3
#0.5000417

modelo4UP <- train(Ingtotug ~ Tot_personas + valor_arriendo + cuartos_pc,
                   data= train1_up,
                   method = "glm")

y_hatu4<- predict(modelo4UP, train1_up)
y_probu4 <- as.numeric(y_hatu4 < 289878.2)
y_probu4
acc_u4<- Accuracy(y_pred=y_probu4, y_true=train1_up$Pobre)
acc_u4
#0.5

#MODELOS DE PRUEBA 2: DOWNSAMPLING
train1$Pobre <- factor(train1$Pobre)
train1_d <- downSample(x = select(train1, -Pobre), 
                      y = train1$Pobre, list = F, yname = "Pobre")

prop.table(table(train1_d$Pobre))
# 0   1 
# 0.5 0.5 
#Se restaron 98912 observaciones

modelo1D <- train(Ingtotug ~ valor_arriendo + Tot_personas,
                   data= train1_d,
                   method = "glm")
y_hatd1<- predict(modelo1D, train1_d)
y_probd1 <- as.numeric(y_hatd1 < 289878.2)
y_probd1
acc_d1<- Accuracy(y_pred=y_probd1, y_true=train1_d$Pobre)
acc_d1
#0.5

modelo2D <- train(Ingtotug ~ valor_arriendo*Tot_personas + valor_arriendo + arriendo_tamano,
                   data= train1_d,
                   method = "glm")

y_hatd2<- predict(modelo2D, train1_d)
y_probd2 <- as.numeric(y_hatd2 < 289878.2)
y_probd2
acc_d2<- Accuracy(y_pred=y_probd2, y_true=train1_d$Pobre)
acc_d2
#0.5000303

modelo3D <- train(Ingtotug ~ arriendo_tamano + valor_arriendo,
                   data= train1_d,
                   method = "glm")

y_hatd3<- predict(modelo3D, train1_d)
y_probd3 <- as.numeric(y_hatd3 < 289878.2)
acc_d3<- Accuracy(y_pred=y_probd3, y_true=train1_d$Pobre)
acc_d3
#0.5000303

modelo4D <- train(Ingtotug ~ Tot_personas + valor_arriendo + cuartos_pc,
                   data= train1_d,
                   method = "glm")

y_hatd4<- predict(modelo4D, train1_d)
y_probd4 <- as.numeric(y_hatd4 < 289878.2)
acc_d4<- Accuracy(y_pred=y_probd4, y_true=train1_d$Pobre)
acc_d4
# 0.5


