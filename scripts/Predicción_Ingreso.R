##Modelos de predicción indirecta
require(pacman)
p_load(tidyverse, stargazer)
#Para los modelos solo usamos como predictores  las variables que el training set y test set tienen en común

#Crear variable valor arriendo: tenemos el valor de arriendo estimado para los hogares que no pagan arriendo y el valor de arriendo de los hogares que sí pagan arriendo.
test1<-test_hogares
test1[is.na(test1)] = 0
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
train1 <- train1 %>% mutate(log_ingreso = log10(Ingtotug))

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

#MODELOS DE PRUEBA 1: OVERSAMPLING
p_load(AER, caret, MLmetrics, tidymodels, themis, smotefamily)

#estandarizamos la muestra
train1_s <- train1
test1_s <- test1

var_num <- c("Ingtotug", "cuartos_pc", "valor_arriendo",
                         "arriendo_tamano", "Tot_personas")
var_num2 <- c( "cuartos_pc", "valor_arriendo",
             "arriendo_tamano", "Tot_personas")
escalador1 <- preProcess(train1[, var_num],
                        method = c("center", "scale"))
escalador2 <- preProcess(test1[, var_num2],
                         method = c("center", "scale"))
train1_s[, var_num] <- predict(escalador1, train1[, var_num])
test1_s[, var_num2] <- predict(escalador2, test1[, var_num2])

#Upsample
train1_s$Pobre <- factor(train1_s$Pobre)
train1_s2 <- upSample(x = select(train1_s, -Pobre), 
                     y = train1_s$Pobre, list = F, yname = "Pobre")

prop.table(table(train1_s2$Pobre))
## 0   1 
# 0.5 0.5 
## Se agregaron 98912 observaciones

#Corremos los modelos de regresión

modelo1UP <- lm(Ingtotug ~ valor_arriendo + Tot_personas, data= train1_s2)
modelo2UP <-  lm(Ingtotug ~ valor_arriendo*Tot_personas + valor_arriendo, data= train1_s2)
modelo3UP <-  lm(Ingtotug ~ valor_arriendo*Tot_personas + valor_arriendo + cuartos_pc, data= train1_s2)
modelo4UP <-  lm(Ingtotug ~ valor_arriendo*Tot_personas + valor_arriendo + arriendo_tamano, data= train1_s2)
modelo5UP <-  lm(Ingtotug ~  valor_arriendo + arriendo_tamano, data= train1_s2)
stargazer( modelo1UP, modelo2UP, modelo3UP, modelo4UP, modelo5UP, type="text")

#Comparaciones entre modelos sin oversampling y con
stargazer(modelo1, modelo1UP, modelo2, modelo2UP, type="text")
stargazer(modelo3, modelo3UP, modelo4, modelo4UP, type="text")
stargazer(modelo5, modelo5UP, modelo4, modelo4UP, type="text")

y_hatu1<- predict(modelo1UP, train1_s2)
y_probu1 <- as.numeric(y_hatu1 < 289878.2)
acc_up1<-Accuracy(y_pred=y_probu1, y_true=train1_s)
acc_up1

y_hatu2<- predict(modelo2UP, train1_s2)
y_probu2 <- as.numeric(y_hatu2 < 289878.2)
acc_up2<-Accuracy(y_pred=y_probu2, y_true=train1_s)
acc_up2

y_hatu5<- predict(modelo5UP, train1_s2)
y_probu5 <- as.numeric(y_hatu5 < 289878.2)
acc_up5<-Accuracy(y_pred=y_probu5, y_true=train1_s)
acc_up5

modelo1UP <- train(x = select(train1_s2, -Pobre), 
                   y = train1_s2$Ingtotug,
                   preProcess = NULL,
                   method = "glmnet")

y_hatu1<- predict(modelo1UP, train1_s2)
y_probu1 <- as.numeric(y_hatu1 < 289878.2)
acc_up1<- Accuracy(y_pred=y_probu1, y_true=train1_s)
acc_up1
