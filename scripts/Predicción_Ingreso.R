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

#Creamos las variables necesarias

#Cuartos per cápita
train1 <- train1 %>% mutate(cuartos_pc = Cuartos/Tot_personas)
#Valor de arriendo entre el número de cuartos
train1 <- train1 %>% mutate(arriendo_tamano = valor_arriendo/Cuartos)

test1 <- test1 %>% mutate(cuartos_pc = Cuartos/Tot_personas)
test1 <- test1 %>% mutate(arriendo_tamano = valor_arriendo/Cuartos)

modelo1 <- lm(Ingtotug ~ valor_arriendo + Tot_personas, data= train1)
modelo2 <-  lm(Ingtotug ~ valor_arriendo*Tot_personas + valor_arriendo, data= train1)
modelo3 <-  lm(Ingtotug ~ valor_arriendo*Tot_personas + valor_arriendo + arriendo_tamano, data= train1)
modelo4 <-  lm(Ingtotug ~  valor_arriendo + arriendo_tamano, data= train1)
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

#MODELOS DE PRUEBA SIN REMUESTREO
modelo1n <- train(Ingtotug ~ valor_arriendo + Tot_personas,
                  data= train1,
                  method = "glm")
y_hatn1<- predict(modelo1n, train1)
y_probn1 <- as.numeric(y_hatn1 < 289878.2)
y_probn1
acc_n1<- Accuracy(y_pred=y_probn1, y_true=train1$Pobre)
acc_n1
#0.799806

modelo2n <- train(Ingtotug ~ valor_arriendo*Tot_personas + valor_arriendo + arriendo_tamano,
                  data= train1,
                  method = "glm")

y_hatn2<- predict(modelo2n, train1)
y_probn2 <- as.numeric(y_hatn2 < 289878.2)
acc_n2<- Accuracy(y_pred=y_probn2, y_true=train1$Pobre)
acc_n2
#0.7997696


modelo3n <- train(Ingtotug ~ arriendo_tamano + valor_arriendo,
                  data= train1,
                  method = "glm")

y_hatn3<- predict(modelo3n, train1)
y_probn3 <- as.numeric(y_hatn3 < 289878.2)
acc_n3<- Accuracy(y_pred=y_probn3, y_true=train1$Pobre)
acc_n3
#0.7998

modelo4n <- train(Ingtotug ~ Tot_personas + valor_arriendo + cuartos_pc,
                  data= train1,
                  method = "glm")

y_hatn4<- predict(modelo4n, train1)
y_probn4 <- as.numeric(y_hatn4 < 289878.2)
acc_n4<- Accuracy(y_pred=y_probn4, y_true=train1$Pobre)
acc_n4
#0.799806

#MODELO CON RIDGE

y<- train1$Ingtotug
x <- data.matrix(train1[, c("valor_arriendo", "Tot_personas", "arriendo_tamano", "cuartos_pc")])
library(glmnet)
mod1ridge <- glmnet(x,y, alpha=0)

#k-fold cross validation 
cv_mod <- cv.glmnet(x,y, alpha=0)
mejor_lambda <- cv_mod$lambda.min
mejor_lambda
#49277.18
plot(cv_mod)

mod1ridgebest <- glmnet(x, y, alpha=0, lambda=mejor_lambda)
y_hatr1<- predict(mod1ridgebest, s=mejor_lambda, newx= x)
y_probr1 <- as.numeric(y_hatr1 < 289878.2)
acc_r1<- Accuracy(y_pred=y_probr1, y_true=train1$Pobre)
acc_r1
#0.7998

#MODELO CON RIDGE Y OVERSAMPLING
yup<- train1_up$Ingtotug
xup <- data.matrix(train1_up[, c("valor_arriendo", "Tot_personas", "arriendo_tamano", "cuartos_pc")])

mod2ridge <- glmnet(xup,yup, alpha=0)

#k-fold cross validation 
cv_mod <- cv.glmnet(xup,yup, alpha=0)
mejor_lambdaup <- cv_mod$lambda.min
mejor_lambdaup
#18009.26

mod2ridgebest <- glmnet(xup, yup, alpha=0, lambda=mejor_lambdaup)
y_hatr2<- predict(mod2ridgebest, s=mejor_lambdaup, newx= xup)
y_probr2 <- as.numeric(y_hatr2 < 289878.2)
acc_r2<- Accuracy(y_pred=y_probr2, y_true=train1_up$Pobre)
acc_r2
# 0.5000303

#MODELO LASSO 
y<- train1$Ingtotug
x <- data.matrix(train1[, c("valor_arriendo", "Tot_personas", "arriendo_tamano", "cuartos_pc")])
mod1lasso <- glmnet(x,y, alpha=1)

#k-fold cross validation 
cv_mod <- cv.glmnet(x,y, alpha=1)
mejor_lambdal <- cv_mod$lambda.min
mejor_lambdal
#4285.878

mod1lassobest <- glmnet(x, y, alpha=1, lambda=mejor_lambdal)
y_hatl1<- predict(mod1lassobest, s=mejor_lambdal, newx= x)
y_probl1 <- as.numeric(y_hatl1 < 289878.2)
acc_l1<- Accuracy(y_pred=y_probl1, y_true=train1$Pobre)
acc_l1
#0.7998



