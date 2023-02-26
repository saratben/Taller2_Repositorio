### Predecir con un Downsample-Logit_lasso

#cargamos los paquetes 
require("pacman")
p_load(tidyverse,caret) 

#creamos los parametros 
lambda_grid <- 10^seq(-4, 0.01, length = 300)

fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

#ponemos la variable a predecir como factor
#ponemos la variable a predecir como factor
train2<-train2 %>% mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No","Si")))            
summary(train2$Pobre)
#Creamos el downsample
downsampleTrain <- downSample(x = train2,
                           y = train2$Pobre,
                           ## keep the class variable name the same:
                           yname = "Pobre")

#Corremos el modelo 
logit_lasso_down <- train(Pobre ~ Educacion_hogar+Salud_hogar+Hrs_trabajo_hogar,
                              data = downsampleTrain, 
                              method = "glmnet",
                              trControl = ctrl,
                              family = "binomial", 
                              metric = "Accuracy",
                              tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid), 
                              preProcess = c("center", "scale")
)
logit_lasso_down

#Hacemos la predicción y arreglamos 
logit_lasso_downsample_predict<-predict(logit_lasso_down, newdata = test2, type = "prob")
head(logit_lasso_downsample_predict)
summary(logit_lasso_downsample_predict)
logit_lasso_downsample_predict<- logit_lasso_downsample_predict %>%  mutate(Pobre= ifelse( Si>No,1,0))

##guardamos la base con los predichos
Pred_Down_Log_lass<- data.frame('id' = test2$id, 'Pobre' = logit_lasso_downsample_predict)
Pred_Down_Log_lass<- Pred_Down_Log_lass[,-c(2,3)]
colnames(Pred_Down_Log_lass)[2]<-"Pobre"
write.csv(Pred_Down_Log_lass, 'Pred_Down_Log_lass.csv',row.names=FALSE)
