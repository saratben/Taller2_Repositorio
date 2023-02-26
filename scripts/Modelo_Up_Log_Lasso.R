
### Predecir con un Upsample-Logit-lasso.

#cargamos los paquetes 


#creamos los parametros 
lambda_grid <- 10^seq(-4, 0.01, length = 300)

fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

#Creamos el Upsample
upSampledTrain <- upSample(x = train2,
                           y = train2$Pobre,
                           ## keep the class variable name the same:
                           yname = "Pobre")

#Corremos el modelo 
logit_lasso_upsample <- train(Pobre ~ Educacion_hogar+Salud_hogar+Hrs_trabajo_hogar,
                              data = upSampledTrain, 
                              method = "glmnet",
                              trControl = ctrl,
                              family = "binomial", 
                              metric = "Accuracy",
                              tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid), 
                              preProcess = c("center", "scale")
)
logit_lasso_upsample

#Hacemos la predicción y arreglamos 
logit_lasso_upsample_predict<-predict(logit_lasso_upsample, newdata = test2, type = "prob")
head(logit_lasso_upsample_predict)
summary(logit_lasso_upsample_predict)
logit_lasso_upsample_predict<- logit_lasso_upsample_predict %>%  mutate(Pobre= ifelse( Si>No,1,0))

##guardamos la base con los predichos
Pred_Up_Log_lass<- data.frame('id' = test2$id, 'Pobre' = logit_lasso_upsample_predict)
Pred_Up_Log_lass<- Pred_Up_Log_lass[,-c(2,3)]
write.csv(Pred_Up_Log_lass, 'Pred_Up_Log_lass.csv',row.names=FALSE)
