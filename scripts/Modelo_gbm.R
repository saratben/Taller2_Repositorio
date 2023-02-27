# setwd("C:/Users/WIN/OneDrive/Documentos/LUIS/Octavo semestre/Machine-Learning/Taller 2")

### Predecir con un gbm.

#cargamos los modelos
require("pacman")
p_load(tidyverse,rpart,caret, gbm)

#creamos la grilla para gbm
grid_gbm<-expand.grid(n.trees= c(200,300,500),interaction.depth=c(2,3,4),shrinkage= c(0.01,0.001),n.minobsinnode=c(1,5,10))

#corremos el modelo 
gbm<-train(Pobre ~ Educacion_hogar+Salud_hogar+Hrs_trabajo_hogar,
           data = train2, 
           method = "gbm", 
           trControl = ctrl,
           tuneGrid=grid_gbm,
           metric = "Accuracy"
)            

#Hacemos la predicción y arreglamos 
gbm_pred<-predict(gbm,newdata = test2, type ="prob")
head(gbm_pred)
summary(gbm_pred$Pobre)
gbm_pred<- gbm_pred %>%  mutate(Pobre= ifelse( Si>No,1,0))

##guardamos la base con los predichos
Pred_gbm<- data.frame('id' = test2$id, 'Pobre' = gbm_pred)
Pred_gbm<- Pred_gbm[,-c(2,3)]
colnames(Pred_gbm)[2]<-"Pobre"
write.csv(Pred_gbm, 'Pred_gbm.csv',row.names=FALSE)
