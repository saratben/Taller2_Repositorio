### Random Forest con dos nuevas variables (en la media)
train3<- train2%>% mutate(Hacinamiento = train2$P5010/train2$Nper)
train3<- train3%>% mutate(Hacinamiento = ifelse(Hacinamiento >= 1/2,0,1))
test3<- test2%>% mutate(Hacinamiento = test2$P5010/test2$Nper)
test3<- test3%>% mutate(Hacinamiento = ifelse(Hacinamiento >= 1/2,0,1))



##
#cargamos los modelos
require("pacman")
p_load(tidyverse,rpart,caret, randomForest)

##
test3 <- test3 %>% mutate(across(c(Salud_hogar, Hrs_trabajo_hogar), ~replace_na(., mean(., na.rm = TRUE))))

#ponemos la variable a predecir como factor
train3<-train3 %>% mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No","Si")))    

#creamos los parametros para el arbol 
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

#creamos el bosque enfocado en la accuracy
forest2 <- train(Pobre ~ Educacion_hogar+Salud_hogar+Hrs_trabajo_hogar+Hacinamiento+Npersug, 
                data = train3, 
                method = "rf",
                trControl = ctrl,
                metric="Accuracy",
)   
forest2

#predecimos 
RF2_pred<-predict(forest2,newdata = test3)
head(RF2_pred)
summary(RF2_pred)


#guardamos la base con los predichos
Pred_RF2_sencillo <- data.frame('id' = test3$id, 'Pobre' = RF2_pred)
Pred_RF2_sencillo<- Pred_RF2_sencillo%>% mutate( Pobre = ifelse(Pobre=="Si",1,0))
write.csv(Pred_RF2_sencillo, 'Pred_RF2_Nvar.csv',row.names=FALSE)
