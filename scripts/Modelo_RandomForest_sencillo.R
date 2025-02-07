# setwd("C:/Users/WIN/OneDrive/Documentos/LUIS/Octavo semestre/Machine-Learning/Taller 2")

### Predecir con un Random forest sencillo.

#cargamos los modelos
require("pacman")
p_load(tidyverse,rpart,caret, randomForest)

#adaptamos un poco la base.
datos_hogar_test <- test_personas %>% 
  group_by(id) %>% 
  summarize(Educacion_hogar = mean(P6210, na.rm = TRUE), 
            Salud_hogar = mean(P6100, na.rm = TRUE), 
            Hrs_trabajo_hogar = mean(P6800, na.rm = TRUE))
test2<-left_join(test2,datos_hogar_test)
test2 <- test2 %>% mutate(across(c(Salud_hogar, Hrs_trabajo_hogar), ~replace_na(., mean(., na.rm = TRUE))))


#ponemos la variable a predecir como factor
train2<-train2 %>% mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No","Si")))         

#creamos los parametros para el arbol 
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

#creamos el bosque enfocado en la accuracy
forest <- train(Pobre ~ Educacion_hogar+Salud_hogar+Hrs_trabajo_hogar, 
                data = train2, 
                method = "rf",
                trControl = ctrl,
                metric="Accuracy",
)   

forest
#predecimos 
RF_pred<-predict(forest,newdata = test2)
head(RF_pred)
summary(RF_pred)


#guardamos la base con los predichos
Pred_RF_sencillo <- data.frame('id' = test2$id, 'Pobre' = RF_pred)
Pred_RF_sencillo<- Pred_RF_sencillo%>% mutate( Pobre = ifelse(Pobre=="Si",1,0))
Pred_RF_sencillo<-Pred_RF_sencillo[,-2]
write.csv(Pred_RF_sencillo, 'Pred_RF_sencillo.csv',row.names=FALSE)



