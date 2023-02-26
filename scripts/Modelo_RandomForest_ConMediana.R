### Predecir con un Random forest con reemplazo de missing por la mediana

#cargamos los modelos
require("pacman")
p_load(tidyverse,rpart,caret, randomForest)

#ponemos la variable a predecir como factor
train3 <- train3 %>% mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No","Si")))         

#creamos los parametros para el arbol 
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl<- trainControl(method = "cv",
                    number = 10,
                    summaryFunction = fiveStats,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

#creamos el bosque enfocado en la accuracy
forest <- train(Pobre ~ Educacion_hogar + Salud_hogar + Hrs_trabajo_hogar + Personas_gasto, 
                data = train3, 
                method = "rf",
                trControl = ctrl,
                metric="Accuracy",
)   

forest

#predecimos 
resultados <- predict(forest, newdata = test3)
head(resultados)
summary(resultados)

#guardamos la base con los predichos
resultados_kaggle <- data.frame('id' = test3$id, 'pobre' = resultados)
resultados_kaggle <- resultados_kaggle %>% mutate( pobre = ifelse(pobre=="Si",1,0))

write.csv(resultados_kaggle, 'resultados_kaggle.csv',row.names=FALSE)