### Taller 2
rm(list = ls())

# Cargar paquetes
require("pacman")
p_load("tidyverse")

# cargar bases de datos
train_hogares <- read.csv("C:/Users/Sofia/OneDrive - Universidad de los Andes/8. Octavo Semestre/Big Data y Machine Learning/Talleres/Taller 2/train_hogares.csv")
train_personas <- read.csv("C:/Users/Sofia/OneDrive - Universidad de los Andes/8. Octavo Semestre/Big Data y Machine Learning/Talleres/Taller 2/train_personas.csv")
test_hogares <- read.csv("C:/Users/Sofia/OneDrive - Universidad de los Andes/8. Octavo Semestre/Big Data y Machine Learning/Talleres/Taller 2/test_hogares.csv")
test_personas <- read.csv("C:/Users/Sofia/OneDrive - Universidad de los Andes/8. Octavo Semestre/Big Data y Machine Learning/Talleres/Taller 2/test_personas.csv")

# Unir bases. Suma ingresos individuales del hogar
colnames(train_hogares)
colnames(train_personas)

sum_ingresos <- train_personas %>% 
                group_by(id) %>% 
                summarize(Ingtot_hogar = sum(Ingtot, na.rm = TRUE))

summary(sum_ingresos)

# Unir variable ingreso total con la base de datos de hogares
train_hogares <- left_join(train_hogares, sum_ingresos)
colnames(train_hogares)

# Arreglar bases de datos 
train_hogares <- rename(train_hogares, c(Cuartos=P5000, Vivienda=P5090, 
                                         Tot_personas=Nper, Personas_gasto=Npersug))

test_hogares <- rename(test_hogares, c(Cuartos=P5000, Vivienda=P5090, 
                                         Tot_personas=Nper, Personas_gasto=Npersug))

"train_hogares <- select(train_hogares, id, Clase, Dominio, Cuartos, Vivienda, Tot_personas, 
                        Personas_gasto, Lp, Pobre, Depto)

test_hogares <- select(test_hogares, id, Clase, Dominio, Cuartos, Vivienda, Tot_personas, 
                       Personas_gasto, Lp, Depto)"

# Cambio de missing values por ceros 
train1 <- train_hogares
test1 <- test_hogares

train1[is.na(train1)] = 0
test1[is.na(test1)] = 0

# Cambio de missing values por la media 
train2 <- train_hogares
test2 <- test_hogares

train2 <- train2 %>% mutate_if(is.numeric, funs(replace_na(., mean(., na.rm = TRUE))))
