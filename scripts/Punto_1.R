### Taller 2
rm(list = ls())

# Cargar paquetes
require("pacman")
p_load("tidyverse", "dplyr")

# cargar bases de datos
train_hogares <- read.csv("C:/Users/Sofia/OneDrive - Universidad de los Andes/8. Octavo Semestre/Big Data y Machine Learning/Talleres/Taller 2/train_hogares.csv")
train_personas <- read.csv("C:/Users/Sofia/OneDrive - Universidad de los Andes/8. Octavo Semestre/Big Data y Machine Learning/Talleres/Taller 2/train_personas.csv")

test_hogares <- read.csv("C:/Users/Sofia/OneDrive - Universidad de los Andes/8. Octavo Semestre/Big Data y Machine Learning/Talleres/Taller 2/test_hogares.csv")
test_personas <- read.csv("C:/Users/Sofia/OneDrive - Universidad de los Andes/8. Octavo Semestre/Big Data y Machine Learning/Talleres/Taller 2/test_personas.csv")

#saveRDS(train_hogares, "stores/train_hogares.rds")
#saveRDS(train_personas, "train_personas.rds")
#saveRDS(test_hogares, "test_hogares.rds")
#saveRDS(test_personas, "test_personas.rds")

# Unir bases. Sacar valores individuales a grupal para ingreso, estrato, educación y horas trabajadas
colnames(train_hogares)
colnames(train_personas)

datos_train_hogar <- train_personas %>% 
                     group_by(id) %>% 
                     summarize(Ingtot_hogar = sum(Ingtot, na.rm = TRUE), 
                               Estrato_hogar = mean(Estrato1, na.rm = TRUE), 
                               Educacion_hogar = mean(P6210, na.rm = TRUE), 
                               Salud_hogar = mean(P6100, na.rm = TRUE), 
                               Hrs_trabajo_hogar = mean(P6800, na.rm = TRUE))

datos_test_hogar <- test_personas %>% 
                    group_by(id) %>% 
                    summarize(Educacion_hogar = mean(P6210, na.rm = TRUE), 
                              Salud_hogar = mean(P6100, na.rm = TRUE), 
                              Hrs_trabajo_hogar = mean(P6800, na.rm = TRUE))

# Unir variables ponderadas de individuos con la base de datos de hogares
train_hogares <- left_join(train_hogares,datos_train_hogar)
test_hogares <- left_join(test_hogares,datos_test_hogar)

colnames(train_hogares)
colnames(test_hogares)

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
# setwd("C:/Users/Sofia/OneDrive - Universidad de los Andes/8. Octavo Semestre/Big Data y Machine Learning/Talleres/Taller 2")
train1 <- train_hogares
test1 <- test_hogares

train1[is.na(train1)] = 0
test1[is.na(test1)] = 0

# saveRDS(train1, "train1.rds")
# saveRDS(test1, "test1.rds")

# Cambio de missing values por la media 
train2 <- train_hogares
train2 <- train2 %>% mutate(across(c(P5100, P5130, P5140, Salud_hogar, Hrs_trabajo_hogar ), ~replace_na(., mean(., na.rm = TRUE))))

# saveRDS(train2, "train2.rds")

test2 <- test_hogares
test2 <- test2 %>% mutate(across(c(P5100, P5130, P5140), ~replace_na(., mean(., na.rm = TRUE))))

# saveRDS(test2, "test2.rds")

# Cambio de missing values por la mediana 
train3 <- train_hogares
train3 <- train3 %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm = TRUE))))

# saveRDS(train3, "train3.rds")

test3 <- test_hogares
test3 <- test3 %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm = TRUE))))

# saveRDS(test3, "test3.rds")

