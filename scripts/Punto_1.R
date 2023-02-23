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

# Cálculo pobreza
table(train_hogares$Pobre)

train_hogares <- train_hogares %>% mutate(Pobre_hand = ifelse(Ingpcug < Lp, 1, 0))
table(train_hogares$Pobre, train_hogares$Pobre_hand)

# Arreglar bases de datos 
train_hogares <- rename(train_hogares, c(cuartos="P5000", vivienda="P5090", 
                                         tot_personas="Nper", personas_gasto="Npersug"))

test_hogares <- rename(test_hogares, c(cuartos="P5000", vivienda="P5090", 
                                         tot_personas="Nper", personas_gasto="Npersug"))

train_hogares <- select(train_hogares, cuartos, vivienda, tot_personas, personas_gasto, 
                       Lp, Pobre, Depto)

