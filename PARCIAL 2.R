#librerias
library(caret)
library(dplyr)
library(ggplot2)
library(tidyverse)
#Importación de data.set
cancer <- read_delim("breastCancer_parcial.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
cancer <- data.frame(cancer)
#Exploración de datos
dim(cancer)
head(cancer, 5)
# glimpse(cancer)


#1-Conversión de la variable diagnosis a variable categorica
cancer2 <- cancer %>% mutate(diagnosis = factor(diagnosis))
table(cancer2$diagnosis)


#2-Cuantas pacientes y predictores tiene el dataset?
dim(cancer)
#El dataset cuenta con 500 pacientes y predictores


#3- Resumen de datos
#Tabla de numero de NA por variable
NAs <- apply(X = is.na (cancer),MARGIN = 2,FUN = sum)
#Transformación a Data frame para comodidad
CantidadNA <- data.frame(NAs)

#Tabla de numeros nulos por variable
Nulos <-apply(X = cancer == 0 ,MARGIN =2,FUN=sum)
table(Nulos)
#Transformación a Data frame para comodidad
CantidadNulos <- data.frame(Nulos)

#4- Grafica NAvsNulls
#Cantidad de Nulos
Cnulos <- sum(is.na(CantidadNulos$Nulos))
#Cantidad de NAs
CNas <- sum(CantidadNA$NAs)
#Crear data e impresión de grafica
NullNa <- data.frame(
  Tipo=c("Null","Na") ,  
  Cantidad=c(Cnulos,CNas)
)
TNullvsNA <- ggplot(NullNa, aes(x=Tipo,y =Cantidad)) +
  geom_bar(stat = "identity",color="black",fill=rgb(0.1,0.4,0.5,0.7))+
  ggtitle("Null vs Na")
TNullvsNA 
  
#5-Asignación de diagnosis para maligno y  benigno a M y B
cancer3 <- cancer %>% mutate_if(is.character,str_replace,"M","Maligno") 
cancer3 <- cancer3 %>% mutate_if(is.character,str_replace,"B","Benigno") 
table(cancer3$diagnosis)
#Conversión a factor
cancer3 <- cancer3 %>% mutate(diagnosis = factor(diagnosis))
cancer3

#6- Porcentaje de pacientes concancer B y M
tablaBM <- data.frame(table(cancer3$diagnosis))
colnames(tablaBM) <- c('Enfermedad','Numero de pacientes')
tablaBM$Porcentaje <- tablaBM$`Numero de pacientes` / sum(tablaBM$`Numero de pacientes`) * 100
tablaBM
#61% de pacientes con efermedad benigna y 39% con enfermedad maligna

TBvM <- ggplot(tablaBM, aes(x=Enfermedad,y =Porcentaje)) +
  geom_bar(stat = "identity",color="black",fill=rgb(0.1,0.4,0.5,0.7))+
  ggtitle("Benigno vs Maligno")
TBvM

#7- Para realizar la imputación de valores, se selecciona el metodo de 
#calcular la media de cada variable y asignar el valor a los datos faltantes.
columnas_numericas <- which(sapply(cancer3, is.numeric))
cancermean <- rep(NA, ncol(cancer))
cancermean[columnas_numericas] <- colMeans(cancer3[, columnas_numericas], na.rm = TRUE)

for (x in columnas_numericas) {
  cancer3[is.na(cancer3[,x]), x] <- cancermean[x]
}

#8 identificación y eliminación de valores atipicos
#Eliminación de valores atipicos en boxplot
boxplot(cancer3)
EVA <- boxplot(cancer3,outline = FALSE)
#Comprobación con variable radius_mean
boxplot(cancer3$radius_mean)
boxplot.stats(cancer3$radius_mean)
EVAradius <- boxplot(cancer3$radius_mean,outline = FALSE)
#Eliminación de valores atipicos en data.frame
outliersReplace <- function(data, lowLimit, highLimit){
  data[data < lowLimit] <- mean(data)
  data[data > highLimit] <- mean(data)
  data     #devolvemos el dato       
}
cancer4 <- cancer3
boxplot.stats(cancer3$radius_mean)
cancer4$radius_mean <- outliersReplace(cancer3$radius_mean, 1, 22)
boxplot(cancer4$radius_mean)

boxplot.stats(cancer3$texture_mean)
cancer4$texture_mean <- outliersReplace(cancer3$texture_mean, 1, 29)
boxplot(cancer4$texture_mean)

boxplot.stats(cancer3$perimeter_mean)
cancer4$perimeter_mean <- outliersReplace(cancer3$perimeter_mean, 43, 151)
boxplot(cancer4$perimeter_mean)

boxplot.stats(cancer3$area_mean)
cancer4$area_mean <- outliersReplace(cancer3$area_mean, 40, 1350)
boxplot(cancer4$area_mean)

boxplot.stats(cancer3$smoothness_mean)
cancer4$smoothness_mean <- outliersReplace(cancer3$smoothness_mean, 0, 0.132)
boxplot(cancer4$smoothness_mean)

boxplot.stats(cancer3$compactness_mean)
cancer4$compactness_mean <- outliersReplace(cancer3$compactness_mean, 0, 0.229)
boxplot(cancer4$compactness_mean)

boxplot.stats(cancer3$concavity_mean)
cancer4$concavity_mean <- outliersReplace(cancer3$concavity_mean, 0, 0.29)
boxplot(cancer4$concavity_mean)

boxplot.stats(cancer3$concave.points_mean)
cancer4$concave.points_mean <- outliersReplace(cancer3$concave.points_mean, 0, 0.15)
boxplot(cancer4$concave.points_mean)

boxplot.stats(cancer3$symmetry_mean)
cancer4$symmetry_mean <- outliersReplace(cancer3$symmetry_mean, 0, 0.241)
boxplot(cancer4$symmetry_mean)

boxplot.stats(cancer3$fractal_dimension_mean)
cancer4$fractal_dimension_mean<- outliersReplace(cancer$fractal_dimension_mean, 0, 0.0776)
boxplot(cancer4$fractal_dimension_mean)

boxplot.stats(cancer3$radius_se)
cancer4$radius_se<- outliersReplace(cancer$radius_se, 0, 0.88110)
boxplot(cancer4$radius_se)

boxplot.stats(cancer3$texture_se)
cancer4$texture_se<- outliersReplace(cancer$texture_se, 0, 2.34)
boxplot(cancer4$texture_se)

boxplot.stats(cancer3$perimeter_se)
cancer4$perimeter_se<- outliersReplace(cancer$perimeter_se, 0, 6)
boxplot(cancer4$perimeter_se)

boxplot.stats(cancer3$area_se)
cancer4$area_se<- outliersReplace(cancer$area_se, 0, 89.7)
boxplot(cancer4$area_se)

boxplot.stats(cancer3$smoothness_se)
cancer4$smoothness_se<- outliersReplace(cancer$smoothness_se, 0, 0.011)
boxplot(cancer4$smoothness_se)

boxplot.stats(cancer3$compactness_se)
cancer4$compactness_se<- outliersReplace(cancer$compactness_se, 0.0030, 0.013)
boxplot(cancer4$compactness_se)

boxplot.stats(cancer3$concavity_se)
cancer4$concavity_se<- outliersReplace(cancer$concavity_se, 0, 0.0083)
boxplot(cancer4$concavity_se)

boxplot.stats(cancer3$concave.points_se)
cancer4$concave.points_se<- outliersReplace(cancer$concave.points_se, 0, 0.024)
boxplot(cancer4$concave.points_se)

boxplot.stats(cancer3$symmetry_se)
cancer4$symmetry_se<- outliersReplace(cancer$symmetry_se, 0, 0.01)
boxplot(cancer4$symmetry_se)

boxplot.stats(cancer3$fractal_dimension_se)
cancer4$fractal_dimension_se<- outliersReplace(cancer$fractal_dimension_se, 0, 0.0077)
boxplot(cancer4$fractal_dimension_se)

boxplot.stats(cancer3$radius_worst)
cancer4$radius_worst<- outliersReplace(cancer$radius_worst, 7, 26.7)
boxplot(cancer4$radius_worst)

boxplot.stats(cancer3$texture_worst)
cancer4$texture_worst<- outliersReplace(cancer$texture_worst, 12, 40.6)
boxplot(cancer4$texture_worst)

boxplot.stats(cancer3$perimeter_worst)
cancer4$perimeter_worst<- outliersReplace(cancer$perimeter_worst, 50.4, 125.2)
boxplot(cancer4$perimeter_worst)

boxplot.stats(cancer3$area_worst)
cancer4$area_worst<- outliersReplace(cancer$area_worst, 0, 2089)
boxplot(cancer4$area_worst)

boxplot.stats(cancer3$smoothness_worst)
cancer4$smoothness_worst<- outliersReplace(cancer$smoothness_worst, 0, 0.188)
boxplot(cancer4$smoothness_worst)

boxplot.stats(cancer3$compactness_worst)
cancer4$compactness_worst<- outliersReplace(cancer$compactness_worst, 0, 0.188)
boxplot(cancer4$compactness_worst)

boxplot.stats(cancer3$concavity_worst)
cancer4$concavity_worst<- outliersReplace(cancer$concavity_worst, 0, 0.789)
boxplot(cancer4$concavity_worst)

boxplot.stats(cancer3$concave.points_worst)
cancer4$concave.points_worst<- outliersReplace(cancer$concave.points_worst, 0, 0.789)
boxplot(cancer4$concave.points_worst)

boxplot.stats(cancer3$symmetry_worst)
cancer4$symmetry_worst<- outliersReplace(cancer$symmetry_worst, 0.15, 0.422)
boxplot(cancer4$symmetry_worst)

boxplot.stats(cancer3$fractal_dimension_worst)
cancer4$fractal_dimension_worst<- outliersReplace(cancer$fractal_dimension_worst, 0.05, 0.12)
boxplot(cancer4$fractal_dimension_worst)

#Eliminación de valores Nulos
columnas_numericas2 <- which(sapply(cancer4, is.numeric))
cancermean2 <- rep(NA, ncol(cancer4))
cancermean2[columnas_numericas2] <- colMeans(cancer4[, columnas_numericas2], na.rm = TRUE)

for (x in columnas_numericas2) {
  cancer4[is.na(cancer4[,x]), x] <- cancermean2[x]
}
#9- resumen de datos con Summary
summary(cancer4)

#10-La imputación de valores faltantes o nulos afectó en la distribución de los datos,
#haciendo que el analisis estadistico de los datos sea más precisa

#11-remover ID
cancer5 <- cancer4 %>% select(-id,-diagnosis)

#12-Estandarización de datos
Cancer.estandar <- cancer5%>% mutate_all (~ ( scale (.)%>% as.vector ))
Cancer.estandar

#PASO 5
#13- 
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(cancer[,c(3:11)],histogram=TRUE, col="grey10", pch=1, main="Cancer Mean")

set.seed(5464)
names(Cancer.estandar)
str(Cancer.estandar)
# Medir la correlación entre dos variables 
cor(Cancer.estandar$radius_mean, Cancer.estandar$texture_mean, use = 'complete.obs')

# Teniendo en cuenta la prueba de significancia y el valor de confianza 
cor.test(Cancer.estandar$radius_mean, Cancer.estandar$texture_mean, use = 'complete.obs')

# Calculamos la matrix de correlación para todo el dataframe
correlationMatrix <- cor(Cancer.estandar[,1:30])  # Remover el outcome
# Resumir la matrix de correlaciòn
print(correlationMatrix)
# encontrar variables que están altamente correlacionas (idealmente> 0,75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# Nombre de la variable(s) correlacionadas
cat("Variable(s) correlacionada(s):",colnames(Cancer.estandar[highlyCorrelated]))

Cancer.estandar <-data.frame(diagnosis = cancer3$diagnosis,Cancer.estandar)

# Eliminar las variables que no tienen fuerte asociacion con el outcome ............
# Correlation Matrix     
correlationMatrix <- cor(Cancer.estandar[,1:30])                              # Select only numerical Variables
hcorrelated       <- findCorrelation(correlationMatrix, cutoff=0.6)  # Threshold >0.6, Find Features that are highly corrected 
print(hcorrelated)                                                   # print indexes of highly correlated attributes
highly_cor_var    <- colnames(Cancer.estandar[hcorrelated])                   # displaying highly correlated variables
data.frame(highly_cor_var)

#Data.frames con variables iniciales y variables altamentecorrelacionadas
set.seed(899)
inTrain <- createDataPartition(y=Cancer.estandar$diagnosis, p = 0.80, list =FALSE)  # Split 80% y 20%
train   <- Cancer.estandar[inTrain,]
test    <- Cancer.estandar[-inTrain,]
# Preparar el esquema de entrenamiento
control <- trainControl(method="repeatedcv", number=10, repeats=3)

#13-
#80% de los datos para entrenamiento
train
#20% de los datos para evaluación
test
install.packages("e1071")
set.seed(1156)
logFit <- train(diagnosis ~.,
                data = train,
                method = "glm",
                preProc = c("center", "scale"),
                trControl = control)


#PASO 6
#15
#*El modelo más exacto es el que se seleccionó las variables más significativas

#*El modelo Train presenta menos errores en la detección de tumores

#*La sensibilidad y especificidad indican la capacidad clasificar 
#los valores negativos y positivos

#*Los valores positivos y negativos representan el grado de exactitud, sensibilidad y especificidad
#de dicha variable
#*Grafica barplot
predictionglm <- predict(logFit,Cancer.estandar = x_test, type="prob")  # Probabilidad de que sea 1(no cumple)
predglm01     <- ifelse(predictionglm > 0.5,"Malignant" ,"Benign")
predglm01     <- as.factor(predglm01)
length(predglm01)
Cancer.estandar$diagnosis
dim(Cancer.estandar)
# AUC and CI   .................................. 
print(aucglm   <- auc(test$diagnosis,predictionglm))
print(ciaucglm <- ci.auc(test$diagnosis, predictionglm))
rocglm          <-roc(test$diagnosis,predictionglm)$auc

# Compute error  ................................
confusionmatrix  <- table(predglm01, 
                          test[,which(names(test) == "diagnosis")])

#16- Con el analisis de dichos datos, se puede llegar a la conclusión que
#la predicción de tumores cancerigenos así como el estado clinico del paciente
# puede analizarse con mayor detenimiento al aplicar el analisis etadistico
#en softwares de tratamiento de datos como R studio.

#Respondiendo a la pregunta de investigación,se puede especificar una serie de
#variables que son  las más relevantes para predecir si una mujer tiene o no cancer
#de mama, dichas variables están altamente correlacionadas y son las siguientes:
data.frame(highly_cor_var)
#1        smoothness_worst
#2         smoothness_mean
#3               area_mean
#4          perimeter_mean
#5       concave.points_se
#6            radius_worst
#7             radius_mean
#8 fractal_dimension_mean
#9              radius_se
#10          texture_worst
#11       compactness_mean
#12        concavity_worst
