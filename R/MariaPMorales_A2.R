
library(tidyverse)
library(dplyr)
library(caret)
library(class)
library(gmodels)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
parentFolder <- dirname(folder)

DIABETES <-
  read_csv(paste0(parentFolder
                  ,"/data/diabetes_012_health_indicators_BRFSS2015.csv"))

DIABETES_DF <- DIABETES

## 1 = tiene diabetes, 0 = no tiene diabetes

DIABETES_DF$Diabetes_012 <- ifelse(DIABETES_DF$Diabetes_012 >= 1,1,0)

######################################### Muestreo estratificado aleatorio ############################################################
set.seed(1)
DIAB_DF <- DIABETES_DF %>%
  group_by(Diabetes_012) %>%
  sample_n(1268, replace = FALSE) %>%
  ungroup()

######################################### análisis exploratorio de datos ###################################################

hist(DIABETES_DF$Diabetes_012
     ,breaks = 50
     ,main = "Histograma presencia de la enfermedad",
     xlab = "Presencia de la enfermedad", ylab = "Frecuencia"
     ,col= "blue")

hist(DIAB_DF$Sex
     ,main = "Histograma de sexo (F-M)",
     xlab = "Sexo", ylab = "Frecuencia"
     ,col= "white")

hist(DIAB_DF$HeartDiseaseorAttack
     ,breaks = 50
     ,main = "Histograma de Ataque al corazón",
     xlab = "ataque al corazon", ylab = "Frecuencia"
     ,col= "red")

plot(DIABETES_DF [c("Sex", "HeartDiseaseorAttack")], pch=21
     , bg= c("white", "red")[unclass(DIABETES_DF$Diabetes_012)])

plot(Heart_attack__DF [c("Sex", "Diabetes_012")], pch=21
     , bg= c("black", "blue")[unclass(Heart_attack__DF$HeartDiseaseorAttack)])


summary(DIABETES_DF)
summary(DIAB_DF)

data.frame(table(DIABETES_DF$Diabetes_012))
data.frame(table(Heart_attack_DF$HeartDiseaseorAttack))
data.frame(table(SEX$Sex))
############################################ MODELO KNN DIABETES_012################################################################


############ Modelo con 21 variables , cross validation= 10 #########################################################################

DIAB_DF$Diabetes_012 <- as.factor(DIAB_DF$Diabetes_012)

set.seed(1)
sample.index <- sample(1:nrow(DIAB_DF)
                       ,nrow(DIAB_DF)*0.7
                       ,replace = F)

predictors <- c("HighBP","HighChol","CholCheck","BMI", "Smoker","Stroke","HeartDiseaseorAttack","PhysActivity"
                ,"Fruits","Veggies","HvyAlcoholConsump", "AnyHealthcare","NoDocbcCost","GenHlth","MentHlth","PhysHlth","DiffWalk"
                ,"Sex","Age","Education","Income")

## datos de entrenamiento y prueba
train.data <- DIAB_DF[sample.index,c(predictors,"Diabetes_012"),drop=F]
test.data <- DIAB_DF[-sample.index,c(predictors,"Diabetes_012"),drop=F]

## primer modelo knn
ctrl <- trainControl(method="cv",  number=10)
knnFit <- train(Diabetes_012 ~ HighBP + HighChol + CholCheck +BMI + Smoker + Stroke + HeartDiseaseorAttack + PhysActivity + Fruits
                + Veggies + HvyAlcoholConsump + AnyHealthcare + NoDocbcCost + GenHlth + MentHlth + PhysHlth + DiffWalk + Sex + Age + Education + Income
                , data = train.data
                , method = "knn", trControl = ctrl
                , preProcess = c("range") #c("center", "scale")
                , tuneLength = 25)

knnFit
plot(knnFit)

#### predicciones

knnPredict <- predict(knnFit, newdata = test.data)
knnPredict
confusionMatrix(knnPredict, test.data$Diabetes_012)

##### primer modelo knn ####
## normalizacion
#normalise <- function(x){
 # return((x-min(x))/(max(x)-min(x)))
#}

#DIABDF<- DIAB_DF
#DIABDF$Diabetes_012 <- as.factor(DIABDF$Diabetes_012)

#DIABDF$BMI <- normalise(DIAB_DF$BMI)
#DIABDF$GenHlth <- normalise(DIAB_DF$GenHlth)
#DIABDF$MentHlth <- normalise(DIAB_DF$MentHlth)
#DIABDF$PhysHlth <- normalise(DIAB_DF$PhysHlth)
#DIABDF$Age <- normalise(DIAB_DF$Age)
#DIABDF$Education <- normalise(DIAB_DF$Education)
#DIABDF$Income <- normalise(DIAB_DF$Income)

#set.seed(1)
#sample.index <- sample(1:nrow(DIABDF)
#                       ,nrow(DIABDF)*0.7
 #                      ,replace = F)

#predictors <- c("HighBP","HighChol","CholCheck","BMI", "Smoker","Stroke","HeartDiseaseorAttack","PhysActivity"
 #               ,"Fruits","Veggies","HvyAlcoholConsump", "AnyHealthcare","NoDocbcCost","GenHlth","MentHlth","PhysHlth","DiffWalk"
  #              ,"Sex","Age","Education","Income")

#k <- 25

#train.data <-DIABDF[sample.index,c(predictors,"Diabetes_012"),drop=F]
#test.data <-DIABDF[-sample.index,c(predictors,"Diabetes_012"),drop=F]

#prediction <- knn(train = train.data[predictors]
 #                 , test = test.data[predictors]
  #                ,cl = train.data$Diabetes_012, k=k)

#prediction

#CrossTable(x = test.data$Diabetes_012, y = prediction
 #          , prop.chisq = F)

############ Modelo con 16 variables  predictoras , cross validation = 5 #########################################################################

DIAB_DF$Diabetes_012 <- as.factor(DIAB_DF$Diabetes_012)

set.seed(1)
sample.index1 <- sample(1:nrow(DIAB_DF)
                       ,nrow(DIAB_DF)*0.7
                       ,replace = F)

predictors1 <- c("HighBP","HighChol","CholCheck","BMI", "Smoker","Stroke","HeartDiseaseorAttack","PhysActivity"
                ,"Fruits","Veggies","HvyAlcoholConsump", "AnyHealthcare","NoDocbcCost","PhysHlth"
                ,"Sex","Age")

## datos de entrenamiento y prueba
train.data1 <- DIAB_DF[sample.index1,c(predictors1,"Diabetes_012"),drop=F]
test.data1 <- DIAB_DF[-sample.index1,c(predictors1,"Diabetes_012"),drop=F]

## p1rimer modelo knn
ctrl1 <- trainControl(method="cv",  number = 5)
knnFit1 <- train(Diabetes_012 ~ HighBP + HighChol + CholCheck +BMI + Smoker + Stroke + HeartDiseaseorAttack + PhysActivity + Fruits
                + Veggies + HvyAlcoholConsump + AnyHealthcare + NoDocbcCost + PhysHlth + Sex + Age
                , data = train.data1
                , method = "knn", trControl = ctrl1
                , preProcess = c("range") #c("center", "scale")
                , tuneLength = 25)

knnFit1
plot(knnFit1)

#### predicciones

knnPredict1 <- predict(knnFit1, newdata = test.data1)
knnPredict1
confusionMatrix(knnPredict1, test.data1$Diabetes_012)

############ Modelo con 11 variables  predictoras , cross validation = 5 #########################################################################

DIAB_DF$Diabetes_012 <- as.factor(DIAB_DF$Diabetes_012)

set.seed(1)
sample.index2 <- sample(1:nrow(DIAB_DF)
                       ,nrow(DIAB_DF)*0.7
                       ,replace = F)

predictors2 <- c("HighBP","HighChol","BMI","HeartDiseaseorAttack","PhysActivity"
                ,"Fruits","Veggies", "AnyHealthcare","PhysHlth"
                ,"Sex","Age")

## datos de entrenamiento y prueba
train.data2 <- DIAB_DF[sample.index2,c(predictors2,"Diabetes_012"),drop=F]
test.data2 <- DIAB_DF[-sample.index2,c(predictors2,"Diabetes_012"),drop=F]

## primer modelo knn
ctrl2 <- trainControl(method="cv",  number = 3)
knnFit2 <- train(Diabetes_012 ~ HighBP + HighChol  +BMI   + HeartDiseaseorAttack + PhysActivity + Fruits
                + Veggies +  AnyHealthcare  + PhysHlth + Sex + Age
                , data = train.data2
                , method = "knn", trControl = ctrl
                , preProcess = c("range") #c("center", "scale")
                , tuneLength = 25)

knnFit2
plot(knnFit2)

#### predicciones

knnPredict2 <- predict(knnFit2, newdata = test.data2)
knnPredict2
confusionMatrix(knnPredict2, test.data2$Diabetes_012)

####################################### modelo knn Heart Diseaseor attack ##################################

Heart_attack_DF <- DIABETES
Heart_attack_DF$HeartDiseaseorAttack <- as.factor(Heart_attack_DF$HeartDiseaseorAttack)

set.seed(1)
H_A_DF <- Heart_attack_DF %>%
  group_by(HeartDiseaseorAttack) %>%
  sample_n(1268, replace = FALSE) %>%
  ungroup()

############################### modelo con 21 variables predictoras ##########################################
set.seed(1)
sample.index3 <- sample(1:nrow(H_A_DF)
                        ,nrow(H_A_DF)*0.7
                        ,replace = F)

predictors3 <- c("Diabetes_012","HighBP","HighChol","CholCheck","BMI", "Smoker","Stroke","PhysActivity"
                 ,"Fruits","Veggies","HvyAlcoholConsump", "AnyHealthcare","NoDocbcCost","GenHlth","MentHlth","PhysHlth","DiffWalk"
                 ,"Sex","Age","Education","Income")

## datos de entrenamiento y prueba
train.data3 <- H_A_DF[sample.index3,c(predictors3,"HeartDiseaseorAttack"),drop=F]
test.data3 <- H_A_DF[-sample.index3,c(predictors3,"HeartDiseaseorAttack"),drop=F]

## primer modelo knn
ctrl3 <- trainControl(method="cv",  number = 10)
knnFit3 <- train(HeartDiseaseorAttack ~.
                 , data = train.data3
                 , method = "knn", trControl = ctrl3
                 , preProcess = c("range") #c("center", "scale")
                 , tuneLength = 25)

knnFit3
plot(knnFit3)

#### predicciones

knnPredict3 <- predict(knnFit3, newdata = test.data3)
knnPredict3
confusionMatrix(knnPredict3, test.data3$HeartDiseaseorAttack)

############################# modelo con 16 variable ############################################


set.seed(1)
sample.index4 <- sample(1:nrow(H_A_DF)
                        ,nrow(H_A_DF)*0.7
                        ,replace = F)

predictors4 <- c("Diabetes_012","HighBP","HighChol","BMI", "Smoker","Stroke","PhysActivity"
                 ,"Fruits","Veggies","HvyAlcoholConsump", "AnyHealthcare","GenHlth","PhysHlth","DiffWalk"
                 ,"Sex","Age")

## datos de entrenamiento y prueba
train.data4 <- H_A_DF[sample.index4,c(predictors4,"HeartDiseaseorAttack"),drop=F]
test.data4 <- H_A_DF[-sample.index4,c(predictors4,"HeartDiseaseorAttack"),drop=F]

## primer modelo knn
ctrl4 <- trainControl(method="cv",  number = 5)
knnFit4 <- train(HeartDiseaseorAttack ~ Diabetes_012 + HighBP + HighChol + BMI + Smoker + Stroke + PhysActivity
                 + Fruits + Veggies + HvyAlcoholConsump + AnyHealthcare + GenHlth + PhysHlth + DiffWalk
                 + Sex + Age
                 , data = train.data4
                 , method = "knn", trControl = ctrl4
                 , preProcess = c("range") #c("center", "scale")
                 , tuneLength = 25)

knnFit4
plot(knnFit4)

#### predicciones

knnPredict4 <- predict(knnFit4, newdata = test.data4)
knnPredict4
confusionMatrix(knnPredict4, test.data4$HeartDiseaseorAttack)


############################# modelo con 11 variable ############################################


set.seed(1)
sample.index5 <- sample(1:nrow(H_A_DF)
                        ,nrow(H_A_DF)*0.7
                        ,replace = F)

predictors5 <- c("HighBP","HighChol","BMI", "Smoker","PhysActivity"
                 ,"Veggies","HvyAlcoholConsump","PhysHlth","DiffWalk"
                 ,"Sex","Age")

## datos de entrenamiento y prueba
train.data5 <- H_A_DF[sample.index5,c(predictors5,"HeartDiseaseorAttack"),drop=F]
test.data5 <- H_A_DF[-sample.index5,c(predictors5,"HeartDiseaseorAttack"),drop=F]

## primer modelo knn
ctrl5 <- trainControl(method="cv",  number = 3)
knnFit5 <- train(HeartDiseaseorAttack ~  HighBP + HighChol + BMI + Smoker + PhysActivity
                  + Veggies + HvyAlcoholConsump  + PhysHlth + DiffWalk
                 + Sex + Age
                 , data = train.data5
                 , method = "knn", trControl = ctrl5
                 , preProcess = c("range") #c("center", "scale")
                 , tuneLength = 25)

knnFit5
plot(knnFit5)

#### predicciones

knnPredict5 <- predict(knnFit5, newdata = test.data5)
knnPredict5
confusionMatrix(knnPredict5, test.data5$HeartDiseaseorAttack)



####################################### modelo knn sex ##################################

SEX <- DIABETES
SEX$Sex <- as.factor(SEX$Sex)

set.seed(1)
SeX <- SEX %>%
  group_by(Sex) %>%
  sample_n(1268, replace = FALSE) %>%
  ungroup()

############################### modelo con 21 variables predictoras ##########################################
set.seed(1)
sample.index6 <- sample(1:nrow(SeX)
                        ,nrow(SeX)*0.7
                        ,replace = F)

predictors6 <- c("Diabetes_012","HighBP","HighChol","CholCheck","BMI", "Smoker","Stroke","PhysActivity"
                 ,"Fruits","Veggies","HvyAlcoholConsump", "AnyHealthcare","NoDocbcCost","GenHlth","MentHlth","PhysHlth","DiffWalk"
                 ,"HeartDiseaseorAttack","Age","Education","Income")

## datos de entrenamiento y prueba
train.data6 <- SeX[sample.index6,c(predictors6,"Sex"),drop=F]
test.data6 <- SeX[-sample.index6,c(predictors6,"Sex"),drop=F]

## primer modelo knn
ctrl6 <- trainControl(method="cv",  number = 10)
knnFit6 <- train(Sex ~.
                 , data = train.data6
                 , method = "knn", trControl = ctrl6
                 , preProcess = c("range") #c("center", "scale")
                 , tuneLength = 25)

knnFit6
plot(knnFit6)

#### predicciones

knnPredict6 <- predict(knnFit6, newdata = test.data6)
knnPredict6
confusionMatrix(knnPredict6, test.data6$Sex)

############################### modelo con 16 variables predictoras ##########################################
set.seed(1)
sample.index7 <- sample(1:nrow(SeX)
                        ,nrow(SeX)*0.7
                        ,replace = F)

predictors7 <- c("Diabetes_012","HighBP","HighChol","BMI", "Smoker","PhysActivity"
                 ,"Fruits","Veggies","HvyAlcoholConsump","MentHlth","PhysHlth","DiffWalk"
                 ,"HeartDiseaseorAttack","Age","Education","Income")

## datos de entrenamiento y prueba
train.data7 <- SeX[sample.index7,c(predictors7,"Sex"),drop=F]
test.data7 <- SeX[-sample.index7,c(predictors7,"Sex"),drop=F]

## primer modelo knn
ctrl7 <- trainControl(method="cv",  number = 5)
knnFit7 <- train(Sex ~ Diabetes_012 + HighBP + HighChol + BMI + Smoker + PhysActivity
                 + Fruits + Veggies + HvyAlcoholConsump + MentHlth + PhysHlth + DiffWalk
                 + HeartDiseaseorAttack + Age + Education + Income
                 , data = train.data7
                 , method = "knn", trControl = ctrl7
                 , preProcess = c("range") #c("center", "scale")
                 , tuneLength = 25)

knnFit7
plot(knnFit7)

#### predicciones

knnPredict7 <- predict(knnFit7, newdata = test.data7)
knnPredict7
confusionMatrix(knnPredict7, test.data7$Sex)


############################### modelo con 11 variables predictoras ##########################################
set.seed(1)
sample.index8 <- sample(1:nrow(SeX)
                        ,nrow(SeX)*0.7
                        ,replace = F)

predictors8 <- c("Diabetes_012","BMI", "Smoker","PhysActivity"
                 ,"HvyAlcoholConsump","MentHlth","DiffWalk"
                 ,"HeartDiseaseorAttack","Age","Education","Income")

## datos de entrenamiento y prueba
train.data8 <- SeX[sample.index8,c(predictors8,"Sex"),drop=F]
test.data8 <- SeX[-sample.index8,c(predictors8,"Sex"),drop=F]

## primer modelo knn
ctrl8 <- trainControl(method="cv",  number = 3)
knnFit8 <- train(Sex ~ Diabetes_012 + BMI + Smoker + PhysActivity
                  + HvyAlcoholConsump + MentHlth  + DiffWalk
                 + HeartDiseaseorAttack + Age + Education + Income
                 , data = train.data8
                 , method = "knn", trControl = ctrl8
                 , preProcess = c("range") #c("center", "scale")
                 , tuneLength = 25)

knnFit8
plot(knnFit8)

#### predicciones

knnPredict8 <- predict(knnFit8, newdata = test.data8)
knnPredict8
confusionMatrix(knnPredict8, test.data8$Sex)

##########################################################################################################

########### MODELO DE REGRESIÓN LINEAL ####################################################

Diabetes<- DIABETES_DF[sample(nrow(DIABETES_DF), 2536),]


######################## Muestreo ###########################################################
set.seed(1)
index.train9 <- sample(1:nrow(Diabetes)
                      ,0.7*nrow(Diabetes)
                      ,replace = FALSE)

########################### Modelo 1 con variable target BWI ###########################3

predictors9 <- colnames(Diabetes)[-5]

train.data9 <- Diabetes[index.train9,c(predictors9,"BMI"),drop=F]
test.data9 <- Diabetes[-index.train9,c(predictors9,"BMI"),drop=F]

ins_model <-lm(BMI ~ ., data = train.data9)
ins_model

train.output <- predict(ins_model, newdata = test.data9)
summary(ins_model)

########## Error cuadratico medio 1
MSE_1<- data.frame(predicted = train.output
             ,actual = test.data9$BMI
             ,MSE_1 =((train.output - test.data9$BMI)^2)/nrow(test.data9))

MSE_1$RMSE_1 <- sqrt(MSE_1$MSE_1)

################## Modelo 2 con variable target MentHlth
predictors10 <- colnames(Diabetes)[-16]

train.data10 <- Diabetes[index.train9,c(predictors10,"MentHlth"),drop=F]
test.data10 <- Diabetes[-index.train9,c(predictors10,"MentHlth"),drop=F]

ins_model1 <-lm(MentHlth ~ ., data = train.data10)
ins_model1

train.output1 <- predict(ins_model1, newdata = test.data10)
summary(ins_model1)

# Error cuadratico medio 2
MSE_2<- data.frame(predicted = train.output1
             ,actual = test.data10$MentHlth
             ,MSE_2 =((train.output1 - test.data10$MentHlth)^2)/nrow(test.data10))

MSE_2$RMSE_2 <- sqrt(MSE_2$MSE_2)


############ Modelo 3 con variable target PhysHlth

predictors11 <- colnames(Diabetes)[-17]

train.data11 <- Diabetes[index.train9,c(predictors11,"PhysHlth"),drop=F]
test.data11 <- Diabetes[-index.train9,c(predictors11,"PhysHlth"),drop=F]

ins_model2 <-lm(PhysHlth ~ ., data = train.data11)
ins_model2

train.output2 <- predict(ins_model2, newdata = test.data11)
summary(ins_model2)

# Error cuadratico medio 3
MSE_3<- data.frame(predicted = train.output2
             ,actual = test.data11$PhysHlth
             ,MSE_3 =((train.output2 - test.data11$PhysHlth)^2)/nrow(test.data11))

MSE_3$RMSE_3 <- sqrt(MSE_3$MSE_3)
