

#Question 2

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

str(concrete)

plot(mixtures$CompressiveStrength,pch=19)

cor(mixtures$CompressiveStrength,mixtures$Age)

#Explorartory data analysis

library(psych)

pairs.panels(concrete)

#Finding out zero Covariates

n <- nearZeroVar(concrete,saveMetrics=TRUE)
n

#Question 3

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

summary(concrete)
str(concrete)

hist(mixtures$Superplasticizer)
hist(log(mixtures$Superplasticizer)+1)

hist(log(training$Superplasticizer)+1)

#Question 4

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

str(adData)
summary(adData)

#Getting all the variables which begins with IL
data_frame <- training[,grep("^IL", names(training), value=TRUE)]

str(data_frame)
summary(data_frame)

pre_obj <- preProcess(data_frame, method=c("center", "scale", "pca"), thresh=0.9)

pre_obj

#Question 5

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

str(adData)
summary(adData)

#Getting all the variables which begins with IL from training data
data_frame <- training[,grep("^IL", names(training), value=TRUE)]
data_frame$diagnosis <- training$diagnosis
str(data_frame)
summary(data_frame)

#Getting all the variables which begins with IL from testing data

data_frame_test <- testing[,grep("^IL", names(testing), value=TRUE)]
data_frame_test$diagnosis <- testing$diagnosis
str(data_frame_test)
summary(data_frame_test)


#Model with pca

pre_obj <- preProcess(data_frame[, -13], method=c("center", "scale", "pca"), thresh=0.8)

train_obj <- predict(pre_obj,data_frame[, -13])

modelfit <- train(data_frame$diagnosis ~ .,method="glm",data=train_obj)


test_obj <- predict(pre_obj,data_frame_test[, -13])

pca_result <- confusionMatrix(data_frame_test[, 13], predict(modelfit, test_obj))
pca_result

#Model without pca

non_pca_model <- train(diagnosis ~ ., data=data_frame, method="glm")

non_pca_result <- confusionMatrix(data_frame_test[, 13], predict(non_pca_model, data_frame_test[, -13]))
non_pca_result

