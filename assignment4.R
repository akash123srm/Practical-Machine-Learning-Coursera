#Question 1

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

model1 <- train(y ~ .,data=vowel.train,method="rf")

model2 <- train(y ~ .,data=vowel.train,method="gbm")

confusionMatrix(predict(model1,vowel.test), vowel.test$y)$overall[1]
confusionMatrix(predict(model2,vowel.test), vowel.test$y)$overall[1]

#Question 2

library(caret)

library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]

set.seed(62433)

model1 <- train(diagnosis ~ .,data=training,method="rf")
predictions1 <- predict(model1,testing)
confusionMatrix(predictions1, testing$diagnosis)$overall[1]

model2 <- train(diagnosis ~ .,data=training,method="gbm")
predictions2 <- predict(model2,testing)
confusionMatrix(predictions2, testing$diagnosis)$overall[1]

model3 <- train(diagnosis ~ .,data=training,method="lda")
predictions3 <- predict(model3,testing)
confusionMatrix(predictions3, testing$diagnosis)$overall[1]

stacked_data <- data.frame(predictions1,predictions2,predictions3,diagnosis=testing$diagnosis)
model_stacked <- train(diagnosis ~ .,data=stacked_data,method="rf")
predictions_stacked <- predict(model_stacked,stacked_data)

confusionMatrix(predictions_stacked, testing$diagnosis)$overall[1]

#Question 3

set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(233)

model_lasso <- train(CompressiveStrength ~ .,data=training,method="lasso")

library(elasticnet)
plot.enet(model_lasso$finalModel, xvar = "penalty", use.color = TRUE)

#Question 4

library(lubridate)  
dat = read.csv(file.choose())
training = dat[year(dat$date) < 2012, ]
testing = dat[(year(dat$date)) > 2011, ]
tstrain = ts(training$visitsTumblr)

library(forecast)
mod_ts <- bats(tstrain)
fcast <- forecast(mod_ts, level = 95, h = dim(testing)[1])
sum(fcast$lower < testing$visitsTumblr & testing$visitsTumblr < fcast$upper) / 
  dim(testing)[1]


#Question 5

set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(325)
library(e1071)

model_svm <- svm(CompressiveStrength ~ ., data = training)

predict_svm <- predict(model_svm,testing)

library(Metrics)
rmse(testing$CompressiveStrength,predict_svm)

