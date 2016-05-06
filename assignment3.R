#Question 1

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

inTrain <- createDataPartition(y = segmentationOriginal$Case, p = 0.6, 
                               list = FALSE) # 60% training
training_data <- segmentationOriginal[inTrain, ]
testing_data <- segmentationOriginal[-inTrain, ]

set.seed(125)
model <- train(Class ~ ., method = "rpart", data = training_data)

#Get the final model
model$finalModel

library(rattle)
library(rpart.plot)

fancyRpartPlot(model$finalModel)


#question 3

library(pgmm)
data(olive)
olive = olive[,-1]

newdata = as.data.frame(t(colMeans(olive)))

model <- train(Area ~ ., method = "rpart", data = olive)
model$finalModel
predict(model$finalModel, newdata = newdata)

#question 4

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

missClass = function(values, prediction){sum(((prediction > 0.5) * 1) != values) / length(values)}

set.seed(13234)
model <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
                 data = trainSA, method = "glm", family = "binomial")

#Missclassification on the test set
missClass(testSA$chd, predict(model, newdata = testSA))

#Missclassification on the training set
missClass(trainSA$chd, predict(model, newdata = trainSA))

#Question 5

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)
library(randomForest)

model <- randomForest(y ~ .,data=vowel.train)

order(varImp(model), decreasing = T)
