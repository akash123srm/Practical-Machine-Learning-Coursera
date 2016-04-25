library(ISLR)
library(ggplot2)
library(caret)

data(Wage)

str(Wage)

summary(Wage)

in_train <- createDataPartition(y=Wage$wage, p=0.7,list=FALSE)

train_data <- Wage[in_train,]

test_data <- Wage[-in_train,]

qplot(age,wage,colour=jobclass,data=train_data)

q <- qplot(age,wage,colour=education,data=train_data)

q + geom_smooth(method='lm',formula=y~x)

# We will break age variable into different categories

cut_wage <- cut_interval(train_data$wage,n=3)

table(cut_wage)

wage_plot <- qplot(cut_wage,age,data=train_data,fill=cut_wage,geom=c("boxplot"))
wage_plot

#Comparing wage with the job class

t1 <- table(cut_wage,train_data$jobclass)
t1

# Comparing wage with the education

t2 <- table(cut_wage,train_data$education)
t2

cut_age <- cut_interval(train_data$age,n=3)
cut_age

table(cut_age)


table(cut_age,train_data$education)


#Basic Preprocessing

library(caret)
library(kernlab)

#Loading the data

data(spam)

str(spam)

in_train <- createDataPartition(y=spam$type, p=0.7,list=FALSE)

train_data <- spam[in_train,]

test_data <- spam[-in_train,]

dim(train_data)
dim(test_data)

hist(train_data$capitalAve,xlab="Avg capital run length")

mean(train_data$capitalAve)
sd(train_data$capitalAve)

#We use preProcess function to standardize the highly skewed variable

pre_o <- preProcess(train_data[,-58],method=c("center","scale"))

processed_values <- predict(pre_o,train_data[,-58])

processed_capitalAve <- processed_values$capitalAve

mean(processed_capitalAve)
sd(processed_capitalAve)

hist(processed_capitalAve,xlab="Processed Avg capital run length")

#Boxcox transformation

pre_o <- preProcess(train_data[,-58],method=c("BoxCox"))

processed_values <- predict(pre_o,train_data[,-58])

processed_capitalAve <- processed_values$capitalAve

mean(processed_capitalAve)
sd(processed_capitalAve)

hist(processed_capitalAve,xlab="Processed Avg capital run length")

#Data imputation.Here we create some missing values(NA's) and replace them by k nearest neighbour method

set.seed(13433)

select_NA <- rbinom(dim(train_data)[1], size=1, prob=0.05)==1

train_data$capitalAve[select_NA] <- NA

#Impute the values using k nearest neighbour

preobj <- preProcess(train_data[,-58],method="knnImpute")
processed_values <- predict(preobj,train_data[,-58])

capitalAve_imputed <- processed_values$capitalAve
