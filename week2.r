library(caret)
library(kernlab)
library(dplyr)
library(gridExtra)
library(RANN)
data(spam)

# my first caret
inTrain <- createDataPartition(y = spam$type,
                               p = 0.75,
                               list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

#train model
set.seed(32343)
modelFit <- train(type ~., data=training, method="glm")
modelFit

# look up coefficients
modelFit$finalModel

# predict
predictions <- predict(modelFit, newdata=testing)
predictions

# confusion matrix
confusionMatrix(predictions, testing$type)


# Data Slicing
# k fold
set.seed(32323)
folds <- createFolds(y=spam$type, k=10,
                     list=TRUE, returnTrain=TRUE)
sapply(folds, length)

folds[[1]][1:10]

# returnTrain=False
folds <- createFolds(y=spam$type, k=10,
                     list=TRUE, returnTrain=FALSE)
sapply(folds, length)

folds[[1]][1:10]


# resampling (random with replacement)
folds <- createResample(y=spam$type, times=10,
                     list=TRUE)
sapply(folds, length)

folds[[1]][1:10]

# Time Slices
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow=20,
                          horizon=10)
names(folds)
folds$train[[1]]

folds$test[[1]]

# Training Options
inTrain <- createDataPartition(y = spam$type,
                               p = 0.75,
                               list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
set.seed(32343)
modelFit <- train(type ~., data=training, method="glm")
modelFit

args(caret::train)

#example
set.seed(1235)
modelFit2 <- train(type ~., data=training, method="glm")
modelFit2

# same seed
set.seed(1235)
modelFit3 <- train(type ~., data=training, method="glm")
modelFit3

#plotting predictors
# using data from introduction to statistical learning
library(ISLR)
library(ggplot2)
# library(caret)
data(Wage)
summary(Wage)
inTrain <- createDataPartition(y = Wage$wage,
                               p = 0.7,
                               list = FALSE)
training <- Wage[inTrain,]
# set testing aside until the end
testing <- Wage[-inTrain,]
dim(training);dim(testing)

# featurePlot from caret
featurePlot(x = training[, c("age", "education", "jobclass")],
            y = training$wage,
            plot = "pairs")

# qplot (quickplot) from ggplot2
qplot(age, wage, data=training)

training %>% 
  ggplot(aes(age, wage)) +
  geom_point()

qplot(age, wage, colour=jobclass, data=training)

training %>% 
  ggplot(aes(age, wage, colour=jobclass)) +
  geom_point()

# regression smoothers
qq <- qplot(age, wage, colour=education, data=training)

qq + geom_smooth(method = 'lm', formula=y~x)

training %>% 
  ggplot(aes(age, wage, colour=education)) +
   geom_point() +
   geom_smooth(method = 'lm', formula=y~x)

# cut2 - making factors
library(Hmisc)
cutWage <- cut2(training$wage, g=3)
table(cutWage)

qplot(cutWage, age, data=training,
      fill=cutWage,
      geom = c("boxplot"))

update_train <- training %>% 
  mutate(cutWage = cutWage)

update_train %>% 
  ggplot(aes(cutWage, age, fill=cutWage)) +
    geom_boxplot()

q1 <- update_train %>% 
  ggplot(aes(cutWage, age, fill=cutWage)) +
  geom_boxplot()

q2 <- update_train %>% 
  ggplot(aes(cutWage, age, fill=cutWage)) +
  geom_boxplot() +
  geom_jitter()

grid.arrange(q1, q2, ncol=2)

# tables
t1 <- table(cutWage, training$jobclass)
t1

prop.table(t1, 1)

# density plot
qplot(wage, colour=education, data=training,
      geom="density")

update_train %>% 
  ggplot(aes(wage, colour=education)) +
    geom_density()

# Preprocessing
inTrain <- createDataPartition(y = spam$type,
                               p = 0.75,
                               list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

hist(training$capitalAve, main="", xlab="ave. capital run length")

mean(training$capitalAve)
sd(training$capitalAve)

trainingCapaveS <- (training$capitalAve - mean(training$capitalAve))/
                      sd(training$capitalAve)
mean(trainingCapaveS)
sd(trainingCapaveS)

# applying training sd mean to test set
testingCapaveS <- (testing$capitalAve - mean(training$capitalAve))/
  sd(training$capitalAve)
mean(testingCapaveS)
sd(testingCapaveS)

#preprocess
preObj <- preProcess(training[,-58], method = c("center", "scale"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

testCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

modelFit <- train(type ~., data=training,
                  preProcess=c("center", "scale"),
                  method="glm")
modelFit

# box cox transformation
preObj <- preProcess(training[,-58], method = c("BoxCox"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)

# imputation
set.seed(13343)

# make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size=1,prob=0.05)==1
training$capAve[selectNA] <- NA

# Impute and Standardise
preObj <- preProcess(training[,-58], method="knnImpute")
capAve <- predict(preObj, training[,-58])$capAve

# Standardise the truth values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth))/sd(capAveTruth)

quantile(capAve - capAveTruth)
quantile((capAve - capAveTruth)[selectNA])
quantile((capAve - capAveTruth)[! selectNA])
