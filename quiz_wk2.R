# q1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]
dim(adData);dim(training);dim(testing)

#q 2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

dim(training)

library(Hmisc)
ggplot(training, aes(as.integer(row.names(training)), CompressiveStrength, colour=cut2(Cement))) +
  geom_point() 

ggplot(training, aes(as.integer(row.names(training)), CompressiveStrength, colour=cut2(BlastFurnaceSlag))) +
  geom_point() 

ggplot(training, aes(as.integer(row.names(training)), CompressiveStrength, colour=cut2(FlyAsh))) +
  geom_point() 

ggplot(training, aes(as.integer(row.names(training)), CompressiveStrength, colour=cut2(Water))) +
  geom_point() 

ggplot(training, aes(as.integer(row.names(training)), CompressiveStrength, colour=cut2(Superplasticizer))) +
  geom_point() 

ggplot(training, aes(as.integer(row.names(training)), CompressiveStrength, colour=cut2(CoarseAggregate))) +
  geom_point() 

ggplot(training, aes(as.integer(row.names(training)), CompressiveStrength, colour=cut2(FineAggregate))) +
  geom_point() 

ggplot(training, aes(as.integer(row.names(training)), CompressiveStrength, colour=cut2(Age))) +
  geom_point() 

# 3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
hist(training$Superplasticizer)
Superplasticizer <- training$Superplasticizer
hist(log(Superplasticizer + 1))

# 4
library(dplyr)
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433);data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]];training = adData[ inTrain,]
testing = adData[-inTrain,]

interleukins <- training %>% 
  select(IL_11:IL_8)
M <- abs(cor(interleukins))
diag(M) <- 0
which(M > 0.8, arr.ind = T)

prComp <- prcomp(interleukins)
prComp$rotation
summary(prComp)

princComp <- preProcess(interleukins, method="pca", thresh = 0.8)
summary(princComp)
princComp


prComp <- prcomp(interleukins, scale = TRUE, center = TRUE)
summary(prComp)

# 5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433);data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]];training = adData[ inTrain,]
testing = adData[-inTrain,]

testing <- testing %>% 
  select(diagnosis, IL_11:IL_8)
training <- training %>% 
  select(diagnosis, IL_11:IL_8)

modelFit <- train(diagnosis ~ ., method = "glm", data = training)
# testing
confusionMatrix(testing$diagnosis, predict(modelFit,testing))

preProc <- preProcess(training, method="pca", pcaComp = 7)
trainPC <- predict(preProc, training)
trainPC$diagnosis <- training$diagnosis
modelFit <- train(diagnosis ~ ., method = "glm", data = trainPC)
# testing
testPC <- predict(preProc, testing)
confusionMatrix(testing$diagnosis, predict(modelFit,testPC))