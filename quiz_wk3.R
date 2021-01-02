library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(dplyr)
testing <- segmentationOriginal %>% filter(Case=="Test")
training <- segmentationOriginal %>% filter(Case=="Train")
dim(training); dim(testing)

set.seed(125)
model <- train(
  Class ~., data = training, method = "rpart"
)

predict(model, newdata = testing)


testA <- testing 
testA$TotalIntenCh2 <- 23000
testA$FiberWidthCh1 <- 10
testA$PerimStatusCh1 <- 2


testB <- testing
testB$TotalIntench2 <-  50000
testB$FiberWidthCh1 <-  10
testB$VarIntenCh4 <-  100

testC <- testing
testC$TotalIntench2 <-  57000
testC$FiberWidthCh1 <-  8
testC$VarIntenCh4 <-  100

testD <- testing
testD$FiberWidthCh1 <-  8
testD$VarIntenCh4 <-  100
testD$PerimStatusCh1 <- 2

predict(model, newdata = testA)
predict(model, newdata = testB)
predict(model, newdata = testC)
predict(model, newdata = testD)

par(mar=c(1,1,1,1))

library(rattle)

par(mar=c(4,4,4,4))
fancyRpartPlot(model$finalModel)

# 3
library(pgmm)
data(olive)
olive = olive[,-1]
model <- train(
  Area ~., data = olive, method = "rpart"
)

newdata = as.data.frame(t(colMeans(olive)))
predict(model, newdata = newdata)


# 4
library(caret)
SAheart <- read.table(file="sa_heart.txt", sep = ",", header = TRUE)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
model <- train(
  chd ~ age + alcohol + obesity + tobacco + typea + ldl, data = trainSA, method = "glm", family=binomial()
)

model <- train(
  as.factor(chd) ~ age*alcohol*obesity*tobacco*typea*ldl, data = trainSA, method = "glm", family=binomial()
)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(testSA$chd, as.integer(predict(model, newdata = testSA)) - 1)
missClass(trainSA$chd, as.integer(predict(model, newdata = trainSA)) - 1)

# 5 
vowel.train <- read.table(file="vowel_train.txt", sep = ",", header = TRUE)
vowel.test <- read.table(file="vowel_test.txt", sep = ",", header = TRUE)

vowel.train["row.names"] = NULL
vowel.test["row.names"] = NULL

set.seed(33833)
library(randomForest)
modFit <- randomForest(Y ~ ., data=vowel.train, proximity=TRUE)
imp <- importance(modFit)
