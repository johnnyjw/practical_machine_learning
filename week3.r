# Week 3: Trees
data(iris); library(ggplot2)
library(dplyr)
names(iris)
table(iris$Species)
inTrain <- createDataPartition(iris$Species,
                               p=0.7, list=FALSE)
testing <- iris[inTrain,]
training <- iris[-inTrain,]
dim(training); dim(testing)

qplot(Petal.Width, Sepal.Width, colour=Species, data=training)

library(caret)
modFit <- train(Species ~ ., method="rpart", data=training)
print(modFit$finalModel)

#plot the tree
plot(modFit$finalModel, uniform=TRUE,
     main="Classificaation Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

library(rattle)
fancyRpartPlot(modFit$finalModel)

predict(modFit, newdata=testing)

# Bagging
ozone <- read.table(file="ozone.data.txt", sep = "\t", header = TRUE)
ozone <- ozone %>% 
  arrange(ozone)
head(ozone)

# Bagged loess
ll <- matrix(NA, nrow=10, ncol=155)
for(i in 1:10){
  ss <- sample(1:dim(ozone)[1], replace=T)
  ozone0 <- ozone[ss,]; ozone0 <- ozone0[order(ozone0$ozone),]
  loess0 <- loess(temperature ~ ozone, data=ozone0, span=0.2)
  ll[i,] <- predict(loess0, newdata=data.frame(ozone=1:155))
}

plot(ozone$ozone, ozone$temperature, pch=19, cex=0.5)
for(i in 1:10){lines(1:155, ll[i,], col="grey", lwd=2)}
lines(1:155, apply(ll,2,mean), col="red", lwd=2)

# make your own bagging
predictors = data.frame(ozone=ozone$ozone)
temperature = ozone$temperature
treebag <- bag(predictors, temperature, B = 10,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))

plot(ozone$ozone, temperature, col='lightgrey', pch=19)
points(ozone$ozone,predict(treebag$fits[[1]]$fit, predictors), pch=19, col="red")
points(ozone$ozone,predict(treebag,predictors), pch=19, col="blue")

# the bits of the function
ctreeBag$fit
ctreeBag$pred
ctreeBag$aggregate

# Random Forest
data(iris); library(ggplot2); library(caret)
library(randomForest)
inTrain <- createDataPartition(iris$Species,
                               p=0.7, list=FALSE)
testing <- iris[inTrain,]
training <- iris[-inTrain,]
dim(training); dim(testing)

modFit <- train(Species ~ ., data=training, method="rf", prox=TRUE)
modFit

getTree(modFit$finalModel, k=2)

# class centers
irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP); irisP$Species <- rownames(irisP)
training %>%
  ggplot(aes(Petal.Width, Petal.Length, col=Species)) +
  geom_point() +
  geom_point(size=5, shape=4, data=irisP)

pred <- predict(modFit, testing); testing$predRight <- pred==testing$Species  
table(pred, testing$Species)

#map correct and failed predictions
qplot(Petal.Width, Petal.Length, colour=predRight, data=testing, main="New data predictions")

# boosting
library(ISLR); library(caret); data(Wage); library(ggplot2)
Wage <- subset(Wage, select=-c(logwage))
summary(Wage)
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

dim(training); dim(testing)

modFit <- train(wage ~ ., method="gbm", data=training, verbose=FALSE)
print(modFit)

# plot predictions
qplot(predict(modFit, testing), wage, data=testing)
