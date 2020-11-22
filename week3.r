# Week 3: Trees
data(iris); library(ggplot2)
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
