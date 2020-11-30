# regularised regression

prostate <- read.table(file="prostate.data.txt", sep = "\t", header = TRUE)
str(prostate)
library(caret)
small <- prostate[1:5,]
# lots of predictors.  Some parameters get estimates, others get na
lm(lpsa ~ ., data=small)

# combining predictors
# model stacking
library(ISLR); library(caret); data(Wage); library(ggplot2)
Wage <- subset(Wage, select=-c(logwage))
summary(Wage)
inBuild <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
buildData <- Wage[inBuild,]; validation<- Wage[-inBuild,]
inTrain <- createDataPartition(y=buildData$wage,
                               p=0.7, list=FALSE)
training <- buildData[inTrain,]; testing <- buildData[-inTrain,]

dim(training)
dim(testing)
dim(validation)

#2 models
mod1 <- train(wage ~ ., method="glm", data=training)
mod2 <- train(wage ~ ., method="rf",
              data=training,
              trControl = trainControl(method="cv"), number=3)

#plot predictions against each other
pred1 <- predict(mod1, testing); pred2 <- predict(mod2, testing)
qplot(pred1, pred2, color=wage, data=testing)

#fit a model that combines predictors
predDF <- data.frame(pred1, pred2, wage=testing$wage)
combModFit <- train(wage ~ ., method="gam", data=predDF)
combPred <- predict(combModFit, predDF)

#comparing errors
sqrt(sum((pred1-testing$wage)^2))
sqrt(sum((pred2-testing$wage)^2))
sqrt(sum((combPred-testing$wage)^2))

#predict on validation
pred1V <- predict(mod1, validation); pred2V <- predict(mod2, validation)
predVDF <- data.frame(pred1=pred1V, pred2 = pred2V)
combPredV <- predict(combModFit, predVDF)

#evaluate
sqrt(sum((pred1V-validation$wage)^2))
sqrt(sum((pred2V-validation$wage)^2))
sqrt(sum((combPredV-validation$wage)^2))

# Forecasting
library(quantmod)
from.dat <- as.Date("01/01/08", format="%m/%d/%y")
to.dat <- as.Date("12/31/13", format="%m/%d/%y")
getSymbols("GOOG", src="yahoo", from=from.dat, to=to.dat)
head(GOOG)

mGoog <- to.monthly(GOOG)
googOpen <- Op(mGoog)
ts1 <- ts(googOpen, frequency=12)
plot(ts1, xlab="years+1", ylab="GOOG")

#decompose
plot(decompose(ts1), xlab="Years+1")

#split from a moment in time
ts1Train <- window(ts1, start=1, end=5)
ts1Test <- window(ts1, start=5, end=(7-0.01))
ts1Train

#simple moving average
library(forecast)
plot(ts1Train)
lines(ma(ts1Train, order=3), col="red")

#exponential smoothing
ets1 <- ets(ts1Train, model="MMM")
fcast <- forecast(ets1)
plot(fcast); lines(ts1Test, col="red")

accuracy(fcast, ts1Test)

# unsupervised learning
data(iris); library(ggplot2)
library(dplyr)
names(iris)
table(iris$Species)
inTrain <- createDataPartition(iris$Species,
                               p=0.7, list=FALSE)
testing <- iris[inTrain,]
training <- iris[-inTrain,]
dim(training); dim(testing)

KMeans1 <- kmeans(subset(training, select=-c(Species)), centers=3)
training$clusters <- as.factor(KMeans1$cluster)
qplot(Petal.Length, Petal.Width, colour=clusters, data=training)

table(KMeans1$cluster, training$Species)

#build predictor
modFit <- train(clusters ~ ., data=subset(training, select=-c(Species)),
                   method="rpart")
table(predict(modFit, training), training$Species)

table(predict(modFit, testing), testing$Species)
