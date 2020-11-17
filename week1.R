library(kernlab)
library(dplyr)
library(ggplot2)
library(tidyr)
data(spam)
head(spam)

#plotting spam and nonspam and the frequency of your
plot(density(spam$your[spam$type=="nonspam"]),
    col="blue", main="", xlab="Frequency of 'your'")
lines(density(spam$your[spam$type=="spam"]), col="red")

spam %>% filter(type=="nonspam")
dens <- as.tibble(density)

ggplot(spam, aes(your, color=type)) +
  geom_density() +
  scale_x_continuous("Frequency of 'your'",
                     limits = c(-1, 11)) 

#algorithm - cut off a constant value for your

ggplot(spam, aes(your, color=type)) +
  geom_density() +
  scale_x_continuous("Frequency of 'your'",
                     limits = c(-1, 11)) +
  geom_vline(xintercept = 0.5)

prediction <- ifelse(spam$your > 0.5, "spam", "notspam")
table(prediction, spam$type) / length(spam$type)

# accuracy is .45 + .29 = .75 (right 75% of time)

data(spam)
set.seed(333)
# count of capital letters in dataset.  If lots of caps more likely to be spam
smallSpam <- spam[sample(dim(spam)[1], size=10),]
rownames(smallSpam) <- NULL


ggplot(smallSpam, aes(as.integer(row.names(smallSpam)), capitalAve, color = type)) +
  geom_point() +
  scale_x_continuous("index", breaks = seq(0, 10, by=1)) +
  scale_color_discrete("Label")

# could appply a rule that captured all the spam in the dataset
# rule 1
rule1 <- function(x){
  prediction <- rep(NA, length(x))
  prediction[x < 1.7] <- "nonspam"
  prediction[x > 17] <- "nonspam"
  prediction[x >= 1.7 & x < 1.79] <- "spam"
  prediction[x >= 1.9 & x < 3] <- "nonspam"
  prediction[x >= 3 & x < 17] <- "spam"
  return(prediction)
}

table(rule1(smallSpam$capitalAve), smallSpam$type)

# I do not know why one value is missing from table function

# apply a more general rule
# rule 2
rule2 <- function(x){
  prediction <- rep(NA, length(x))
  prediction[x < 2.8] <- "nonspam"
  prediction[x >= 2.8] <- "spam"
  return(prediction)
}

# not so good on the prediction
table(rule2(smallSpam$capitalAve), smallSpam$type)

#but rule2 more generalizable
table(rule1(spam$capitalAve), spam$type)
table(rule2(spam$capitalAve), spam$type)

#accuracy
spamType <- paste(spam$type)
spamPredict1 <- paste(rule1(spam$capitalAve))
spamPredict2 <- paste(rule2(spam$capitalAve))
sum(spamPredict1 == spamType)
sum(spamPredict2 == spamType)
