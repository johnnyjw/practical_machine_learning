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