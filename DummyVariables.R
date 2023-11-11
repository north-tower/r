library(ISLR)
library(caret)

data("diamonds")
head(diamonds)
#Convert from ordered factor to factor
diamonds$cut <- factor(diamonds$cut, ordered=F)
diamonds$color <- factor(diamonds$color, ordered=F)
diamonds$clarity <- factor(diamonds$clarity, ordered=F)
#Use dummyVars in caret to create one hot encoding/dummy variables
dmy <- dummyVars("~ .", data = diamonds, fullRank = T)
diamonds <- data.frame(predict(dmy, newdata = diamonds))

#Another example
customers <- data.frame(
  id=c(10,20,30,40,50,100),
  gender=c('male','female','female','male','female','Male'),
  mood=c('happy','sad','happy','sad','happy','neutral'),
  class=c(1,1,0,0,0,0))

#No conversion is needed as the gender and mood column are already factors
dmy <- dummyVars(" ~ .", data = customers, fullRank = T)
customers <- data.frame(predict(dmy, newdata = customers))
