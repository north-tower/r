
# Task 1 ------------------------------------------------------------------

credit_data <- read.csv("credit_data.txt",sep = ',',dec = ".", na.strings = c(NA," "))
#head(credit_data)
dim(credit_data)

#
#credit_data <- read.table("credit_data.txt", skipNul = T)#, sep = ',')
str(credit_data)
#
credit_data$CARDHLDR <- factor(credit_data$CARDHLDR)#, levels = c(0,1))
credit_data$DEFAULT <- factor(credit_data$DEFAULT)#, levels = c(0,1))
credit_data$ADEPCNT <- factor(credit_data$ADEPCNT)#, levels = c(0:9))
credit_data$MAJORDRG <- factor(credit_data$MAJORDRG)#, levels = c(0:22))
credit_data$MINORDRG <- factor(credit_data$MINORDRG)#, levels = c(0:11))
credit_data$OWNRENT <- factor(credit_data$OWNRENT)#, levels = c(0,1))
credit_data$SELFEMPL <- factor(credit_data$SELFEMPL)#, levels = c(0,1))
#
colnames(credit_data)
#
sapply(credit_data, function(x) sum(is.na(x)))
sapply(credit_data, function(x) class(x))
#
summary(credit_data)
#
table(credit_data$CARDHLDR)
table(credit_data$DEFAULT)
table(credit_data$ADEPCNT)
table(credit_data$MAJORDRG)
table(credit_data$MINORDRG)
table(credit_data$OWNRENT)
table(credit_data$SELFEMPL)
#
table(credit_data$CARDHLDR, credit_data$DEFAULT)
table(credit_data$ADEPCNT, credit_data$DEFAULT)
table(credit_data$MAJORDRG, credit_data$DEFAULT)
table(credit_data$MINORDRG, credit_data$DEFAULT)
table(credit_data$OWNRENT, credit_data$DEFAULT)
table(credit_data$SELFEMPL, credit_data$DEFAULT)
# credit_data$AGE; credit_data$ACADMOS; credit_data$INCOME; credit_data$INCPER
# credit_data$EXP_INC; credit_data$SPENDING; credit_data$LOGSPEND

# Correlation
cor_data <- credit_data[
  c("AGE","ACADMOS","INCOME","INCPER","EXP_INC","SPENDING","LOGSPEND")] 
#Subset only numerical values

#remove observations with missing data
cor_data <- cor_data[complete.cases(cor_data),] 
#
library(ggplot2)
library(reshape) # for melting data to long format
corr_mat <- round(cor(cor_data),2) # find correlation and round values
cor_mat_melt <- melt(corr_mat) #Data to long format
corr_plot <- ggplot(cor_mat_melt, aes(x = X1, y = X2, fill = value)) +
  geom_tile() + scale_fill_continuous(type = "viridis") +
  geom_text(aes(x = X1, y = X2, label = value), colour = "white") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90,face = "bold"),
        axis.title = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(size = 10,face = "bold"))
corr_plot
#
CARDHLDR <- ggplot(credit_data, aes(x = CARDHLDR)) + 
  geom_bar() + theme_bw() + facet_wrap(.~DEFAULT) 
ADEPCNT <- ggplot(credit_data, aes(x = ADEPCNT)) + 
  geom_bar() + theme_bw() + facet_wrap(.~DEFAULT) 
MAJORDRG <- ggplot(credit_data, aes(x = MAJORDRG)) + 
  geom_bar() + theme_bw() + facet_wrap(.~DEFAULT) 
MINORDRG <- ggplot(credit_data, aes(x = MINORDRG)) + 
  geom_bar() + theme_bw() + facet_wrap(.~DEFAULT) 
OWNRENT <- ggplot(credit_data, aes(x = OWNRENT)) + 
  geom_bar() + theme_bw() + facet_wrap(.~DEFAULT) 
SELFEMPL <- ggplot(credit_data, aes(x = SELFEMPL)) + 
  geom_bar() + theme_bw() + facet_wrap(.~DEFAULT) 
#
library(gridExtra)
grid.arrange(CARDHLDR,ADEPCNT, MAJORDRG,MINORDRG,OWNRENT,SELFEMPL)
##
library(tidyverse)
hist_data <- credit_data[
  c("AGE","ACADMOS","INCOME","INCPER","EXP_INC","SPENDING","LOGSPEND")] 
hist_data$id <- 1:nrow(hist_data)
hist_data_long <- hist_data %>% 
  pivot_longer(cols = !id, names_to = "variable", values_to = "values")
#
hist_plot <- hist_data_long %>% 
  ggplot(aes(x = values, fill = variable)) + 
  geom_histogram() +
  facet_wrap(.~variable, scales = "free") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90,face = "bold"),
        axis.title = element_blank(), legend.position = "none",
        legend.title = element_blank(), legend.direction = "horizontal",
        axis.text.y = element_text(size = 10,face = "bold"))
hist_plot

# Model

library(magrittr)
library(tidyverse)
set.seed(123456)
credit_data_data <- credit_data[,-1] #Remove the cardholder
credit_data_data <- credit_data_data[complete.cases(credit_data_data),]
#credit_data_data$DEFAULT <- factor(credit_data_data$DEFAULT)
str(credit_data_data)
#
credit_data_data[c(2,3,8,10:13)] <- lapply( #3,4,9,11:14
  credit_data_data[c(2,3,8,10:13)], as.numeric)
credit_data_data <- credit_data_data %>% 
  mutate_if(is.numeric, scale) 
#

#credit_data_data <- credit_data_data[complete.cases(credit_data_data),]
# Create training and validation sets.
credit_data_data$DEFAULT <- as.numeric(credit_data_data$DEFAULT)-1
sum(is.na(credit_data_data))
#
index_train <- sample(c(1:dim(credit_data_data)[1]), dim(credit_data_data)[1]*0.60)
train_data <- credit_data_data[index_train, ]
test_data <- credit_data_data[-index_train, ]
#
DEFAULT_model <- glm(
  DEFAULT~.,
  data = train_data, family = binomial("logit"))
#
summary(DEFAULT_model)
# Logistic Regression R
DEFAULT_model_pred <- predict(DEFAULT_model, test_data, type = "response")
library(caret)
print("GLM: Training Set Confusion Matrix")
confusionMatrix(
  factor(ifelse(DEFAULT_model$fitted.values > 0.5, 1, 0), levels = c(0,1)),
  factor(train_data$DEFAULT, levels = c(0,1)),positive = "1" )
#
print("GLM: Test Set Confusion Matrix")
confusionMatrix(
  factor(ifelse(DEFAULT_model_pred > 0.5, 1, 0), levels = c(0,1)), 
  factor(test_data$DEFAULT, levels = c(0,1)), positive = "1")

#
df <- data.frame(actual = test_data$DEFAULT, 
                 predicted = DEFAULT_model_pred)
library(pROC)
roc_curv_log <- roc(df$actual, as.numeric(df$predicted)-1)
plot.roc(roc_curv_log)
grid()
##

mfx <- function(x,sims=1000){
  set.seed(12345)
  pdf <- ifelse(as.character(x$call)[3]=="binomial(link = \"probit\")",
                mean(dnorm(predict(x, type = "link"))),
                mean(dlogis(predict(x, type = "link"))))
  pdfsd <- ifelse(as.character(x$call)[3]=="binomial(link = \"probit\")",
                  sd(dnorm(predict(x, type = "link"))),
                  sd(dlogis(predict(x, type = "link"))))
  marginal.effects <- pdf*coef(x)
  sim <- matrix(rep(NA,sims*length(coef(x))), nrow=sims)
  for(i in 1:length(coef(x))){
    sim[,i] <- rnorm(sims,coef(x)[i],diag(vcov(x)^0.5)[i])
  }
  pdfsim <- rnorm(sims,pdf,pdfsd)
  sim.se <- pdfsim*sim
  res <- cbind(marginal.effects,sd(sim.se))
  colnames(res)[2] <- "standard.error"
  ifelse(names(x$coefficients[1])=="(Intercept)",
         return(res[2:nrow(res),]),return(res))
}
mfx(DEFAULT_model)
##

