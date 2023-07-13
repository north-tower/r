dat2 <- read_csv("hw10_p2.csv")

#1
a3 <- aov(Weight ~ Day * Group, data=dat2)
summary(a3)

#2
library(ggeffects)
a3 <- aov(Weight ~ Day * Group + Day * id, dat2) 
ggpredict(a3, "Group")

#3
ggpredict(a3, "Day")

#4
a4 <- aov(Weight ~ Day * Group + Day * id + Error(Group/(Day*id)), dat2) 
summary(a4)

#5
library(ggpubr)
library(rstatix)
res <- anova_test(data = dat2, dv = Weight, wid = id, within = Day)
res

#Extra Credit

boxplot(Weight~ Day* Group,data=dat2)
