library(tidyverse)
driver<-read.csv(file = "DATASET.csv")


group_by(driver) %>%
  summarise(
    count = n(),
    mean = mean(full_licences_male, na.rm = TRUE),
    sd = sd(full_licences_male, na.rm = TRUE)
  )
group_by(driver) %>%
  summarise(
    count = n(),
    mean = mean(full_licences_female, na.rm = TRUE),
    sd = sd(full_licences_female, na.rm = TRUE)
  )
ggplot(driver,aes(x=as.numeric(age),y=full_licences_male))+geom_boxplot(fill='#A4A4A4', color="darkred",outlier.colour = "blue")+scale_y_continuous(limit=c(10000,400000),breaks=seq(0,400000,100000),expand=c(0,0))+scale_x_continuous(limits = c(10,100),breaks = seq(10,100,20),expand = c(0,0))+xlab("AGE")+ylab("Licence Pass no")+ggtitle("Male Driving Licence")


ggplot(driver,aes(x=as.numeric(age),y=full_licences_female))+geom_boxplot(fill='#A4A4A4', color="darkred",outlier.colour = "blue")+scale_y_continuous(limit=c(10000,400000),breaks=seq(0,400000,100000),expand=c(0,0))+scale_x_continuous(limits = c(10,100),breaks = seq(10,100,20),expand = c(0,0))+xlab("AGE")+ylab("Licence Pass no")+ggtitle("Female Driving Licence")

hist(driver$age,xlab="Age",ylab="Frequency",main= "Driving Test Pass rate at different age",col = "green",border = "blue", xlim = c(20,80), ylim = c(0,40))

h <- hist(driver$age,xlab="Age",ylab="Frequency",main= "Normal Distribution Curve",col = "green",border = "blue", xlim = c(20,80), ylim = c(0,70))