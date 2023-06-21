Federal_stimulus_Data <- read.csv("federal_stimulus_data.csv")
df_yourname_fed_stimulus <- Federal_stimulus_Data
str(df_yourname_fed_stimulus)

#sum and mean of the payment column
sum(df_yourname_fed_stimulus$Payment.Value)
mean(df_yourname_fed_stimulus$Payment.Value)

#Subset with of project status "Completed 50% or more"  
projects_50 <- subset(df_yourname_fed_stimulus, Project.Status == "Completed 50% or more")