unemploy_rate <- c(7.9,7.7,7.5,7.5,7.5,7.5,7.3,7.2,7.2,7.2,7.0,6.7)
month <- c("Jan","Feb","Mar","Apr","May","Jun","July","Aug","Sep","Oct","Nov","Dec")
month <- as.factor(month)
monthly_rate <- data.frame(month,unemploy_rate)

monthly_rate_March <- subset(monthly_rate,month=="Mar")

unemploy_rate_7.5 <- subset(monthly_rate, unemploy_rate <= 7.5)