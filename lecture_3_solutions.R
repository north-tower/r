# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 1: Compute the quarterly growth rate (in log points) of the US economy and store it
# in the "data" dataframe as the variable "growth".
data$growth <- c(NA,diff(log(data$GDPC1)))
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 2: Plot growth rate (y axis) over time (x axis) as a blue bar for
# positive quarter and red for negative quarters. Add a reference line for the 
# 0 growth rate. Hint: you may use either plot or barplot, and ifelse() to create
# a vector of colors.
# Strategy 1:
plot(data[,c("time","growth")], type="h", lwd=2, col=ifelse(data$growth>0,"blue","red"))
abline(h=0)
# Strategy 2:  
barplot(data$growth, col=ifelse(data$growth>0,"blue","red"))
abline(h=0)
# axis(1) # you will have to create a custom x-axis
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 3: Add a transparent grey shaded area ove rthe previous the figure of
# the previous exercise over quarters with negative growth.
# Strategy 1:
plot(data[,c("time","growth")], type="h", lwd=2, col=ifelse(data$growth>0,"blue","red"))
starts <- data$time[data$growth<0]
step <- diff(data$time)[1]/2
abline(h=0)
rect(xleft=starts-step, xright=starts+step, ybottom=-1, ytop=1, col=adjustcolor("black", alpha.f=0.2), border=NA)
# Strategy 1:
barplot(data$growth, col=ifelse(data$growth>0,"blue","red"), space=0)
abline(h=0)
axis(1) # notice the support of the x-axis starts from 0. These are positions, not years
starts <- which(data$growth<0) - 1# position, starting at the 0 mark
step <- 1
rect(xleft=starts, xright=starts+step, ybottom=-1, ytop=1, col=adjustcolor("black", alpha.f=0.2), border=NA)
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 4: Create a series of figures that make the planet rotate a bit. Write 
# these figures to the disk. Hint: 1- write a loop where the central parameter
# runs from 200 to 300.
rotate.map <- function(angles, write=F) {
  for (i in angles) {
    print(i)
    number <- substr(match(i,rev(angles))+1000,2,5) # this ensures the planet rotates in the right direction
    if (write) png(paste("map_",number,".png",sep=""),600,600) 
    par(mar=c(0,0,0,0))
    o <- c(15,i,0) 
    map("world", proj="orthographic",orientation=o) 
    map("county",proj="orthographic",orientation=o, col = colors[colorsmatched], fill = TRUE, resolution = 0, lty = 0, add=T)
    map("state",proj="orthographic",orientation=o, add=T, lwd=.5) 
    if (write) dev.off()
  }
}
rotate.map(200:300)
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Exercise 5: Compute anual growth rate for each state in 1 line of code
growth <- t(apply(data[,-1],1, function(x) diff(log(x)) ))
colnames(growth) <- 1998:2017
rownames(growth) <- data[,1]
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =