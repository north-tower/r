library(grid)
library(dplyr)
library(scales)
library(ggplot2)

# original formula 
Formula <- function(x) 1.2 * (x-2)^2 + 3.2

# visualize the function, and the optimal solution
ggplot( data.frame( x = c(0, 4) ), aes(x) ) + 
  stat_function(fun = Formula) + 
  geom_point( data = data.frame( x = 2, y = Formula(2) ), aes(x, y), 
              color = "blue", size = 3 ) + 
  ggtitle( expression( 1.2 * (x-2)^2 + 3.2 ) )

# first derivative of the formula above
Derivative <- function(x) 2 * 1.2 * (x-2) 


#### Gradient Descent implementation :

## Define parameters :
# x_new : initial guess for the x value
# x_old : assign a random value to start the first iteration 
x_new <- .1 
x_old <- 0

# define the eta value (learning rate)
learning_rate <- .6

# define the epilson value, maximum iteration allowed 
epsilon <- .001
step <- 1
iteration <- 20

# records the x and y value for visualization ; add the inital guess 
xtrace <- list() ; ytrace <- list()
xtrace[[1]] <- x_new ; ytrace[[1]] <- Formula(x_new)
cbind( xtrace, ytrace )

while( abs(x_new - x_old) > epsilon & step <= iteration )
{
  # update iteration count 
  step <- step + 1    
  
  # gradient descent
  x_old <- x_new
  x_new <- x_old - learning_rate * Derivative(x_old)
  
  # record keeping 
  xtrace[[step]] <- x_new
  ytrace[[step]] <- Formula(x_new)    
}

# create the data points' dataframe
record <- data.frame( x = unlist(xtrace), y = unlist(ytrace))
record

# create the segment between each points (gradient steps)
segment <- data.frame( x = double(), y = double(), xend = double(), yend = double() )
for( i in 1:( nrow(record)-1 ) ) {
  segment[ i, ] <- cbind( record[i, ], record[i + 1, ] )
}

# visualize the gradient descent's value 
ggplot( data.frame( x = c(0, 4) ), aes(x) ) + 
  stat_function(fun = Formula) + 
  ggtitle( expression( 1.2 * (x-2)^2 + 3.2 ) ) + 
  geom_point( data = record, aes(x, y), color = "red", size = 3, alpha = .8, shape = 2 ) +
  geom_segment( data = segment , aes(x = x, y = y, xend = xend, yend = yend), 
                color = "blue", alpha = .8, arrow = arrow( length = unit(0.25, "cm") ) )

