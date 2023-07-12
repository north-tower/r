gamecounts<-function(n){
  games <- NULL
  
  N<-n
  
  for (t in 1:N){
    comeout <- sample(1:6, 1) + sample(1:6, 1)
    
    if(comeout %in% c(2,3,12)){
      result <- F
      
    } else if( comeout %in% c(7,11)){
      result <- T
    } else {
      roll <- sample(1:6,1) + sample(1:6,1)
      
      while (!roll %in% c(7, comeout)) {
        roll <- sample(1:6,1) + sample(1:6,1)
      }
      if(roll == 7)
        result = F
      else
        result = T
    }
    games[t] <- result
  }
}



read_data <-  function(filename){
  read.csv(file = filename, stringsAsFactors = TRUE)
}

basket_with <- function(df, itemname) {
  basket_with1 <- subset(df, item_name == itemname)
  basket_with1$txn_id
}

basket_common <- function(df, item1, item2){
  x <- subset(df, item_name == item1)
  y <- subset(df, item_name == item2)
  intersect(x$txn_id,y$txn_id)
}

confidence <- function(df, itemA, itemB){
  x <- subset(df, item_name == itemA)
  y <- subset(df, item_name == itemB)
  n <- (intersect(x$txn_id, y$txn_id))
  s <- length(n)/nrow(carSpeeds)
  d <- nrow(x)/nrow(df)
  s/d
}

lift <- function(df, itemA, itemB) {
  a <- subset(df, item_name == itemA)
  b <- subset(df, item_name == itemB)
  n <- (intersect(a$txn_id, b$txn_id))
  a.b <- length(n)/nrow(df)
  x <- nrow(a)/nrow(carSpeeds)
  y <- nrow(b)/nrow(carSpeeds)
  a.b/(x*y)
}

plot_trees <- function(){
  par(mfrow=c(1,3))
  plot(trees$Girth,trees$Height,xlab = "Girth", ylab = "height")
  plot(trees$Volume,trees$Height,xlab = "Volume", ylab = "height")
  plot(trees$Volume, trees$Girth,xlab = "volume", ylab = "Girth")
}

predict_volume <- function(g,h){
  fit_2 <- lm(Volume ~ Girth + Height, data = trees)
  predict(fit_2, data.frame(Girth = g, Height=h))
}