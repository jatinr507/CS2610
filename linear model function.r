linereg <- function(x,y){
  n <- length(x)
  data1 <- cbind(x,y)
  data1
  xbar <- mean(x)
  ybar <- mean(y)
  xc <- x-xbar
  yc <- y-ybar
  xs <- xc^2
  b1 <- sum(xc*yc)/sum(xs)
  b1
  b0 <- ybar-b1*xbar
  yhat <- b0+b1*x
  e <- y-yhat
  y <- b0+b1*x+e
  y
  plot(y~x)
  abline(a=b0,b=b1)
  b1
  b0
  print(linereg)
}

x <- c(630,370,616,700,430,568,1200,2976) #number of shopping centers in each state
y <- c(15.5,7.5,13.9,18.7,8.2,13.2,23,87.3) #retail sales in billions per state
linereg(x,y)

print(linereg)
