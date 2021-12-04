w <- c(2.5,-5.0, -1.2, 0.5, 2.0, 0.7)
b <- 0.1
x <- c(3,2,1,3,0, 4.19)

Z <- w%*%x + b

sigmoid <- function(Z, y){
  if(y==0){
    return(1 - (1/(1+exp(-Z))))
  }else{
    return(1/(1+exp(-Z)))
  }
}


loss <- function(sigmoid_res,y){
  return(-(y*log(sigmoid_res) + (1-y)*log(1-sigmoid_res)))
}
sigmoid_res <- sigmoid(Z,y=1)

my_loss <- loss(sigmoid_res,y=1)

my_loss


sigmoid_res2 <- sigmoid(Z, y=0)
my_loss2 <- loss(sigmoid_res, y=0)
my_loss2