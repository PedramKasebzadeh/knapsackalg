#' Knapsack problem using dynamic algortihm
#' 
#' @name "Lab 6 package"
#'  
#'  
#'  
#' @param  x A data.frame 
#' 
#' @param W A number 
#' 
#' 
#' @return the maximum values and elements to be carried by knapsack
#' 
#' @export 






knapsack_dynamic <- function(x, W){
  stopifnot(is.data.frame(x))
  stopifnot(W>0)
  n <- nrow(x)
  w <- x[[1]]  #weights
  v <- x[[2]]  #values
  capacity <- W     #capacity of knapsack
  elem <- c()
  m <- matrix(0,nrow = n+1,ncol = capacity+1)
  
  #to return the max value 
  for (i in 2:n){
    for (j in 1:capacity){
      if (j > w[i]) 
        m[i, j] <- max(m[i-1, j-w[i]] + v[i], m[i-1, j])
      else 
        m[i,j] <- m[i-1, j]
    }
  }
  
  val <- m[i, j]
  value <- round(val)
  
  #to print the elements
  while(capacity>0 && n-1>0){
    if(m[n,capacity]!=m[n-1,capacity]){
      elem<-c(elem,n)
      capacity<-capacity-w[n]
      elements <- sort(elem)
    }
    n<-n-1
  }
  return(list("value"=value,"elements"=elements))

}