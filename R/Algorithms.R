#' Knapsack problem using dynamic algortihm
#'  
#' @param x a data.frame with two variables v and w (values and weights)
#' @param W knapsack size (Max weight capacity)
#' 
#' @return the maximum values and elements to be carried by knapsack
#' 
#' @examples knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#' @examples knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
#' 
#' @export knapsack_dynamic


knapsack_dynamic <- function(x, W){
  stopifnot(is.data.frame(x) == TRUE)
  stopifnot(is.numeric(W) == TRUE)
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

#Example dataframe

#system.time({knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)})

#run time of a chunk of code


###################################################################

#using proc.time() for n=16 it took: User:25.694. System:0.414. Elapsed:29.748
#' @export brute_force_knapsack

brute_force_knapsack <- function(x,W)
{stopifnot(is.data.frame(x),is.numeric(W))
  value <- 0
  elem <-c()
  
  for(i in 1:(2^nrow(x)-1))
  {wsum <- 0
  vsum <- 0
  loop <-c()
  binary <- intToBits(i)
  
  for(j in 1:length(binary))
  {if( binary[j] == TRUE )
  {
    wsum <- wsum + x[j,1]
    vsum <- vsum + x[j,2]
    loop <- c(loop,j)
  }
    
  }
  
  if(vsum > value && wsum <= W)
  {value<-vsum 
  elem<-loop}}
  
  return(list(value=round(unname(value)),elements=elem))
}
######################################################

#using proc.time() for n=16 it took: User:25.694. System:0.414. Elapsed:29.748

###########################################################
#' @export greedy_knapsack

greedy_knapsack <- function(X,W){
  df <- transform(X, c= v/w )
  df<- df[order(df$c,decreasing = TRUE),]
  w8  <- vector()
  j=0
  Val <- vector()
  elements <- vector()
  while(sum(w8) <= W)
  {
    w8 <-  append(w8,df$w[j])
    Val <- append(Val,df$v[j])
    j <- j+1
    
  }
  
  if(sum(w8)> W)
  { 
    w8 <- head(w8,-1)
    Val <- head(Val,-1)
  }
  Val
  elements <- append(elements,which(X$w  %in% w8))
  results <- list("value"= sum(Val),"elements"=elements,"weight"=sum(w8))
  return(results)
  
}



#greedy_knapsack(Z = knapsack_objects[1:1200,], W = 2000)


#ptm <- proc.time()
#greedy_knapsack(X = knapsack_objects[1:1000000,], W = 3500)
#proc.time() - ptm

# time
#user  system elapsed 
#1.22    0.04    1.27 