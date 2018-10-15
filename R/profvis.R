library(profvis)
set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)
# greedy_knapsack
profvis({
  x = knapsack_objects[1:1200,]
  W = 3500
  stopifnot(is.data.frame(x))
  stopifnot(W>0)
  v <- 0
  w <- 0
  df <- transform(x, c= v/w )
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
  elements <- append(elements,which(x$w  %in% w8))
  results <- list("value"= sum(Val),"elements"=elements)
})

# brute_force_knapsack
profvis({
  
  x = knapsack_objects[1:8,]
  W = -3500
stopifnot(is.data.frame(x))
stopifnot(W>0)
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

})

#knapsack_dynamic
profvis({
  x = knapsack_objects[1:12,]
  W = 2000
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

}
)