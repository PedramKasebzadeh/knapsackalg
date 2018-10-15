#' this function uses greedy algorithem
#' 
#' @param  x A data.frame. 
#' 
#' @param W A numeric .
#' 
#' @return it returns the calculated value and elements
#' 
#' @export


greedy_knapsack <- function(x,W){

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
  return(results)
  
}
