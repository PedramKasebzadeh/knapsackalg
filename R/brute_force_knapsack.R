#' this function uses brute force as an algorithem
#' 
#  @param  data.set 
#  @param  numeric
#' 
#' @param  x A data.frame.
#' 
#' @param W A numeric. 
#' 
#' 1
#' @return  the calculated value by this algorithem  
#' @export

brute_force_knapsack <- function(x,W)
{

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
  
  return(list(value=round(unname(value)),elements=elem))
}