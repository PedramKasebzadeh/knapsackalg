#' this function uses brute force as an algorithem
#' 
#' 
#' @param  x A data.frame.
#' 
#' @param W A numeric. 
#' 
#' 
#' @return  the calculated value by this algorithem 
#'  
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

library(parallel)
par <- function(x,W)
{
  last_value <- 0
  last_vector <-c()
  rows<-(2^nrow(x)-1) * 32
  
  #first int function, it receives all combinations (7x32)
  step1 <- function(rows)
  {
    w_sum <- 0
    v_sum <- 0
    last_p <-0
    i <- 1:(2^nrow(x)-1)
    j <- 1:32
    #matrix repeats 32 (7 times), repeat 7 (32 times)
    mat_ij <- matrix(c(rep ( j , (2^nrow(x)-1) ),rep (i,32)),ncol=2)
    binary <- intToBits(mat_ij[rows,2])
    #for 1 (1), for 2 (2), for 3 (1,2) for 4 (3), for 5(1,3)...
    if (binary[mat_ij[rows,1]] == TRUE)
    {
      w_sum <- w_sum + unname(x[mat_ij[rows,1],1])
      v_sum <- v_sum + unname(x[mat_ij[rows,1],2])
      last_p <- mat_ij[rows,1]
    }
    list(w=w_sum,v=v_sum,p=last_p)
  }
  #doesnt work on windows stackoverflow.com/questions/17196261/understanding-the-differences-between-mclapply-and-parlapply-in-r
  all_posibilities <-mclapply(1:rows,step1, mc.cores = 2)
  resvec <- unlist(all_posibilities) 
  
  
  
  step2 <- function(i)
  {
    start <- 96*(i-1)+1
    end <- 96*i
    temp1 <- resvec[start:end]
    weightsum <- sum(temp1[which(names(temp1) == "w")])
    weightsum <- unname(weightsum)
    valuesum <- sum(temp1[which(names(temp1) == "v")])
    valuesum <-unname(valuesum)
    packetschosen <- temp1[which(names(temp1) == "p")]
    packetschosen <- packetschosen[-which(packetschosen == 0)]
    packetschosen <-unname(packetschosen)
    list(w=weightsum,v=valuesum,p=packetschosen)
  }
  #only considering valid options.
  list_valid_weight <-mclapply(1:(rows * 3 / 96),step2, mc.cores = 2)
  
  
  lengthy <-length(list_valid_weight)
  
  step3 <- function(number){
    if(list_valid_weight[[number]]$w > W){
      return(list(v=0,p=c(0,0)))
    }else{
      return(list(v=unname(list_valid_weight[[number]]$v),p=unname(list_valid_weight[[number]]$p)))
    }
  }
  
  step3 <-mclapply(1:lengthy,step3, mc.cores = 2)
  
  values <- unlist(step3)
  values <- values[which(names(values)=="v")]
  last_value <- max(values)
  last_vector <- max.col(matrix(values,nrow=1)) 
  list(value=round(unname(last_value)),elements = step3[[last_vector]]$p)
}
