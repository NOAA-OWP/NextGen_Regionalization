#function to compute geometric mean, weighted geometric mean and weighted mean

gm_mean <- function(x, na.rm=TRUE, zero.propagate = FALSE){
  if(any(x < 0, na.rm = TRUE)){
    return(NaN)
  }
  if(zero.propagate){
    if(any(x == 0, na.rm = TRUE)){
      return(0)
    }
    exp(mean(log(x), na.rm = na.rm))
  } else { 
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
}

gm_mean_weighted <- function(x, w=NULL,na.rm=TRUE){

  if (is.null(w)) w <- rep(1/length(x),length(x)) 
  w <- w[x>0]
  exp(sum(w*log(x[x > 0]), na.rm=na.rm)/sum(w))
}

mean_weighted <- function(x, w=NULL,na.rm=TRUE) {
  if (is.null(w)) w <- rep(1/length(x),length(x))
  sum(w*x,na.rm=na.rm)/sum(w)
}
