
lab2 <- function(theta,y,X,Z){
  
  N<-nrow(X) # number of observations
  k<-ncol(X) # number of parameters X
  q<-ncol(Z) # number of parameters z
  
  
  # Supstring paramters theta
  beta<-theta[1:k] 
  gamma<-theta[(k+1):(k+q)]
  
  # Ensure gamma to be positive
  sigma2i <- exp(Z%*%gamma)#multiply z with gama
  
  # Calculate errors
  yihat <- X%*%beta
  
  # Write down Liklihood from formula
  logLi <- log(dnorm(y,yihat,sigma2i))
  logl <-  sum(logLi)
  
  # Return Liklihood
  return(logl)
}
