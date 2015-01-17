mlogLL <- function(params, x, y) {
    x <- cbind(1,x) # make into a matix on n rows and 2-columns
    n <- nrow(x)
	k <- ncol(x)
 beta <- params[1:k] #params[1] = b ; params[2] = m ; params[3] = sigma
sigma <- params[k+1]
     e<- y-x%*%beta 
      # %*% is MATRIx Multiplication. So at each row of x we get: 
      # [1,xi]trans([b,m]) = b + mxi or n rows and 1 col
 mlogLL <- -0.5*n*log(2*pi)-0.5*n*log(sigma^2)-((t(e)%*%e)/(2*sigma^2)) #the t(e) means transpose of e
return(-mlogLL)  
  
  


}


#   N <- length(x)
#   b <- as.double(params[0]) #int
#   m <- as.double(params[1]) #Inv Wstar   
#sigx  = as.double(params[2])
#   A  = as.double(sqrt(2.0)*pi )
#   #This is already a log of the likelihood I conmputed by hand
#   mLL = -0.5*N*log(2.0*pi*sigx^2)-sum((y-b-m*x)^2)/(2.0*sigx^2)
#  return(-mLL)

#to get erros:
#OI <- solve(p$hessian)
#sse <- se<-sqrt(diag(OI))