mlogLLhetero <- function(params, x, y, sigma) {
    	   n <- length(x)
          t1 <- -0.5*n*log(2*pi)*rep(1.0, n)
     	  t2 <- -0.5*n*log(sigma^2)*rep(1.0, n)
          t3 <- -((y- params[1]*rep(1.0, n) - x*params[2])^2./(2.0*sigma^2)) 
          mlogLL <-  sum(t1 + t2 + t3)



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