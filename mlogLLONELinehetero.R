mlogLLONELinehetero <- function(params, x,y, sigma, strparams ) {

      n <- length(x)
 #    brk <- 0.91513801ß
  

#interp2 <- as.double((plines[2]-plines[4])*plines[1]+plines[3])

#Line at x<= break

 beta.str <-     strparams[2] #strong slope 
alpha.str <- exp(strparams[1]) #strong intercept
#2nd LINE
 alpha.wk <- params[1] #weak intercept
  beta.wk <- params[2] #weak slope
          t1 <- -0.5*n*log(2*pi)*rep(1.0, n)
     	  t2 <- -0.5*n*log(sigma^2)*rep(1.0, n)
       model <- log(alpha.wk*exp(beta.wk*x) + alpha.str*exp(beta.str*x))
          t3 <- -((y- model)^2./(2.0*sigma^2)) 
      mlogLL <- sum(t1 + t2 + t3)
return(-mlogLL)
#return(model)
}

