mlogLLfixedSlope <- function(params, x,y, sigma){

#Constant Parameters
beta.strong <-  -7.79256020 
  beta.weak <-  -2.65130592

#-7.137515386
#-2.551285002

             n <- length(x)
          N.wk <- params[1] #exp(intercept)
         N.str <- params[2] #slope

            t1 <- -0.5*n*log(2*pi)*rep(1.0, n)
    	    t2 <- -0.5*n*log(sigma^2)*rep(1.0, n)
         model <- log(N.wk*exp(beta.weak*x) + N.str*exp(beta.strong*x))
            t3 <- -((y- model)^2./(2.0*sigma^2)) 
        mlogLL <- sum(t1 + t2 + t3)

return(-mlogLL)

}