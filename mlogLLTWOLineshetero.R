mlogLLTWOLineshetero <- function(params, x,y, sigma) {
             n <- length(x)
      alpha.wk <- params[1] #exp(intercept)
       beta.wk <- params[2] #slope
#WEAK component
    alpha.str  <- params[3] #exp(intercept2)
     beta.str  <- params[4] #slope2
            t1 <- -0.5*n*log(2*pi)*rep(1.0, n)
    	    t2 <- -0.5*n*log(sigma^2)*rep(1.0, n)
         model <- log(alpha.wk*exp(beta.wk*x) + alpha.str*exp(beta.str*x))
            t3 <- -((y- model)^2./(2.0*sigma^2)) 
        mlogLL <- sum(t1 + t2 + t3)
return(-mlogLL)
#return(model)
}

