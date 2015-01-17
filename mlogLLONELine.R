mlogLLONELine <- function(params, x,y) {

      n <- length(x)
 #    brk <- 0.91513801ß
 #plines <- c(0.91513801, -4.5450431, -3.2137706, -2.9572433)
 #plines <- c(0.91513801, -5.5439760, -2.2433128,  0.3268616) #Purely linear and including everything
 #plines <- c(1.0, -5.9226603, -2.2815094,  0.3561301) #this is the best-fit line for the strong component 3934 #WRONG BINNING
  plines <- c(  -4.8991984, -2.6828542 , 0.3529932)

#interp2 <- as.double((plines[2]-plines[4])*plines[1]+plines[3])

#Line at x<= break

 beta.str <-    plines[2] #strong slope 
alpha.str <- exp(plines[1]) #strong intercept
#2nd LINE
 alpha.wk <- params[1] #weak intercept
  beta.wk <- params[2] #weak slope
    sigma <- params[3]
    model <- log(alpha.wk*exp(beta.wk*x) + alpha.str*exp(beta.str*x))
        e <- y-model
   mlogLL <- -0.5*n*log(sigma) -0.5*n*log(2*pi) - (t(e)%*%e)/(2*sigma^2) #the t(e) means transpose of e
return(-mlogLL)
#return(model)
}

