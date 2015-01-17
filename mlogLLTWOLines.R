mlogLLTWOLines <- function(params, x,y) {
    n <- length(x)
#  brk = 0.91513801
#Line at x<= break
    alpha.weak <- params[1] #exp(intercept)
     beta.weak <- params[2] #slope
#WEAK component
    alpha.str  <- params[3] #exp(intercept2)
     beta.str  <- params[4] #slope2
        sigma  <- params[5]
#const  = (m-m2)*brk+ b
# x <= brk :   mx + b
# x  > brk : m2*x + b2 ; b2 = (m-m2)*brk+ b
#model <- ifelse(x<=brk, m*x+b, m2*x+const) #Power Law and Line
 model <- log(alpha.weak*exp(beta.weak*x) + alpha.str*exp(beta.str*x))
     e <- y-model
mlogLL <- -0.5*n*log(sigma) - 0.5*n*log(2*pi) - (t(e)%*%e)/(2*sigma^2) #the t(e) means transpose of e
return(-mlogLL)
#return(model)
}

