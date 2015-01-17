mlogLLpowerline <- function(params, x,y) {
    n <- length(x)
  brk = 0.8218175 #<- params[1]
#Line at x<= break
    m <- params[1] #slope
    b <- params[2] #intercept
#Power-LAW    
alpha <- params[3] #coefficient
 beta <- params[4] #power
sigma <- params[5]
const <- brk*(m-alpha*brk^(beta-1))+b
# x <= brk : mx + b
# x  > brk : alpha*x^beta + const
model <- ifelse(x<=brk, m*x+b, alpha*x^beta+const) #Power Law and Line
    e <- y-model
   mlogLL <- -0.5*n*log(sigma) -0.5*n*log(2*pi) - (t(e)%*%e)/(2*sigma^2) #the t(e) means transpose of e
return(-mlogLL)
#return(model)
}

