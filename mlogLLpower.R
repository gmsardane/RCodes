mlogLLpower <- function(params, x,y) {
    #x <- cbind(1,x) # make into a matix on n rows and 2-columns
    n <- length(x)
  brk <- params[1]
alpha <- params[2]	
 beta <- params[3] #params[1] = breakpt ; params[2] = alpha ; params[3] = beta
    A <- params[4]
    B <- params[5]
    b <- params[6]
sigma <- params[7]
#model <- ifelse(x<=brk, alpha*x^beta,(alpha*brk^beta)-b*(x-brk)) #Power Law and Line
const <- (alpha*brk^beta + A - B)/brk^b
#model <- ifelse(x<=brk, alpha*x^beta,(alpha*brk^(beta-b)*x^b))  #Double Power law
model <- ifelse(x<=brk, alpha*x^beta+A,const*x^b + B)  #Double Power law
    e <- y-model
   mlogLL <- -0.5*n*log(sigma) -0.5*n*log(2*pi) - (t(e)%*%e)/(2*sigma^2) #the t(e) means transpose of e
 #print(params)
 #print(-mlogLL)   
     
return(-mlogLL)

}


#mlogLL <- function(params, x, y) {
#    x <- cbind(1,x) # make into a matix on n rows and 2-columns
#    n <- nrow(x)
#	k <- ncol(x)
# beta <- params[1:k] #params[1] = b ; params[2] = m ; params[3] = sigma
#sigma <- params[k+1]
#     
      # %*% is MATRIx Multiplication. So at each row of x we get: 
      # [1,xi]trans([b,m]) = b + mxi or n rows and 1 col
#return(-mlogLL)  
