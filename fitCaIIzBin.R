fitCaIIzBin <-  function(fname) {

data <- read.table(fname, header=T) #CORRECT NO-BINNING
#Now fit those greater than the break as strong components with a line.
REWA.strong <- as.double(  data$W3934[data$W3934 >= brk])
dNdW.strong <- as.double(data$logdNdW[data$W3934 >= brk])
 err.strong <- as.double(data$dNdWerr[data$W3934 >= brk])
#c(N.weak, N.strong)
start.params <- as.double(c( 0.059015740, -0.002052981))

fit <- optim(start.params, mlogLLfixedSlope, method="BFGS",hessian=T, 
                     x=data$W3934, y=data$logdNdW , sigma=data$dNdWerr) 
 
 OI <- solve(fit$hessian)
sse <- sqrt(diag(OI))

print(list("Here are the results for: ", fname))

return(c(fit,sse))

}
