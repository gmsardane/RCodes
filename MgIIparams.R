MgIIparams <- function(data, Nstar, Wstar) {

data <- read.table(data, header=T)



fit.dist.MgII <- optim(par=c(Nstar, Wstar), fn=mlogLLUnbin, method="L-BFGS-B",hessian=T
                  , W=data$W2796, lower=c(0,0), upper=c(5,5))

return(fit.dist.MgII)

}