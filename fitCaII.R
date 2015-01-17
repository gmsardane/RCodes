#R-code for optimizing my data this case is for the 3934 Line 1st

fitCaII <- function(level, brk){
# CaII3934525sigSummary
# data <- read.table('goodtofitALLW0s.txt', header=T)
#New set:
#data <- read.table('RefitNewBins.txt', header=T) 
# data <- read.table('goodtofitALLW0sV2noNANs.txt', header=T) #INCORRECT BINNING 
# data <- read.table('goodtofitALLW0s.txt', header=T) #CORRECT BINNING
# data <- read.table('Binzabs02_20B.txt', header=T) #CORRECT BINNING
# data <- read.table('Binzabs20_45B.txt', header=T) #CORRECT BINNING
# data <- read.table('Binzabs45_75B.txt', header=T) #CORRECT BINNING
#data <- read.table('Binzabs75_134a.txt', header=T) #CORRECT BINNING
#data <- read.table('NoBinningCaII.txt', header=T) #CORRECT NO-BINNING
data <- read.table('test.txt', header=T) 
#Now fit those greater than the break as strong components with a line.
REWA.strong <- as.double(  data$W3934[data$W3934 >= brk])
dNdW.strong <- as.double(data$logdNdW[data$W3934 >= brk])
 err.strong <- as.double(data$dNdWerr[data$W3934 >= brk])
#params[1] = b ; params[2] = m ; params[3] = sigma
start.params.strong <- as.double(c( -1, -1))
#By minimizing the log-Likelihood
fit.strong <- optim(start.params.strong, mlogLLhetero, method="BFGS",hessian=T, 
                     x=REWA.strong, y=dNdW.strong, sigma=err.strong)
print(data)
print("USING OPTIM")
print((fit.strong))
#Calculate the standard errors:
 OI <- solve(fit.strong$hessian)
sse <- sqrt(diag(OI))
print("SSE STRONG")
print(sse)
#Now write the confidence intervals:
conf.level <- level
#crit  <- qt(1 - (1 - conf.level)/2/length(fit.strong$par), length(REWA.strong)-2) #with Bonferroni correction
crit <- qnorm(1 - (1 - conf.level)/2/length(fit.strong$par)) #with Bonferroni correction

#Confidence Interval for the 
names = list(rbind("2.5%", "97.5%"), rbind("b", "m"))
CI.intercept.strong <- fit.strong$par[1] + c(-1, 1) * crit * sqrt(sse[1])
    CI.slope.strong <- fit.strong$par[2] + c(-1, 1) * crit * sqrt(sse[2])
    CI.strong  <- matrix(nrow=2, ncol=2, dimnames=names)
CI.strong[1, ] <- CI.intercept.strong
CI.strong[2, ] <- CI.slope.strong
start=c(b=start.params.strong[1], m=start.params.strong[2])
fit.nls <- nls(dNdW.strong ~ b + m*REWA.strong, start=start)
fit.params <- fit.nls$m$getPars()
fit.lmodel2 <- lmodel2(dNdW.strong ~ REWA.strong)

#print(confint(fit.nls, "b", level=0.95))
#ALL these methods/algorithms give the same results to the 5th decimal place
#Now create an ellipse for the parameters:
#plot(REWA.strong,dNdW.strong, pch=20)
#lines(REWA.strong,fitted(fit.nls))
#plot(ellipse(fit.nls, which = c('m','b')), type = 'l')
#points(fit.params['m'],fit.params['b'])
#Now plotting the part including the weaker components
start.params.weak <- as.double(c(0.5917956, -6.6693375))
#By minimizing the log-Likelihood


#fit.strong.weak <- optim(start.params.weak, mlogLLONELinehetero, method="Nelder-Mead",hessian=T
#                  , x=data$W3934, y=data$logdNdW, sigma=data$dNdWerr, strparams=fit.strong$par)
#
#OI <- solve(fit.strong.weak$hessian)
#sse <- sqrt(diag(OI))
#print("USING OPTIM FOR WEAK & STRONG USING INFo FROM STRONG")
#print(fit.strong.weak)
#print("SSE WEAK+STRONG")
#print(sse)
 
start.params.ALL <- c(alpha.wk=1.152409, beta.wk=-9.734246, alpha.str=exp(-4), beta.str=-3.)
fit.ALL <- optim(start.params.ALL, mlogLLTWOLineshetero, method="BFGS",hessian=T, 
                     x=data$W3934, y=data$logdNdW , sigma=data$dNdWerr) 
 OI <- solve(fit.ALL$hessian)
sse <- sqrt(diag(OI))
print("USING OPTIM FOR WEAK & STRONG USING NO INFo FROM STRONG")
print(fit.ALL$par)
print("SSE ALL")
print(sse)
print("minus Log-Lik")
print(fit.ALL$val) 
print("Therefore the goodness of FIT parameters are : ")

BayeIC <- -2*fit.ALL$val + 4*log(24)
AikaIC <- 2*4 - 2*log(fit.ALL$val)
print(list("TWO POWER FIT BIC=", BayeIC))

print(list("TWO POWER FIT AIC=", AikaIC))




fit.ONELine.nls <- nls(data$logdNdW ~ beta.one+alpha.one*data$W3934, data=data, start=list(beta.one=-1.0, alpha.one=-3.0))
print(list("ONE POWER FIT BIC=", BIC(fit.ONELine.nls)))
print(list("ONE POWER FIT AIC=", AIC(fit.ONELine.nls)))
print(summary(fit.ONELine.nls ))
fit.ALL.nls = nls(data$logdNdW ~ log(alpha.wk*exp(beta.wk*data$W3934)+alpha.str*exp(beta.str*data$W3934)),
                   data=data, start=fit.ALL$par)


print(summary(fit.ALL.nls))
print(logLik(fit.ONELine.nls))

print(list("TWO POWER FIT BIC=", BIC(fit.ALL.nls)))

print(list("TWO POWER FIT AIC=", AIC(fit.ALL.nls)))



#TRY this:
#start.params.ALL.nls2 <- list(N0=0.1, E0=1.0, gamma1=-3, gamma2=-1.5, Eb=1.0, beta=1)
#fit.ALL.nls2 = nls(data$logdNdW ~ log(N0)+gamma1*log((data$W3934/E0))-beta*log((1+(data$W3934/Eb)^((gamma1-gamma2)/beta))) ,
#                   data=data, start=start.params.ALL.nls2)
#N0=0.1
#E0=1.0
#gamma1=-3
#gamma2=-1.5
#Eb=1.0
#bet=1.5
#model <- log(N0)+gamma1*log((data$W3934/E0))-bet*log((1+(data$W3934/Eb)^((gamma1-gamma2)/bet)))
#print(fit.ALL.nls2)
return(c(fit.strong$value, fit.strong.weak$value))
} 

#Foer Wmin >= 0.2 AA
z = c(0.21058041, 0.46345967, 0.69224864, 0.96418571)
y = c(0.0543571, 0.0571236,0.0679276,0.0977141)
# x = c( 0.53154624  ,    0.75703675 ,     0.94217515   ,    1.1340301   ,    1.3287580    ,   1.5451591   ,    1.8680413)
# y = c( 0.162492    ,    0.214686   ,     0.244189     ,     0.285718   ,    0.306866 ,    0.324406  ,   0.286993)
# source('noEvoldNdz.R')
# z = c( 0.53154624  ,    0.75703675 ,     0.94217515   ,    1.1340301   ,    1.3287580    ,   1.5451591   ,    1.8680413)
# list(z, y)
# summary(nls(y~N*(1.+z)^(2)/sqrt(0.27*(1.+z)^3.+0.73), data = list(z,y)))
#To run:
#library(lmodel2)
#source('fitCaII.R')