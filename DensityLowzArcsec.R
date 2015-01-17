AbsMg <- function (apparentmag, dL) {

 y <- apparentMag - 5*log10(dL) + 5


return(y)
}

AppMag <- function (AbsoluteMag, dL) {
 y <- AbsoluteMag + 5*log10(dL) - 5
return(y)
}


Neighbors <- function(fname){

rbandLIM <- 22.0 #APPArent mag
AbsrbandLIM <- -17.21 # At z = 0.15
#LESS BINS
zbins <-   c(0.015, 0.0450 , 0.080,  0.11000, 0.1200,0.140, 0.150 )
dLzbins <- c(64.09, 196.73 , 358.79, 503.77, 553.32,  654.18, 705.49) * 1.0e6
radius <-  as.double(c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120 ))



data <- read.table(fname, header=T) 

#Distance <- as.double(data$DistanceArcsec)
#    rMag <- as.double(data$modelMag_r)

Distance <- as.double(data$distarcsec)
    rMag <- as.double(data$magr)
	zabs <- as.double(data$zabs)
 
#BIN 1: 

for(i in 6:length(zbins) -1 ) {
#1st WHERE : zabs cut
    print(zbins[i])
	wzabs <- zabs[(zabs >= zbins[i]) & (zabs <= zbins[i+1]) ]
	wdistance <- Distance[(zabs >= zbins[i]) & (zabs <= zbins[i+1]) ]
	wrmag <- rMag[(zabs >= zbins[i]) & (zabs <= zbins[i+1]) ]
	
#2nd where : Magnitude CUT   
   limAppMagrband <- AppMag(AbsrbandLIM, dLzbins[i+1])  
   disti <- wdistance[wrmag <= limAppMagrband]
   
   rmagi <- wrmag[wrmag <= limAppMagrband]
   count <- radius*0.0
   ecount <- count
   
   	for(j in 1:length(radius)) {
#3rd WHERE : i.e. the radius cut
  		cnt <- length(disti[disti <= radius[j]])
  		count[j] <- cnt * 1.0 / (pi* radius[j]^2)
	    ecount[j] <- sqrt(cnt*1.0) /(pi * radius[j]^2)
	}	
		    print(min(log10(count)))
#print(list("i=", i, "count = ", cnt))
start.params.ALL.nls <- list(  m=-1, b=min(count))
lower=c(1e-4,0.,0)
upper=c(1,1e5,1)
#fit.ALL.nls = nls(count ~ m*radius  + b ,  start=start.params.ALL.nls, weights=ecount)
fit.ALL.nls = nls(count ~ cbind(1, exp(- radius/abs(a1))), start = list(a1 = 500.), weights=ecount,alg = "plinear")
print(summary(fit.ALL.nls))
print("Loglik")
print(logLik(fit.ALL.nls))
print("AIC")
print(AIC(fit.ALL.nls))
}

return(length(count))

}