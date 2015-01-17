mlogLLUnbin <- function( W, gW, lower){

#Please see Churchill Cahpter 3 and Schneider et al (1993)
#And Lanzetta, Turnshek and Wolfe (1987)
#And Murdoch et al ()
Wstar =0.5 
    m <- length(W)  #The observed number of data points
#Wstar <- Wstar*rep(1.0, m) 
#The normalization A is then
summary(gW)
plot(W, gW*exp(-W/Wstar))

 #There are m terms in this matrix
    #     A <- integrate(Vectorize(integrand, vectorize.args=c("W", "gW")),
    #    , lower = lower, upper =Inf,  W, gW) #this is just some scalar
    A <- integrate(integrand,lower, Inf, W=W, gW=gW, Wstar=Wstar) 
    term1 <- -m*A
    term2 <-  sum(log(gW))
    term3 <- -sum(-W/Wstar)
   mlogLL <- sum(term1 + term2 + term3)
   
   return(-mlogLL)

}