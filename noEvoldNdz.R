noEvoldNdz <- function(N, z) {

    num = N*(1.+z)^2
   dnom = sqrt(0.3*(1.+z)^3.+0.7)
   dNdz = num/dnom

return(dNdz)

}

