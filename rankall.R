rankall <- function(outcome, num = "best") {
## Read outcome data
data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
options(warn=-1)
## Check that state and outcome are valid
statelist  <- sort(unique(data$State))
validOutcome <- c("heart attack", "heart failure", "pneumonia")
checkOutcome <- outcome %in% validOutcome

if(checkOutcome==FALSE){stop("invalid outcome")
} else {

	if(outcome=="heart attack") { heartA <-data[,11]} 
    else if(outcome=="heart failure") {heartA <-data[,17]} 
    else if(outcome=="pneumonia"  ) {heartA <-data[,23] }
    
    rank.Hosp <-array(0L,dim=c(length(statelist),2))
    for(i in seq_along(statelist)) {
    rank.Hosp[i,2] <- statelist[i]
    Rate <- as.numeric(heartA[data$State==statelist[i]])  
	Hospital.Name <- data$Hospital.Name[data$State==statelist[i]]	
	tab<-na.exclude(cbind(Hospital.Name, Rate))
	allRate<-as.numeric((tab[,2]))
	x<-cbind(allRate,allHosp=tab[,1])
	pxx <- as.data.frame(x, stringsAsFactors=FALSE) 
	spxx<-cbind(sort(as.numeric(pxx[,1])), unlist(tapply(pxx[,2], as.numeric(pxx[,1]),
		sort, decreasing = F)))
	spxx<-as.data.frame(spxx,row.names=1:nrow(spxx),stringsAsFactors=FALSE,col.name=c("HRank","HName"))
		tmp<-is.numeric(num)
		if(tmp==FALSE){
			if(num== "best") {rank.Hosp[i,1] <- spxx$V2[1]}
	    	if(num=="worst") {rank.Hosp[i,1] <- spxx$V2[nrow(spxx)]}
    					} else {
    		 if(num > nrow(spxx)) {rank.Hosp[i,1] <- NA} 
    		 else { rank.Hosp[i,1] <- spxx$V2[num]}	
    							} #if tmp==numeric
		

									} #for loop
			} #if all outcomes valid

xx<-as.data.frame(list(hospital=rank.Hosp[,1],state=rank.Hosp[,2]),row.names=statelist)
assign("xx",xx,envir = .GlobalEnv)
return(xx)        

}
