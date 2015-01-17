rankhospital <- function(state, outcome, num = "best") {
## Read outcome data
data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
options(warn=-1)
## Check that state and outcome are valid
state<-toupper(state)
statelist  <- unique(data$State)
checkState <- state %in% statelist
validOutcome <- c("heart attack", "heart failure", "pneumonia")
checkOutcome <- outcome %in% validOutcome
if(checkState==FALSE | checkOutcome==FALSE) {
	if(checkState==FALSE & checkOutcome==FALSE) {stop("invalid state and outcome")}
	if(checkState==FALSE){stop("invalid state")}
	if(checkOutcome==FALSE){stop("invalid outcome")}
} else {
    if(outcome=="heart attack") { heartA <-data[,11]} 
    else if(outcome=="heart failure") {heartA <-data[,17]} 
    else if(outcome=="pneumonia"  ) {heartA <-data[,23] }
    
	Rate <- as.numeric(heartA[data$State==state])   #only those with right state
	Hospital.Name <- data$Hospital.Name[data$State==state]	
	tab<-na.exclude(cbind(Hospital.Name, Rate))
	allRate<-as.numeric((tab[,2]))
	x<-cbind(allRate,allHosp=tab[,1])
	pxx <- as.data.frame(x, stringsAsFactors=FALSE) 
	spxx<-cbind(sort(as.numeric(pxx[,1])), unlist(tapply(pxx[,2], as.numeric(pxx[,1]),
		sort, decreasing = F)))
	spxx<-as.data.frame(spxx,row.names=1:nrow(spxx),stringsAsFactors=FALSE,col.name=c("HRank","HName"))
		tmp<-is.numeric(num)
	
		if(tmp==FALSE){
			if(num== "best") {rank.Hosp <- spxx$V2[1]}
	    	if(num=="worst") {rank.Hosp <- spxx$V2[nrow(spxx)]}
    		} else {
    		 if(num > nrow(spxx)) {
    		 rank.Hosp <- NA
    		 } else {  
    		 rank.Hosp <- spxx$V2[num]	
    		 }	
    		}
    	
} 

return(rank.Hosp) 	## Return hospital name in that state with the given rank
					## 30-day death rate

}
