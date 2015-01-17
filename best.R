best <- function(state, outcome) {
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
	if(outcome=="heart attack"){
	heartA<-as.numeric(data[,11])
	heartSt<-heartA[data$State==state]#only those with right state
	hospSt <-data$Hospital.Name[data$State==state]
	id<-which(heartSt == min(heartSt, na.rm = TRUE))
	#print(length(id))
		if(length(id) > 1) {	
		sHosp<-sort(hospSt[id])
		besthosp <- sHosp[1] 
		} else { besthosp <- hospSt[id] }
	}
	if(outcome=="heart failure"){
	heartF<-as.numeric(data[,17])
	heartSt<-heartF[data$State==state]#only those with right state
	hospSt <-data$Hospital.Name[data$State==state]
	id<-which(heartSt == min(heartSt, na.rm = TRUE))
		if(length(id) > 1) {	
		sHosp<-sort(hospSt[id])
		besthosp <- sHosp[1] 
		} else { besthosp <- hospSt[id] }
	}
	if(outcome=="pneumonia"){
	pneu <-as.numeric(data[,23])
	pneuSt<-pneu[data$State==state]#only those with right state
	hospSt <-data$Hospital.Name[data$State==state]
	id<-which(pneuSt == min(pneuSt, na.rm = TRUE))
	print(length(id))
		if(length(id) > 1) {	
		sHosp<-sort(hospSt[id])
		besthosp <- sHosp[1] 
		} else { besthosp <- hospSt[id] }

	}

}
## Return hospital name in that state with lowest 30-day death
## rate

return(besthosp)

}