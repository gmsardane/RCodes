complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
		data<-lapply(id,getmonitor,directory)
		good<-sapply(data, complete.cases)
		nobs<-sapply(good,sum) ##OR sapply(sapply(data, complete.cases),sum)
		x<-as.data.frame(list(id=id,nobs=nobs))
		assign("x",x,envir = .GlobalEnv)
		return(x)        
}

##Alternative way: CLASSIC FOR LOOP
#create array that contains results
        #x<-array(0L,dim=c(length(id),2))
        #dimnames(x)=list(1:length(id),c("id","nobs"))
        #for(i in seq_along(id)) {       	
	    #    data<-getmonitor(id[i],directory) #CAN I SAY INSTEAD: ?
	    #    good<-complete.cases(data$sulfate,data$nitrate)
	    #    x[i,1]<-id[i]
	    #    x[i,2]<-sum(good)
        #}
        #y=as.data.frame(x)
        #assign("y",y,envir = .GlobalEnv)
        #return(y)