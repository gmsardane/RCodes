corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
        
        alldat<-complete("specdata/")
        goodid<-alldat$id[alldat$nobs > threshold]
        if(length(goodid)>0) {
        #define correlation matrix
        xcorr<-c(1,(length(goodid)))*0
        	for(i in seq_along(goodid)) {       	
	        	data<-getmonitor(goodid[i],directory)
	    		xcorr[i]<-cor(data$sulfate,data$nitrate, use='complete.obs')
	    	}
	    	
        yy=as.data.frame(xcorr)
        assign("yy",yy,envir = .GlobalEnv)
        
        	
       } else {xcorr<-array(0,dim=0)}
        return(xcorr)                
}
##Alternative:
#data<-sapply(goodid, getmonitor, directory)