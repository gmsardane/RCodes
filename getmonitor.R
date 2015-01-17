getmonitor <- function(id, directory, summarize = FALSE) {
        ## 'id' is a vector of length 1 indicating the monitor ID
        ## number. The user can specify 'id' as either an integer, a
        ## character, or a numeric.
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'summarize' is a logical indicating whether a summary of
        ## the data should be printed to the console; the default is
        ## FALSE
        
        ## Your code here
        ##Establish full path to datafile
    		id<-strtoi(id, base = 0L)
    		if(id>=1 & id<=332) {
	        id<-sprintf("%03s", id) 
         	fname<-paste(directory,"/",id,".csv",sep="",collapse="")
          	dframe<-read.csv(file=fname,head=TRUE,sep=",")
    		assign("dframe",dframe,envir = .GlobalEnv)
    		if(summarize) {print(summary(dframe))}        	
        } else {        
        print("ID must be an integer between 1 and 332")
        print("Exiting")
        }
              
return(dframe)	   
}

