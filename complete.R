complete <- function(directory, id = 1:332) {
        #Initialize 
        nobs <- NULL;
        
        #Iterate over id
        for(i in id) {
                
                #Name file
                if(i < 10) {
                        filename <- paste(directory, "/00", as.character(i),".csv", sep = "")
                } else if(i >= 10 && i < 100) {
                        filename <- paste(directory,"/0", as.character(i),".csv", sep = "")
                } else if(i>=100) {
                        filename <- paste(directory,"/", as.character(i),".csv", sep = "")
                }  
                #Make file connection
                con <- file(filename,"r")
                df <- read.csv(con, header=TRUE)
                close(con)
                
                #Get complete cases
                good <- complete.cases(df)
                complete <- df[good,]
                nobs<-c(nobs, nrow(complete))
        }
        
        return(data.frame(id, nobs))
}