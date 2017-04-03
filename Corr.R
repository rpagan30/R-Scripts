unclascorr <- function(directory, threshold = 0) {
        id <- 1:332
        df <- complete(directory,id)
        goodIDs = df[df["nobs"] > threshold, ]$id
        
        df2 <- NULL
        
        for(i in goodIDs) {
                
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
                df2 <- read.csv(con, header=TRUE)
                close(con)
                
                good <- complete.cases(df2)
                complete <- df2[good,]
                corr(df2[,1], df2[,2])
        }
        
        
}