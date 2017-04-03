

pollutantmean <- function(directory, pollutant, id = 1:332) {
        
        #Selects .csv's that are going to be read according to "<ID_Number>.csv".
        cleandata <- NULL;
        
        for(i in id) {
                
                if(i < 10) {
                        filename <- paste(directory, "/00", as.character(i),".csv", sep = "")
                } else if(i >= 10 && i < 100) {
                        filename <- paste(directory,"/0", as.character(i),".csv", sep = "")
                } else if(i>=100) {
                        filename <- paste(directory,"/", as.character(i),".csv", sep = "")
                }  
                con <- file(filename,"r")
                df <- read.csv(con, header=TRUE)
                close(con)
                data<-df[pollutant]
                bad <-is.na(data)
                clean <- data[!bad]
                cleandata <- c(cleandata, clean) #Concatenates clean data.
        }
        mean(cleandata) #Mean of clean data
}