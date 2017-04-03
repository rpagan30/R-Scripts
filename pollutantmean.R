

pollutantmean <- function(directory, pollutant, id = 1:332) {
        
        #Selects .csv's that are going to be read according to "<ID_Number>.csv".
        cleandata <- 0;
        
        for(i in id) {
                
                df <- readDirectory(nameDirectory(directory)) #Gets directories and reads data.
                clean <- RemoveNa(df) #Removes NA values.
                cleandata <- cleandata + clean #Stores clean data.
        }
        mean(cleandata) #Mean of clean data
}


readDirectory(directory) {
        
        df <- read.csv(directory, header=TRUE)
        df
}


nameDirectory(directory) {
        
        if(i < 10) {
                directory <- paste(directory, "00", as.character(i),".csv", sep = "")
        } else if(i >= 10 && i < 100) {
                directory <- paste(directory,"0", as.character(i),".csv", sep = "")
        } else if(i>=100) {
                directory <- paste(directory, as.character(i),".csv", sep = "")
        }  
        directory
}

RemoveNA(df) {
        
        bad <-is.na(df$pollutant)
        clean <- df$pollutant[!bad]
        clean
}