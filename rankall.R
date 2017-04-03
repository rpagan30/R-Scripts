rankall <- function(outcome, num = "best") {
        ## Read outcome data
        con <- file("/Users/nildavargas/Documents/Clases 2017/R Programming/R Projects/ProgAssignment3-data/outcome-of-care-measures.csv","r")
        df <- read.csv(con, header=TRUE)
        close(con)
        #print(num)
        
        ##List of outcomes
        Outcomes <- c("heart attack", "heart failure", "pneumonia")          
        
        ##Gets the sum of a boolean vector to check if the outcome can be found 
        ##in the list of outcomes.
        checkOutcome <- sum(as.numeric(Outcomes==outcome))    
        
        ## Checks it outcome is valid.
        if(checkOutcome == 0) {                                            
                paste("Error in best(", state, ", ", outcome, ", ", num, ") : invalid outcome") 
        }
        ## Checks if num is valid.
        else if(is.numeric(num) && num > length(df[,11])) {
                N <- NA
                print("HERE")
                print(N)
        } 
        else {
                if(Outcomes[1] == outcome) {
                        partition <- df[,11]
                } else if(Outcomes[2] == outcome) {
                        partition <- df[,17]
                } else if(Outcomes[3] == outcome) {
                        partition <- df[,23]
                }
                
                ## Gets vectors of all hospitals, all outcomes and all states
                AllHospitals <- as.character(df$Hospital.Name)
                AllOutcomes <- partition
                AllStates <- df$State
                
                ## Gets vector of unique states.
                States <- unique(df$State)
                
                ## Converts factor vector to numeric.
                AllOutcomes <- as.numeric(levels(AllOutcomes)[AllOutcomes])
                
                ## Conditional vector that stores cases that have an NA value.
                bad <- is.na(AllOutcomes)
                
                ## Stores cases that have existing values.
                ValidHospitals <- AllHospitals[!bad]
                ValidOutcomes <- AllOutcomes[!bad]
                ValidStates <- AllStates[!bad]
                
                ## Stores valid values in a new data frame.
                df1 <- data.frame(Hospitals = ValidHospitals, Rates = ValidOutcomes, State = ValidStates)

                ## Declares NULL values to be printed outside of the for loop to resolve scoping issues.
                Best <- NULL
                Worst <- NULL
                TotalHospitals <- NULL

                for(i in 1:length(States)) {
                        #print(i)
                        #print(States[i])
                        ## New data frame with the i-th state hospitals and outcomes.
                        tempHospitals <- df1$Hospitals[df1$State == States[i] ]
                        tempRates <- df1$Rates[df1$State == States[i]]
                        tempdf <- data.frame(Hospitals = tempHospitals, Rates = tempRates)
                        
                        ## Sort data frames according to mortality rates. 
                        ## Done in this order so that hospitals will be in 
                        ## alphabetical order when there is a tie in rank.
                        tempdf <- tempdf[order(tempdf$Hospitals) ,]
                        tempdf <- tempdf[order(tempdf$Rates) ,]
                        #print(tempdf)
                        
                        #print(rbind(Best, c(tempdf$Hospitals[tempdf$Rates == min(tempdf$Rates)], States[i])))
                        
                        if(num == "Best" || num == "best") {
                                Best <- rbind(Best, data.frame(Hospitals = tempdf$Hospitals[tempdf$Rates == min(tempdf$Rates)], States = States[i]))
                                #print(Best)
                                
                        } else if( num == "Worst" || num == "worst") {
                                Worst <- rbind(Worst, data.frame(Hospitals = tempdf$Hospitals[tempdf$Rates == max(tempdf$Rates)], States = States[i]))
                                #print(Worst)
                        } else {
                                TotalHospitals <- rbind(TotalHospitals, data.frame(Hospitals = tempdf$Hospitals[num], States = States[i]))
                                #print(TotalHospitals)
                        }
                }
                ## Return
                if(num == "Best" || num == "best") {
                        Best <- Best[order(Best$States) ,]
                        Best
                } else if( num == "Worst" || num == "worst") {
                        Worst <- Worst[order(Worst$States) ,]
                        Worst
                } else {
                        TotalHospitals <- TotalHospitals[order(TotalHospitals$States) ,]
                        TotalHospitals
                }

        }
       
}
