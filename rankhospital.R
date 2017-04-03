rankhospital <- function(state, outcome, num ="best") {
        ## Read outcome data
        con <- file("/Users/nildavargas/Documents/Clases 2017/R Programming/R Projects/ProgAssignment3-data/outcome-of-care-measures.csv","r")
        df <- read.csv(con, header=TRUE)
        close(con)
        print(num)
        
        ## Check that the state and outcome are valid. Gets the sum of a boolean 
        ##vector to check if the state can be found in the dataframe.
        checkState <- sum(as.numeric(df$State==state))                         
        
        ##List of outcomes
        Outcomes <- c("heart attack", "heart failure", "pneumonia")          
        
        ##Gets the sum of a boolean vector to check if the outcome can be found 
        ##in the list of outcomes.
        checkOutcome <- sum(as.numeric(Outcomes==outcome))                      
        
        ##Checks if state is valid.
        if(checkState == 0) {                                                   
                paste("Error in best(", state, ", ", outcome, ", ", num, ") : invalid state")
        }
        ##Checks it outcome is valid.
        else if(checkOutcome == 0) {                                            
                paste("Error in best(", state, ", ", outcome, ", ", num, ") : invalid outcome") 
        }
        else if(is.numeric(num) && num > length(df[,11])) {
                N <- NA
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
                ## Return hospital name in that state with the lowest 30-day death 
                ## rate. Gets state hospital names and corresponding outcomes columns.
                StateHospitals <- as.character(df$Hospital.Name[df$State == state])
                StateOutcomes <- partition[df$State == state]
                
                ## Converts factor vector to numeric.
                StateOutcomes <- as.numeric(levels(StateOutcomes)[StateOutcomes])
                
                ## Conditional vector that stores cases that have an NA value.
                bad <- is.na(StateOutcomes)
                
                ## Stores cases that have existing values.
                ValidHospitals <- StateHospitals[!bad]
                ValidOutcomes <- StateOutcomes[!bad]
                
                df <- data.frame(Hospitals = ValidHospitals, Rate = ValidOutcomes)
                df <- df[order(df$Hospitals) ,]
                df <- df[order(df$Rate) ,]
                #print(df)
                
                if(num == "Best" || num == "best") {
                        Best <- df$Hospitals[df$Rate == min(df$Rate)]
                        Best
                } else if( num == "Worst" || num == "worst") {
                        Worst <- df$Hospitals[df$Rate == max(df$Rate)] 
                        Worst
                } else {
                        df$Hospitals[num]
                }
        }
}

     

