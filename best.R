
## Ranks hospitals according to state and mortality rates. Gets data from "outcome-of-care-measures.csv" 
## taken from "https://hospitalcompare.hhs.gov/".

## Usage
## best("2-character US state abbreviation", "mortality rate condition" )
## Mortality rate conditions are "heart attack", "heart failure" and "pneumonia".
## EXAMPLE
## > best("MD", "heart attack")
##[1] "JOHNS HOPKINS HOSPITAL, THE"

best <- function(state, outcome) {
        ## Read outcome data
        con <- file("/Users/nildavargas/Documents/Clases 2017/R Programming/R Projects/ProgAssignment3-data/outcome-of-care-measures.csv","r")
        df <- read.csv(con, header=TRUE)
        close(con)
        
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
                paste("Error in best(", state, ", ", outcome, ") : invalid state")
        }
        ##Checks it outcome is valid.
        else if(checkOutcome == 0) {                                            
                paste("Error in best(", state, ", ", outcome, ") : invalid outcome") 
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
                
                ## Gets vector index of lowest value in ValidOutcomes.
                ## Gives Validhospitals corresponding vector index.
                Best <- ValidHospitals[ValidOutcomes == min(ValidOutcomes)]
                ## Outputs hospital with best ranking. In case of a tie, they are 
                ## sorted.
                Best <- sort(Best)
                Best
        }
        
}