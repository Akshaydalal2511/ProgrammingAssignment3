best <- function(state, outcome) {
        ## Read csv file
        outcomeData <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", 
                          stringsAsFactors = FALSE)
        
        ## Checking for a valid state argument
        if(!is.element(state,outcomeData[,7])) stop("invalid state")
        
        ## Checking for a valid outcome argument and assigning specific column Indices 
        ## for valid outcomes for data extraction
        if(outcome == "heart attack") {
                colIndex <- 11
        }
        else if(outcome == "heart failure") {
                colIndex <- 17
        }
        else if(outcome == "pneumonia") {
                colIndex <- 23
        }
        else stop("invalid outcome")
        
        ## Specific State data extraction
        stateData <- outcomeData[which(outcomeData$State == state),]
        
        ## Extraction of Hospital Name(2), State(7) and Mortality rate for specific disease 
        filteredData <- stateData[,c(2,7,colIndex)]
        
        ## NA rows removal
        filteredData <- filteredData[complete.cases(filteredData),]
        
        ## Naming the columns
        colnames(filteredData) <- c("hospital","state","outcome")
        
        ## Ordering the data based on Ascending order of mortality rate
        sortedData <- filteredData[order(filteredData$outcome),]
        
        ## Returning the top ranked hospital name
        sortedData[1,1]
}