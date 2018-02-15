rankhospital <- function(state, outcome, num) {
  ## Read csv file
  ##outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
  
  ## Ordering the data alphabetically for tie breakers
  alphabeticalData <- filteredData[order(filteredData$hospital),]
  
  ## Ordering the data based on Ascending order of mortality rate
  sortedData <- alphabeticalData[order(alphabeticalData$outcome),]
  
  ##Getting the Rank number
  if (num == "best") num <-  1
  else if (num == "worst") num <-  nrow(sortedData)
  else if (num > nrow(sortedData)) return(NA)
  
  ## Returning the required rank hospital name
  sortedData[num,]
}