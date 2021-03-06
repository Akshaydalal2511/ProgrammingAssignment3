rankhospital <- function(state, outcome, num) {
  ## Read csv file
  outcomeData <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", 
                          stringsAsFactors = FALSE)
  
  ## Checking for a valid state argument
  if(!is.element(state,outcomeData[,7])) stop("invalid state")
  
  ## Assigning specific column Indices of diseases for data extraction
  disease <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  colIndex <- disease[outcome]
  
  ## if nothing gets assigned to colIndex then the outcome argument is invalid
  if(is.na(colIndex)) stop("invalid outcome")
  
  ## Specific State data extraction
  stateData <- outcomeData[which(outcomeData$State == state),]
  
  ## Extraction of Hospital Name(2), State(7) and Mortality rate for specific disease 
  filteredData <- stateData[,c(2,7,colIndex)]
  
  ## NA rows removal
  filteredData <- filteredData[complete.cases(filteredData),]
  
  ## Naming the columns
  colnames(filteredData) <- c("hospital","state","outcome")
  
  ## Sorting the data according to mortality rate and alphabetical as well
  sortedData <- filteredData[order(filteredData$outcome,filteredData$hospital),]
  
  ##Getting the Rank number
  if (num == "best") num <-  1
  else if (num == "worst") num <-  nrow(sortedData)
  else if (num > nrow(sortedData)) return(NA)
  
  ## Returning the required rank hospital name
  sortedData[num,]
}