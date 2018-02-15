rankall <- function(outcome, num = "best") {
  ## Read csv file
  outcomeData <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", 
                          stringsAsFactors = FALSE)
  
  ## Assigning specific column Indices of diseases for data extraction
  disease <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  colIndex <- disease[outcome]
  
  ## if nothing gets assigned to colIndex then the outcome argument is invalid
  if(is.na(colIndex)) stop("invalid outcome")
  
  ## Extraction of Hospital Name(2), State(7) and Mortality rate for specific disease 
  filteredData <- outcomeData[,c(2,7,colIndex)]
  
  ## NA rows removal
  filteredData <- filteredData[complete.cases(filteredData),]
  
  ## Naming the columns
  colnames(filteredData) <- c("hospital","state","outcome")
  
  ## Sorting the data according to mortality rate and alphabetical as well
  sortedData <- filteredData[order(filteredData$outcome,filteredData$hospital),]
  
  ## Assigning num for "best"
  if (num == "best") num <-  1
  
  ## Extracting the required rank hospital from every state 
  finalData <- lapply(split(sortedData,sortedData$state),function(z) {
    if (num == "worst") num <-  nrow(z)
    z[num,c(1,2)]})

  ## Converting the list output of lapply to a data frame
  as.data.frame(do.call(rbind,finalData))
}