# rankall - returns the hospital name and state for a given rank and outcome
# e.g. the fourth best hospital in each state for heart attacks
# e.g. the worst hospital in each state for pneumonia
# num = rank, one of integer|"best"|"worst"
# outcome = illness, one of "heart attack"|"pneumonia"|"heart failure"

rankall <- function(outcome, num = "best") {
  ## check that outcome is valid
  outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  if(!(outcome %in% names(outcomes))) {
    stop("invalid outcome")
  }
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
  
  ## Return hospital name in that state with the given rank 30-day death rate
  data <- data[, c(2,7,outcomes[outcome])]
  ## Rename human readable
  names(data) <- c("hospital", "state", "outcome")
  ## remove NA
  data <- na.omit(data)
  ## Sort by outcome
  data <- data[order(data$state, data$outcome, data$hospital),]
  # Split by state
  data <- split(data, data$state)
  
  ## Get list by num
  
  if(num == "best") num <-1
  
  data_list = list()
  
  # loop through all states and extract the correctly ranked hospital
  for (cur_state in names(data)) {
    if(num == "worst") {
      worst <- nrow(data[[cur_state]])
      ranked_hospital <- data[[cur_state]][worst,]
    } else {
      ranked_hospital <- data[[cur_state]][num,]
    }
    
    ## create df for current state
    state_df <- data.frame(
      hospital = ranked_hospital$hospital, 
      state = cur_state, 
      stringsAsFactors=FALSE,
      row.names = cur_state)
    
    
    ## Add the current data to the list of results
    data_list[[cur_state]] <- state_df
  }
  
  ## format result set as a dataframe
  result <- do.call(rbind, data_list)
  ## Sort by state abbreviation and return the result
  result[ with( result, order(state)) , ]
}

