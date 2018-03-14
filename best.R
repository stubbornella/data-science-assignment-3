# Best finds the best hospital for a given state and outcome category
# outcome is one of “heart attack”, “heart failure”, or “pneumonia”

best <- function(state="MD", outcome) {
  ## check that state is valid
  if (!(state %in% state.abb)) {
    stop("invalid state")
  }
  
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", stringsAsFactors=FALSE)
  
  ## Check outcome are valid and get outcome column number
  
  if(outcome == "pneumonia") { 
    column <- 23
  } else if(outcome == "heart attack") {
    column <- 11
  } else if(outcome == "heart failure") {
    column <- 17
  } else {
    stop("invalid outcome")
  }
  
  ## Filter by state
  in_state <- outcome_data[outcome_data[,7] %in% state, ] #filter by state
  #data$Number <- as.numeric(data$Number)
  
  ## Retain only 2 required columns: name, outcome
  filtered_data <- in_state[c(2, column)]
  ## Turn "Not Available" string into a proper NA and filter it.
  filtered_data[,2] <- suppressWarnings(as.numeric(filtered_data[,2]))
  filtered_data <- na.omit(filtered_data)
  
  #ed_exp4 <- na.omit(subset(outcome_data, State == state, select = c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")))
  
  #print(filtered_data)
  
  ## Return hospital name in that state with lowest 30-day death rate
  with(filtered_data, Hospital.Name[filtered_data[,2] == min(filtered_data[,2])])[1]
}