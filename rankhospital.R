# rank hostpital returns the hospital name for a given state, health outcome and rank

rankhospital <- function(state, outcome, num = "best") {
  ## check that state is valid
  if (!(state %in% state.abb)) {
    stop("invalid state")
  }
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
  ## filter by state
  data <- data[data$state %in% state, ] 
  ## remove NA
  data <- na.omit(data)
  #sort by outcome and hospital (ascending)
  data <- data[order(data$outcome, data$hospital, data$state),]
  
  if (typeof(num) == "double"){
    return(data[num,"hospital"])
  } else if (num == "best"){
    return(data[1,"hospital"])
  }else if (num == "worst"){
    return(data[nrow(data),"hospital"])
  }
}