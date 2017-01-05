best <- function(state, outcome){
    'validate outcome'
    if (outcome %in% c("heart attack","heart failure", "pneumonia") == FALSE){
        stop("invalid outcome")
    }
    
    'read data'
    data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", 
                     na.strings="Not Available", 
                     stringsAsFactors = FALSE)
    
    'Select variables of interest'
    outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    
    'Check for invalid states'
    if (state %in% unique(data$State) == FALSE){
        stop("invalid state")
    }
    
    data <- data[data$State == state, c(2,7,outcomes[[outcome]])]
    'rename columns'
    colnames(data)<- c("hospital", "state", "outcome")  
    
    data <- data[complete.cases(data),]
    
    best <- min(data$outcome)

    data <- data[data$outcome == best,]
    sort(data$hospital)[1]
}

