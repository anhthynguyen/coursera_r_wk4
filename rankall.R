rankall <- function(outcome, rank = "best"){
    'validate args'
    if (outcome %in% c("heart attack","heart failure", "pneumonia") == FALSE){
        stop("invalid outcome")
    }
    
    data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", 
                     na.strings="Not Available", 
                     stringsAsFactors = FALSE)
    
    'Select variables of interest'
    outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    data <- data[,c(2,7,outcomes[[outcome]])]
    'rename columns'
    colnames(data)<- c("hospital", "state", "outcome")
    
    'order data'
    li <- order(data$state, data$outcome, data$hospital)
    data <- data[li,]
    
    'remove NAs'
    data <- data[complete.cases(data),]
    
    'handle data'
    data <- split(data, data$state)
    
    fun <- function(x, rank){
        if(rank == "best"){
            return(x$hospital[1])
        } else if(rank =="worst"){
            return(x$hospital[length(x$hospital)])
        } else {
            return(x$hospital[rank])
        }
    }
    
    data <- sapply(data, fun, rank = rank)
    data <- data.frame("hospital" = data, "state" = names(data))
}
