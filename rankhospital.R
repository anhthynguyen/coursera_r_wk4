rankhospital <- function(state, outcome, num= "best"){
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

    li <- order(data$outcome, data$hospital)

       
    data <- data[li,]

    data$hospital    
    
    if(num == "best"){
        return(data$hospital[1])
    } else if(num == "worst"){
        return(data$hospital[length(data$hospital)])
    } else if(num > length(data$hospital) | num < 0){
        return(NA)
    } else {
        return(data$hospital[num])
    }

}

