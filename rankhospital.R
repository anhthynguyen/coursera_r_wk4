rankhospital <- function(state, outcome, num= "best"){
    data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClass="character")
    'Select variables of interest'
    data <- data[,c(2,7,11,17,23)]
    colnames(data)[3:5]<- c("heart.attack", "heart.failure", "pneumonia")
    data[,3:5] <- suppressWarnings(lapply(data[,3:5], as.numeric))
    
    'Check for invalid arguments'
    if (state %in% levels(as.factor(data$State)) == FALSE){
        stop("invalid state")
    }
    if (outcome %in% c("heart attack","heart failure", "pneumonia") == FALSE){
        stop("invalid outcome")
    }
    

    data <- data[data$State == state,]
    column <- list("heart attack" = data$heart.attack, "heart failure" = data$heart.failure, "pneumonia" = data$pneumonia)
    
    
    li <- order(column[[outcome]], data$Hospital.Name)

    data <- data[!(is.na(column[[outcome]])),]    
    
    'Return conditions'
    if (0 < num && num <= length(data$Hospital.Name)){
        return (data[li,]$Hospital.Name[num])
    } else if(num == "worst"){
        return (data[li,]$Hospital.Name[length(data$Hospital.Name)])
    } else if(num > length(data$Hospital.Name) || num < 0){
        return (NA)
    } else {
        return (data[li,]$Hospital.Name[1])
    }

}
