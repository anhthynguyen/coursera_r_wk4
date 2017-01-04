best <- function(state, outcome){
    data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClass="character")
    'Select variables of interest'
    data <- data[,c(2,7,11,17,23)]
    colnames(data)[3:5]<- c("heart.attack", "heart.failure", "pneumonia")
    data[,3:5] <- lapply(data[,3:5], as.numeric)

    'Check for invalid arguments'
    if (state %in% levels(as.factor(data$State)) == FALSE){
        stop("invalid state")
    }
    if (outcome %in% c("heart attack","heart failure", "pneumonia") == FALSE){
        stop("invalid outcome")
    }


    data <- data[data$State == state,]
    
    'List to return data for outcome of interest and best value'
    
    outcome_data <- list("heart attack" = list("data" = data$heart.attack, "bestValue" = min(data$heart.attack, na.rm = TRUE)), 
                         "heart failure" = list("data" = data$heart.failure, "bestValue" = min(data$heart.failure, na.rm = TRUE)), 
                         "pneumonia" = list("data" = data$pneumonia, "bestValue" = min(data$pneumonia, na.rm=TRUE)))

    data <- data[outcome_data[[outcome]]$data == outcome_data[[outcome]]$bestValue,]
    data <- data[is.na(data$Hospital.Name) == FALSE,]
    sort(data$Hospital.Name)[1] 'Sort by hospital name and return first item'
}

best("TX","hart attack")
