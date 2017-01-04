outcome <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClass="character")
head(outcome)
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])

