source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")

best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    
    
    
    
    ## Check that state and outcome are valid
    validStates <- names(table(data$State))# get the states that the file contains
 
    usableData <- NULL
    res <- NULL
    if(match(state,validStates) > 0){
        if(outcome == "heart attack") {
            usableData <- subset(data, data$State == state)
            usableData[,11] <- as.numeric(usableData[,11])
            usableData <- subset(usableData, !is.na(usableData[,11]))      
            res <- aggregate(usableData[, c(2,11)], list(usableData[,11]), min)
            return(res[1,2])
        }else if(outcome == "heart failure"){
            usableData <- subset(data, data$State == state)
            usableData[,17] <- as.numeric(usableData[,17])
            usableData <- subset(usableData, !is.na(usableData[,17]))      
            res <- aggregate(usableData[, c(2,17)], list(usableData[,17]), min)
            return(res[1,2])        
        }else if(outcome == "pneumonia"){
            usableData <- subset(data, data$State == state)
            usableData[,23] <- as.numeric(usableData[,23])
            usableData <- subset(usableData, !is.na(usableData[,23]))      
            res <- aggregate(usableData[, c(2,23)], list(usableData[,23]), min)
            return(res[1,2])
        }
        
    }

    
}


submit()

