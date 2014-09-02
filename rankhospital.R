source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")
outcomeOption <- data.frame(11,17,23)
names(outcomeOption) <- c("heart attack","heart failure", "pneumonia")



rankhospital <- function(state, outcome, num){
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    validStates <- names(table(data$State))# get the states that the file contains
    validNum <- list("best", "worst")
    usableData <- NULL
    res <- NULL
    isValid <- match(state,validStates) > 0
    if(!is.na(isValid) & isValid & (num %in% validNum | num > 0)){
        if(outcome  %in% c("heart attack","heart failure", "pneumonia")) {
            if(num == "best"){
                return(bestHospital(state, outcome, data))
            }else if(num ==  "worst"){
                return(worstHospital(state, outcome, data))
            }else{
                return(limitedBestHospital(state, outcome, data, num))
            }
        }else{
            stop("invalid outcome")
        }
        
    }else{
        stop("invalid state")
    }
    
    
}


bestHospital <- function(state, outcome, data){
    column <- outcomeOption[outcome]

    usableData <- subset(data, data$State == state 
                         & !is.na(
                             as.numeric(
                                 data[,column[1,1]]
                             )
                         )
    )[,c(2,column[1,1])]
    usableData[,2] <- as.numeric(usableData[,2])
    orderUsable <- order(usableData[,2], usableData[,1])
    res <- usableData[orderUsable,c(1,2)]
    res[1,1]
}

worstHospital <- function(state, outcome, data){
    column <- outcomeOption[outcome]
    
    usableData <- subset(data, data$State == state 
                         & !is.na(
                             as.numeric(
                                 data[,column[1,1]]
                             )
                         )
    )[,c(2,column[1,1])]
    usableData[,2] <- as.numeric(usableData[,2])
    orderUsable <- order(usableData[,2], usableData[,1])
    res <- usableData[orderUsable,c(1,2)]
    res[length(res[,1]),1]
}

limitedBestHospital <- function(state, outcome, data, num){
    column <- outcomeOption[outcome]
    
    usableData <- subset(data, data$State == state 
                         & !is.na(
                             as.numeric(
                                 data[,column[1,1]]
                             )
                         )
    )[,c(2,column[1,1])]
    usableData[,2] <- as.numeric(usableData[,2])

    if(length(usableData[,1]) >= num ){
        
        orderUsable <- order(usableData[,2], usableData[,1])
        res <- usableData[orderUsable,c(1,2)]
        return(res[num,1])
    }else{
        return(NA)
    }
    
}
# rankhospital("TX", "heart failure", 4)
#[1] "DETAR HOSPITAL NAVARRO"
# rankhospital("MD", "heart attack", "worst")
#[1] "HARFORD MEMORIAL HOSPITAL"
# rankhospital("MN", "heart attack", 5000)
#[1] NA
# rankhospital("MD", "heart attack", "best")

#submit()
