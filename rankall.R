source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")
library("plyr")
outcomeOption <- data.frame(11,17,23)
names(outcomeOption) <- c("heart attack","heart failure", "pneumonia")



rankall <- function(outcome, num = "best"){
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    
    ## Check that state and outcome are valid

    validNum <- list("best", "worst")
    usableData <- NULL
    res <- NULL

    if(num %in% validNum | num > 0){
        if(outcome  %in% c("heart attack","heart failure", "pneumonia")) {
            if(num == "best"){
                return(bestHospital(outcome, data))
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





bestHospital <- function( outcome, data){
    column <- outcomeOption[outcome]
    
    usableData <- subset(data, 
                         !is.na(
                             as.numeric(
                                 data[,column[1,1]]
                             )
                         )
    )
    usableData[,column[1,1]] <- as.numeric(usableData[,column[1,1]])
    rank <-  ddply(usableData, usableData[,column[1,1]], summarize, rank = max(usableData[,column[1,1]]))
    rank <- rank(rank$x)
    rank
}

worstHospital <- function(outcome, data){
    column <- outcomeOption[outcome]
    
    usableData <- subset(data,!is.na(
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

#rankall( "heart failure", "best")