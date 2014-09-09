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
                return(bestHospitalAll(outcome, data))
            }else if(num ==  "worst"){
                return(worstHospitalAll(outcome, data))
            }else{
                return(limitedBestHospitalAll(outcome, data, num))
            }
        }else{
            stop("invalid outcome")
        }
        
    }else{
        stop("invalid state")
    }
    
}





bestHospitalAll <- function( outcome, data){
    column <- outcomeOption[outcome]
    
    usableData <- data
    usableData[,column[1,1]] <- as.numeric(usableData[,column[1,1]])
    splitData <- split(usableData,usableData$State )
    res <- data.frame(row.names = c("hospital", "state"));

    for (i in splitData) {
    #    message(names(i))
        i[,column[1,1]] <- as.numeric(i[,column[1,1]])
        orderedI <- i[order(i[,c(column[1,1], 2)], na.last=TRUE) , ]
        res <- rbind(res, data.frame(hospital = orderedI$Hospital.Name[1], state = orderedI$State[1]))
    }
    res
}

worstHospitalAll <- function(outcome, data){
    column <- outcomeOption[outcome]
    
    usableData <- data
    usableData[,column[1,1]] <- as.numeric(usableData[,column[1,1]])
    splitData <- split(usableData,usableData$State )
    res <- data.frame(row.names = c("hospital", "state"));
    
    for (i in splitData) {
        #    message(names(i))
        i[,column[1,1]] <- as.numeric(i[,column[1,1]])
        orderedI <- i[order(i[,c(column[1,1], 2)], na.last=TRUE) ,  ]
        res <- rbind(res, data.frame(hospital = orderedI$Hospital.Name[orderedI[,column[1,1]] == max(orderedI[,column[1,1]], na.rm = TRUE)], state = orderedI$State[orderedI[,column[1,1]] == max(orderedI[,column[1,1]], na.rm = TRUE)]))
    }
    cres <- res[complete.cases(res[,]),]
    cres[,c(1,2)]
}

limitedBestHospitalAll <- function(outcome, data, num){
    column <- outcomeOption[outcome]
    
    usableData <- data
    usableData[,column[1,1]] <- as.numeric(usableData[,column[1,1]])
    splitData <- split(usableData,usableData$State )
    res <- data.frame(row.names = c("hospital", "state"));
    
    for (i in splitData) {
        #    message(names(i))
        i[,column[1,1]] <- as.numeric(i[,column[1,1]])
        orderUse <- order(i[,column[1,1]], i$Hospital.Name, na.last=TRUE)
        orderedI <- i[orderUse,]
        res <- rbind(res, data.frame(hospital = orderedI$Hospital.Name[num], state = orderedI$State[1]))
    }
    res
    
}

#rankall("pneumonia", "worst")
#rankall("heart failure")
#tail(rankall("pneumonia", "worst"), 3)
#head(rankall("heart attack", 20), 10)
#rankall("heart attack", 4)
#rankall("pneumonia", "worst")
#tail(rankall("pneumonia", "worst"), 3)

#submit()

