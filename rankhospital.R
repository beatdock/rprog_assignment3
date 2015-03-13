rankhospital <- function(state, outcome, num = "best") {
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        if(!any(state == data$State)) {
                stop("invalid state")
        } else {
                sub_data <- data[data$State == state, ]
                switch(outcome,
                       "heart attack" = sub_data <- sub_data[ ,c(2, 11)]
                       ,
                       "heart failure" = sub_data <- sub_data[ ,c(2, 17)]
                       ,
                       "pneumonia" = sub_data <- sub_data[ ,c(2, 23)]
                       ,
                       stop("invalid outcome")
                )        
        }
        colnames(sub_data) <- c("Hospital.Name", "Rate")
        sub_data$Rate <- as.numeric(sub_data$Rate)
        ordered_data <- sub_data[order((sub_data$Rate), (sub_data$Hospital.Name), na.last = NA), ]
        ordered_data$Rank <- c(1:nrow(ordered_data))
        if(num %in% ordered_data$Rank) {
                print(as.vector(ordered_data[num,1], mode = "character"))
        } else if(is.numeric(num)) {
                print(NA)
        } else {
                switch(num,
                       "best" = print(as.vector(ordered_data[1,1], mode = "character"))
                       ,
                       "worst" = print(as.vector(ordered_data[nrow(ordered_data),1], mode = "character"))
                       ,
                       print(NA)
                )
        }       
}
