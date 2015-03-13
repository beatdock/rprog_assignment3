rankall <- function(outcome, num = "best") {
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        split_data <- split(data, data$State)
        rankall_data <- data.frame()
        for (i in 1:length(split_data)) {
                sub_data <- data.frame(split_data[i])
                switch(outcome,
                       "heart attack" = sub_data <- sub_data[ ,c(2, 7, 11)]
                       ,
                       "heart failure" = sub_data <- sub_data[ ,c(2, 7, 17)]
                       ,
                       "pneumonia" = sub_data <- sub_data[ ,c(2, 7, 23)]
                       ,
                       stop("invalid outcome")
                )
                colnames(sub_data) <- c("hospital", "state", "Rate")
                sub_data$Rate <- as.numeric(sub_data$Rate)
                ordered_data <- sub_data[order((sub_data$Rate), (sub_data$hospital), na.last = NA), ]
                ordered_data$Rank <- c(1:nrow(ordered_data))
                if(num %in% ordered_data$Rank) {
                        rank_data <- data.frame(x = ordered_data[num,1], y = ordered_data[1,2])
                        colnames(rank_data) <- c("hospital", "state")
                } else if(is.numeric(num)) {
                        rank_data <- data.frame(x = NA, y = ordered_data[1,2])
                        colnames(rank_data) <- c("hospital", "state")
                } else {
                        switch(num,
                               "best" = {rank_data <- data.frame(x = ordered_data[num,1], y = ordered_data[1,2])
                                        colnames(rank_data) <- c("hospital", "state")
                               }
                               ,
                               "worst" = {rank_data <- data.frame(x = ordered_data[nrow(ordered_data),1], y = ordered_data[1,2])
                                        colnames(rank_data) <- c("hospital", "state")
                               }
                        )
                }       
                rankall_data <- rbind(rankall_data, rank_data)
        }
        print(rankall_data)
}