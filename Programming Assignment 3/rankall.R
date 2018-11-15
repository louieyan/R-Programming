rankall <- function(outcome, num = "best") {
    # Yanlei 2018/11/15
    # It is very important to backup variables outside
    # for loop when these variables will be changed inside the loop.
    # For example, in this function's for-loop, the first two lines
    # are used to achieving this goal.
    
    
    # Read the data
    
    hosp_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    set_of_states <- sort(unique(hosp_data$State))
    # heart attack --- 11   heart failure --- 17  # pneumonia --- 21
    set_of_outcome <- c("heart attack", "heart failure", "pneumonia")
    names(set_of_outcome) <- c(11, 17, 23)
    
    # Chech "outcome" are valid
    if (!outcome %in% set_of_outcome) {
        stop("invalid outcome")
    }
    
    # Creat empty vectors
    col_hospital <- c()
    col_state <- c()
    # for each state 
    for (state in set_of_states) {
        my_data <- hosp_data
        each_num <- num
        #First, get the obs. that don't have NAs in the "outcome" column
        my_data <- my_data[my_data$State == state, ] # subset by "state"
        j <- names(set_of_outcome)[set_of_outcome == outcome] # get col number
        j <- as.numeric(j) # coerce char into num
        my_data[, j] <- as.numeric(my_data[, j]) # coerce char into num
        my_data <- my_data[!is.na(my_data[, j]), ] # get rid of NAs
        num_of_hospital <- nrow(my_data)  # get total number of obs.
        # Second, sort and translate "best" and "worst" into numbers
        my_data <- my_data[order(my_data[, j]), ] # sort by "outcome" col
        if (each_num == "best") {
            each_num <- 1
        }
        if (each_num == "worst") {
            each_num <- num_of_hospital
        }
        if (each_num > num_of_hospital) {
            col_hospital <- c(col_hospital, NA)
            col_state <- c(col_state, state)
            next
        }
        # beak ties using order()
        hospitals <- my_data$Hospital.Name[order(my_data[, j], my_data$Hospital.Name)]
        # Finally
        col_hospital <- c(col_hospital, hospitals[each_num])
        col_state <- c(col_state, state)
    }
    # turn into dataframe
    result <- data.frame("hospital" = col_hospital, "state" = col_state)
    rownames(result) <- set_of_states # add row names
    result
    
}

