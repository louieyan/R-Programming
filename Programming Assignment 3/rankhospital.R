rankhospital <- function(state, outcome, num = "best") {
    # Yanlei 2018/11/14
    
    
    
    # Read the data
    
    my_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    set_of_states <- unique(my_data$State)
    # heart attack --- 11   heart failure --- 17  # pneumonia --- 21
    set_of_outcome <- c("heart attack", "heart failure", "pneumonia")
    names(set_of_outcome) <- c(11, 17, 23)
    
    
    
    # Chech that state and outcome are valid
    if (!state %in% set_of_states) {
        stop("invalid state")
    }
    if (!outcome %in% set_of_outcome) {
        stop("invalid outcome")
    }
    
    
    
    # Return hospital name in that state with lowest 30-day death rate
    
    #First, get the obs. that don't have NAs in the "outcome" column
    my_data <- my_data[my_data$State == state, ] # subset by "state"
    j <- names(set_of_outcome)[set_of_outcome == outcome] # get col number
    j <- as.numeric(j) # coerce char into num
    my_data[, j] <- as.numeric(my_data[, j]) # coerce char into num
    my_data <- my_data[!is.na(my_data[, j]), ] # get rid of NAs
    num_of_hospital <- nrow(my_data)  # get total number of obs.
    # Second, sort and translate "best" and "worst" into numbers
    my_data <- my_data[order(my_data[, j]), ] # sort by "outcome" col
    if (num == "best") {
        num <- 1
    }
    if (num == "worst") {
        num <- num_of_hospital
    }
    if (num > num_of_hospital) {
        return(NA)
    }
    # beak ties using order()
    hospitals <- my_data$Hospital.Name[order(my_data[, j], my_data$Hospital.Name)]
    # Finally
    hospitals[num] # return the num_th best hospital
}