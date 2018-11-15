best <- function(state, outcome) {
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
    # Second, sort and subset by minimum
    my_data <- my_data[order(my_data[, j]), ] # sort by "outcome" col
    minimum <- my_data[1, j] # get minimum value
    result <- my_data[my_data[, j] == minimum, ] # get all obs. that have 
                                                 # minimum 
    # Finally
    sort(result$Hospital.Name)[1] # sort name vector and return the 1st
}