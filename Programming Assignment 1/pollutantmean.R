pollutantmean <- function(directory, pollutant, id = 1:332) {
    # "directory" is a character vector of lenght 1 indicating the
    # location of the CSV files
    
    # "pollutant" is a character vector of length 1 indicating the
    # pollutant we will calculate the mean
    
    # "id" is a integer vector indicating the monitor ID number
    # that will be used
    
    # return the mean of the pillutant across all the CSV files in
    # the "id" vector (ignoring NA values)
    # Yanlei 2018-11-02 16:00:44 CST
    
    # read all files under the given "directory"
    all_files_name <- list.files(path = directory, pattern = "*.csv")
    # select files according to "id" 
    # and convert to relative directory
    specific_files_name_with_path <- paste(directory, "/", all_files_name[id],
                                           sep = "")
    # use lapply to prevent looping
    # myfiles is a list
    myfiles <- lapply(specific_files_name_with_path, read.csv) 
    
    # use original total / num to compute mean
    total <- 0
    num <- 0
    for(fi in myfiles) {
        temp <- fi[pollutant]
        num <- num + sum(!is.na(temp))
        total <- total + sum(temp[!is.na(temp)])
    }
    total / num
}

