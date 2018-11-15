complete <- function(directory, id = 1:332) {
    # "directory" is a character vector of lenght 1 indicating the
    # location of the CSV files
    
    # "id" is a integer vector indicating the monitor ID number
    # that will be used
    
    # return a dataframe that contains two columns, "id" and "nobs"
    # represent ID and numbers of complete cases
    # Yanlei 2018-11-02 18:44:42 CST
    
    # read all files under the given "directory"
    all_files_name <- list.files(path = directory, pattern = "*.csv")
    # select files according to "id" 
    # and convert to relative directory
    specific_files_name_with_path <- paste(directory, "/", all_files_name[id],
                                           sep = "")
    # use lapply to prevent looping
    # myfiles is a list
    myfiles <- lapply(specific_files_name_with_path, read.csv) 
    
    # add content to id and nobs
    id <- c()
    nobs <- c()
    for(fi in myfiles) {
        id <- c(id, fi$ID[1])
        temp <- sum(!is.na(fi$sulfate) & !is.na(fi$nitrate))
        nobs <- c(nobs, temp)
    }
    data.frame(id, nobs)
}