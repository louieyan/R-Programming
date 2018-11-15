corr <- function(directory, threshold = 0) {
    # "directory" is a character vector of lenght 1 indicating the
    # location of the CSV files
    
    # "id" is a integer vector indicating the monitor ID number
    # that will be used
    
    # return a dataframe that contains two columns, "id" and "nobs"
    # represent ID and numbers of complete cases
    # Yanlei 2018-11-02 18:44:42 CST
    
    complete_info <- complete(directory)
    id <- complete_info$id
    nobs <- complete_info$nobs
    # get the required ID 
    id <- id[nobs > threshold]
    if(length(id) == 0) {
        return(numeric())
    }
    # retrive data
    all_files_name <- list.files(path = directory, pattern = "*.csv")
    # select files according to "id" 
    # and convert to relative directory
    specific_files_name_with_path <- paste(directory, "/", all_files_name[id],
                                           sep = "")
    # use lapply to prevent looping
    # myfiles is a list
    myfiles <- lapply(specific_files_name_with_path, read.csv)
    #for(fi in myfiles) {
    #    sulfate <- fi$sulfate
    #    nitrate <- fi$nitrate
    #    both <- !is.na(sulfate) & !is.na(nitrate)
    #    sulfate <- sulfate[both]
    #    nitrate <- nitrate[both]
    #    
    #    result <- c(result, cor(sulfate, nitrate))
    #}
    helper <- function(fi) {
        sulfate <- fi$sulfate
        nitrate <- fi$nitrate
        both <- !is.na(sulfate) & !is.na(nitrate)
        sulfate <- sulfate[both]
        nitrate <- nitrate[both]
        cor(sulfate, nitrate)
    }
    sapply(myfiles, helper)
}
