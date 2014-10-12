# the aim of this file is to understand more about the data files 
# from the HFD and to arrange it in a 'tidy' format 

# This can be done in an automated way by noting that the 
# first line of each file contains a description
# and the third line contains the variable names

file_names <- list.files(
  "data/hfd/Files/zip_w/"
  )


fn <- function(x){
  inputs <- readLines(
    paste("data/hfd/Files/zip_w", x, sep="/"),
    n=3
    )
    
  return(list(
    
    desc=inputs[1],
    vars=inputs[3]
    ))
}

summaries <- llply(file_names, fn)
names(summaries) <- file_names
