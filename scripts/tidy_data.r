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


summaries

# 
# > summaries
# 
# $asfrRR.txt
# $asfrRR.txt$desc
# [1] "Period fertility rates by calendar year and age (Lexis squares, age in completed years (ACY))"
# 
# $asfrRR.txt$vars
# [1] "Code Year    Age        ASFR"
# 
# 
# $birthsRR.txt
# $birthsRR.txt$desc
# [1] "Live births by calendar year and age (Lexis squares, age in completed years (ACY))"
# 
# $birthsRR.txt$vars
# [1] "Code Year    Age        Total"
# 
# 
# 
# $cpfrRR.txt
# $cpfrRR.txt$desc
# [1] "Cumulative period fertility rates (Lexis squares)"
# 
# $cpfrRR.txt$vars
# [1] "Code Year    Age     CPFR"
# 
# 
# 
# $exposRR.txt
# $exposRR.txt$desc
# [1] "Female population exposure by calendar year and age (Lexis squares, age in completed years (ACY))"
# 
# $exposRR.txt$vars
# [1] "Code Year    Age        Exposure"



data_asfr <- read.table(
  file="data/hfd/Files/zip_w/asfrRR.txt",
  header=T,
  skip=2
  )

names(data_asfr) <- tolower(names(data_asfr))
data_asfr$age <- revalue(data_asfr$age, replace=c("12-" = "12", "55+" = "55"))
data_asfr$age <- as.numeric(as.character(data_asfr$age))


####

data_births <- read.table(
  file="data/hfd/Files/zip_w/birthsRR.txt",
  header=T,
  skip=2
)

names(data_births) <- tolower(names(data_births))
data_births$age <- revalue(data_births$age, replace=c("12-" = "12", "55+" = "55"))
data_asfr$age <- as.numeric(as.character(data_asfr$age))

####

data_cpfr <- read.table(
  file="data/hfd/Files/zip_w/cpfrRR.txt",
  header=T,
  skip=2
)

names(data_cpfr) <- tolower(names(data_cpfr))

####

data_expos <- read.table(
  file="data/hfd/Files/zip_w/exposRR.txt",
  header=T,
  skip=2
)

names(data_expos) <- tolower(names(data_expos))

data_combined <- join(
  x=data_asfr,
  y=data_births
  ) 

data_combined <- join(
  x=data_combined,
  y=data_cpfr
) 

data_combined <- join(
  x=data_combined,
  y=data_expos
) 


data_combined <- mutate(data_combined, birth_rate=total/exposure)

write.csv(
  data_combined,
  file="data/tidy/lexis_square_combined.csv"
  )
