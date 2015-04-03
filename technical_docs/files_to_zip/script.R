# Merging datasets in the Human Fertility Database using tidy data 
# and split-apply-combine paradigms in R. 


# Introduction
# Like the Human Mortality Database (HMD), the Human Fertility Database (HFD) offers the 
# option to download all available data as a zipped file. Unzippiing this file 
# produces a complex directory structure containing a large number of files. Most, but not all
# of these files contain the fertility data, but all of the files containing data also contain
# some metadata, such as a description of the variables and date the data were produced. Additionaly
# some of the files include variables that can be meaningfully combined with other variables in other 
# files, whereas other files do not. 

# The benefits of identifying and combining variables from different datasets are that they allow 
# more complex analyses and comparisons between groups, in particular countries, to be performed 
# more easily. However, combining files manually can be a time consuming task.

# This paper introduces two function that, given the location of the unzipped HFD and HMD directories,
# collects and merges data from a range of separate files and places them in a single dataframe. 
# Doing this means it becomes much easier to work with such data within R and produce a large number 
# of analyses quickly. 

# Description of HFD and HMD data structures 

# The Human Fertility Database
# The bulk download option from the HFD downloads a zipped file which has the following 
# file structure:
## Illustration here

# The directory has been moved into the subdirectory data/raw_data/hfd within our R 
# directory. 

# The Human Mortality Database
# The bulk download option from the HMD downloads a much larger file [DETAILS], which 
# when unzipped reveals a much more complex directory and file structure. 
# Each country's data are stored in a separate subdirectory, labelled with the 
# country code of that country. Within each country directory, the data files
# tend to have the same name, and so both the directory name and the file 
# name are needed in order to distinguish between datasets. Not all directories 
# contain exactly the same files, and the range of years reported in each 
# year vary. 

# Functions for re-arranging HFD and HMD data. 

# This technical report introduces some functions that automatically combine
# data from a larger number of separate files from the HMD and HFD into 
# just two files: an HMD data file and an HFD data file. In both cases, 
# the functions require that the user has unzipped the HMD and HFD files described 
# previously. The functions require that the user enter the 'root' directory
# for the unzipped HFD and HMD directories, but otherwise is automated. It should
# be noted, however, that the functions require that the structure, format and 
# contents of the HFD and HMD directories do not change: the functions work with 
# [VERSION NUMBER OF CURRENT HFD AND HMD]. 

# Loading the functions

# The functions can be loaded using the source command. The functions make use 
# of a number of data management packages: tidyr, stringr, plyr and dplyr. 
# If these packages have not already been installed then they will need to be 
# using the commands install.packages("tidyr"); install.packages("stringr"); 
# install.packages("plyr"); and install.packages("dplyr").


source("scripts/tidy_data_functions.r")
# Use of the HFD database

# The function hfd_compile compiles four variables from the HFD:

# asfrRR : period fertility rates by calendar year and age (Lexis squares)
# birthsRR : Live births by calendar year and age (Lexis squares)
# cpfr: Cumulative period fertility rates (Lexis squares)
# exposRR : Female population exposure by calendar year and age (Lexis squares)

# To use it the base directory, i.e. the directory containing the unzipped HFD database,
# needs to be specified as the argument to the function

hfd_data <- hfd_compile("data/raw_data/hfd")

write.csv(
  hfd_data,
  file="data/tidy/hfd/lexis_square_combined.csv",
  row.names=F  
)

# Use of the HMD database

# Like the hfd_compile function, the hmd_compile function simply requires the 
# base directory for the HMD database be specified as an input. 

hmd_data <- hmd_compile("data/raw_data/hmd")

write.csv(
  hmd_data,
  file="data/tidy/hmd/lexis_square_combined.csv",
  row.names=F
)



# Discussion 

# This technical document has illustrated how R packages like plyr, tidyr and 
# dplyr can help with batch data processing operations, extracting relevant data 
# located in multiple files in multiple locations and joining them into a single 
# file. With data in the format produced a number of additional analyses become 
# much more straightforward. A later technical document will illustrate how the 
# data produced in this technical document can be used to batch generate a large
# number of data visualisations, as well as other bespoke analyses.

