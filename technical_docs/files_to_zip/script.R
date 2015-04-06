# Dr Jon Minton
# University of Glasgow
# 5/4/2015

# Script file with examples for merging and batch processing of 
# data from the HFD and HMD


# Pre-requisites

# To start with, please ensure the working directory of the R session is the 
# base directory of the zipped file you have downloaded, after this 
# zipped file has been unzipped. Once unzipped, the base dir should contain
# this file, 'script.R', and the folders 'data', 'figures', and 'scripts'.
# You can check the base directory using 
getwd()

# you can set the base directory using (for example):

#setwd("E:/repos/human_fertility_database/technical_docs/files_to_zip/")


#Starting off

# To begin, clear the workspace and load the functions in functions.r. 
# (Note, this file also loads the required packages)

rm(list=ls())
source("scripts/functions.R")

# To see the functions that have been loaded, use
ls.str()

# Please ensure the following R functions are listed
# merge_lexis_square_hfd
# merge_lexis_square_hmd

# generate_scp_asfr
# generate_scp_population
# generate_scp_logmort



# Part 1: Creating the derived datasets -------------------------------------------

dta_hfd <- merge_lexis_square_hfd("data/raw_data/hfd/")

dta_hmd <- merge_lexis_square_hmd("data/raw_data/hmd/")

# Save the above using the write.csv function as follows

write.csv(dta_hfd, file="data/derived_data/hfd_lexis_square.csv", row.names=F)
write.csv(dta_hmd, file="data/derived_data/hmd_lexis_square.csv", row.names=F)


# Part 2: Generating outputs based on the derived datasets

# To start with, assume that the derived datasets have been generated in a previous session.
# To simuate this, first clear all data from the workspace, and load the functions again

rm(list=ls())

source("scripts/functions.R")

dta_hfd <- read.csv("data/derived_data/hfd_lexis_square.csv")
dta_hmd <- read.csv("data/derived_data/hmd_lexis_square.csv")


# The function that generates figures based on the HFD dataset is 
# generate_scp_asfr.
# The functions that generate figures based on the HMD dataset are 
# generate_scp_population and generate_scp_logmort

generate_scp_asfr(dta_hfd)
generate_scp_population(dta_hmd)
generate_scp_logmort(dta_hmd)



#  Part 3: Using the complete HFD and HMD datasets ------------------------

#To make best use of the functions provided please download the full datasets 
# from the HFD and HMD websites and places them in the data/raw_data/ directories.
# This will then allow data from a much larger range of countries to be combined 
# from the HMD, and a much larger range of shaded contour plots to be generated. 


# Part 4: Going further ---------------------------------------------------

#Please explore the contents of the functions, paying attention to how they make 
# use of the plyr and dplyr packages, and how their contents could be used as the blueprint
# for performing a much wider range of batch merging and batch processing operations 
# with the data. 
# Please also visit the website
# https://github.com/JonMinton/human_fertility_database/ 
# for additional examples and code, and feel free to contact me 
# if you have any further questions or suggestions 
# jonathan.minton@glasgow.ac.uk 
# nate.minton@gmail.com

