# 3D visualisation and 3D printing of HFD and HMD data using R 

# Introduction 

# Lexis surfaces are three dimensional structures, in which the value of some 
# demographic variable, such as a fertility rate, mortality rate or population size,
# is presented for a large number of combinations of relative time (usually age)
# and absolute time (usually year). Relative and absolute time can therefore 
# be thought of as two orthogonal axes defining the position across a surface, 
# like latitude and longitude in a map of space, and the value of the demographic
# variable at different age-year combinations can be thought of as variations of 
# this surface's height. 

# By using R it is possible to visualise these virtual three dimensional surfaces. 
# This technical document will show two approaches to producing such surfaces: firstly
# using the rgl package in R, which creates computer generated and interactive surface 
# plots of three dimensional data; and secondly by using R to produce 
# stereolithography (STL) files, which can then be used in computer-aided manufacture
# (CAM) to produce 3D printed 'data sculptures'. 

# This technical document follows from two previous document: XXXX, which describes 
# how to combine and restructure a large amount of data from the HMD and HFD into 
# a smaller number of datasets that use a 'tidy data' structure; and YYYY, which demonstrates 
# how to use these derived datasets for data exploration and  the batch production 
# of image and text files. Many of the methods and concepts developed in these 
# documents are used within this technical document as well, and so readers 
# are encouraged to read those documents first. 

# The R session

# To follow this exercise we assume that R has been set up as in YYYY, with the base R
# directory containing a directory called data, which itself contains a directoy called 
# tidy, and which contains two directories, 'hfd' and 'hmd'. The hfd directory contains 
# the combined and 'tidy' data produced in XXXX from the HFD, and the HMD directory 
# contains the equivalent data produced in XXXX from the HMD. As before, if a needed 
# R package has not been installed it will need to be using the function install.packages

# We start by clearing the R workspace, loading the required packages, and loading the 
# tidied datasets 

rm(list=ls())


require(plyr)
require(tidyr)
require(dplyr)

require(rgl)
require(r2stl)

data_hfd <- read.csv("data/tidy/hfd/lexis_square_combined.csv") %>%
  tbl_df

data_hmd <- read.csv("data/tidy/hmd/lexis_square_combined.csv") %>%
  tbl_df

data_hfd <- data_hfd %>%
  rename(country=code)

data_hfd$country <- data_hfd$country %>% tolower


# rgl

# Rgl is an R package that allows R make use of OpenGL, a cross-platform programming 
# interface for producing 3D images. Images produced using RGL can be interacted with
# using a mouse and allow fine-grained control of graphical defaults


matrx <- data_hmd %>%
  filter(country=="usa" & sex=="male" & age <=80) %>%
  select(year, age, population_count) %>%
  spread(key=age, value=population_count) 

rownames(matrx) <- matrx$year
matrx$year <- NULL
matrx <- as.matrix(matrx)

