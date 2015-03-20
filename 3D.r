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
# using a mouse and allow fine-grained control of graphical defaults.
# The following shows log mortality and population sizes for males and 
# females as four linked images, such that mouse commands will zoom and rotate all 
# images at the same time.


mfrow3d(2,2, sharedMouse=TRUE)

hmd_usa <- data_hmd %>%
  filter(country=="usa" & age <=80) %>%
  mutate(death_rate=death_count / population_count, ldeath_rate=log(death_rate))


matrx_mp <- hmd_usa %>%
  filter(country=="usa" & sex=="male" & age <=80) %>%
  select(year, age, population_count) %>%
  spread(key=age, value=population_count) 

rownames(matrx_mp) <- matrx_mp$year
matrx_mp$year <- NULL
matrx_mp <- as.matrix(matrx_mp)


mtlst <- list(
  x=as.numeric(rownames(matrx_mp)),
  y=as.numeric(colnames(matrx_mp)),
  z=matrx_mp
)


persp3d(
   x=mtlst$x, 
   y=mtlst$y, 
   z=mtlst$z, 
   col="lightblue", 
   xlab="", 
   ylab="", 
   zlab="", 
   box=F, axes=F 
   )

next3d()

matrx_fp <- hmd_usa %>%
  filter(country=="usa" & sex=="female" & age <=80) %>%
  select(year, age, population_count) %>%
  spread(key=age, value=population_count) 

rownames(matrx_fp) <- matrx_fp$year
matrx_fp$year <- NULL
matrx_fp <- as.matrix(matrx_fp)


mtlst <- list(
  x=as.numeric(rownames(matrx_fp)),
  y=as.numeric(colnames(matrx_fp)),
  z=matrx_fp
)

persp3d(
  x=mtlst$x, 
  y=mtlst$y, 
  z=mtlst$z, 
  col="red", 
  xlab="", 
  ylab="", 
  zlab="", 
  box=F, axes=F 
)


next3d()

matrx_md <- hmd_usa %>%
  filter(country=="usa" & sex=="male" & age <=80) %>%
  select(year, age, ldeath_rate) %>%
  spread(key=age, value=ldeath_rate) 

rownames(matrx_md) <- matrx_md$year
matrx_md$year <- NULL
matrx_md <- as.matrix(matrx_md)


mtlst <- list(
  x=as.numeric(rownames(matrx_md)),
  y=as.numeric(colnames(matrx_md)),
  z=matrx_md
)

persp3d(
  x=mtlst$x, 
  y=mtlst$y, 
  z=mtlst$z, 
  col="darkblue", 
  xlab="", 
  ylab="", 
  zlab="", 
  box=F, axes=F
)

next3d()

matrx_fd <- hmd_usa %>%
  filter(country=="usa" & sex=="female" & age <=80) %>%
  select(year, age, ldeath_rate) %>%
  spread(key=age, value=ldeath_rate) 

rownames(matrx_fd) <- matrx_fd$year
matrx_fd$year <- NULL
matrx_fd <- as.matrix(matrx_fd)


mtlst <- list(
  x=as.numeric(rownames(matrx_fd)),
  y=as.numeric(colnames(matrx_fd)),
  z=matrx_fd
)

persp3d(
  x=mtlst$x, 
  y=mtlst$y, 
  z=mtlst$z, 
  col="darkred", 
  xlab="", 
  ylab="", 
  zlab="", 
  box=F, axes=F 
)


# 3D printing

# The R package r2stl can be used to generate stereolithography (STL) files. 
# This is a standard file format within computer-aided manufacturer, including
# for the production of 3D printing. (Note: Rgl has its own STL writing capabilities). 
# By using the plyr package it is possible to automate the production of STL 
# files for a wide range of demographic variables. The methods and principles 
# are described in more detail in YYYY. 

# The example below will generate STL files from HFD data, plotting ASFR for 
# each country in the dataset. As before, the principles are generalisable
# to other variables. 

dir.create("stl/hfd/", recursive=T)
fn <- function(x){
  
  this_country <- x$country[1]
  
  mtrx <- x %>%
    select(year, age, asfr) %>%
    mutate(asfr=asfr+0.02 * max(asfr)) %>% # Add 2% to ASFR values as a minimum height is needed
    spread(key=age, value=asfr) 
  
  years <- mtrx$year
  min_year <- min(years)
  max_year <- max(years)
  
  mtrx$year <- NULL
  
  mtrx <- as.matrix(mtrx)
  rownames(mtrx) <- years
  
  
  r2stl(
    x=as.numeric(rownames(mtrx)),
    y=as.numeric(colnames(mtrx)),
    z=mtrx,
  
    filename=paste0("stl/hfd/", this_country, "_(", min_year, "_", max_year, ")_asfr.stl"),
    z.expand=T,
    show.persp=F
  )
}


d_ply(
  data_hfd, 
  .(country),
  fn,
  .progress="text"
)

## Viewing stl files

# The r2stl function includes an option show.persp which renders the 
# stl file using the perspective function in R. This is useful when testing a 
# single image, but not when producting a large number of files. 
# The rgl package includes the function readSTL. STL files can then be visualised
# using the rgl functions described above. 
# Outside of the R environment, a large number of free viewers are available to view
# and manipulate STL images. One example of this is Meshlab, available from [REFERENCE]; 
# below are some examples of the STL files generated earlier as rendered in Meshlab.

# EXAMPLES HERE 


## Github as an STL viewer

# Github, the online repository and version control system, allows STL files to 
# be stored within its online repositories. By default, any STL files are automatically
# rendered as 3D and interactive images. Some examples are shown below

# EXAMPLES HERE

# By pressing the 'raw' button when viewing an STL file within github, the raw data becomes 
# visible. This shows that STL files are text files rather than binary, comprising the 
# specifications of large numbers of vertices. The first few lines of such a file are shown
# below. 



# Discussion

# This technical document, the third in a series, has shown how R's rgl and r2stl packages,
# respectively, allow inherently three dimensional demographic data to be presented as such,
# either on screen using the 3D graphics capabilities of openGL, or as physical objects 
# using 3D printers. Some examples of 3D printed demographic data, in this case population
# sizes and log(mortality) rates in Scotland, are shown below.

# [FINAL EXAMPLES]
#
