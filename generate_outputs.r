# Automate production of outputs

# Examples from HFD

# For each country, produce 
# 1 A shaded contour plot
# 2 stl images
# 3 bathtub curves by year and cohort
# 4 difference from average for all available data
# 5 male/female ratio

rm(list=ls())

require(stringr)
require(tidyr)
require(plyr)
require(dplyr)
require(r2stl)
require(lattice)
require(ggplot2)


hfd_data <- read.csv("data/derived_data/hfd_lexis_squares.csv") %>%
  tbl_df
hmd_data <- read.csv("data/derived_data/hmd_lexis_squares.csv") %>%
  tbl_df

hmd_data$Age <- hmd_data$Age %>%
  mapvalues(from="110+", to="110") %>%
  as.character %>%
  as.numeric
# HMD -
# change Age to numeric



# produce 

# An effective command for the batch production of images and other files is the 
# d_ply command. This expects a dataframe as an input and returns no output. 
# (From the perspective of R, files are generated as 'side effects' of R operations, and are not 
# considered outputs as they do not create new objects within the workspace.)


# period fertility rates

asfr_birth_order <- hfd_data %>%
  select(Code, Year, Age, ASFR1, ASFR2, ASFR3, ASFR4, ASFR5p) %>%
  gather(key=birth_order, value=asfr, -Code, -Year, -Age)  %>%
  filter(!is.na(asfr)) 

asfr_birth_order$birth_order <- asfr_birth_order$birth_order %>%
  str_replace_all("[A-Z]*", "") %>%
  str_replace_all("[a-z]*", "") 

class(asfr_birth_order$Year) <- "numeric"
class(asfr_birth_order$Age) <- "numeric"
class(asfr_birth_order$birth_order) <- "numeric"

asfr_birth_order %>%
  filter(Code=="AUT" & birth_order==1) %>%
  ggplot(aes(x=Year, y=asfr)) +
  geom_line()



