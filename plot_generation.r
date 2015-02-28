# Batch production of small multiples and shaded contour plots 
# using joined data from the Human Mortality Database and Human
# Fertility Database. 


# Introduction

# An earlier technical document described how a range of R 
# packages including plyr, tidyr and dplyr can be used to easily 
# extract data from a large number of separate files and combine
# them into a smaller number of files which follow tidy data conventions. 
# The purpose of this document is to illustrate how, once dat have been 
# produced in this format, the production of large numbers of outputs based
# on the data can be automated. 

# Within this technical document, we will describe an R session in which 
# the data produced in the previous technical document are used to 
# produce two types of visualisation for each of the countries: 
# 1) small multiples, also known as trellis or lattice plots. These will be 
# produced using the ggplot2 package.  
# Shaded contour plots, which present values on a Lexis surface, as 
# a function of age (relative time) and year (absolute time). These will be 
# produced using the lattice package. For more information about shaded contour 
# plots and how to interpret them please read XXXXX. 


# Set up 

# We assume that the base directory contains a directory called 'data',
# which contains a subdirectory called tidy, and that this subdirectory contains
# the two subdirectories 'hmd' and 'hfd'. Within each of these subdirectories
# are the combined and tidied datasets produced as a result of the earlier technical 
# document. 

# We begin the R session by removing any existing R objects from the workspace and 
# loading the required packages. If any packages have not been loaded previously 
# they can be installed using the install.packages function. 

rm(list=ls())

#Data manipulation
require(tidyr)
require(stringr)
require(plyr)
require(dplyr)

#Data visualisation
require(lattice)
require(ggplot2)


# We can now load the datasets as follows. By using the tbl_df function
# from dplyr we can set the default display options to make the 
# displays more user friendly. For further details see XXX. 

data_hfd <- read.csv("data/tidy/hfd/lexis_square_combined.csv") %>%
  tbl_df

data_hmd <- read.csv("data/tidy/hmd/lexis_square_combined.csv") %>%
  tbl_df


# For consistency we will also rename 'code' as 'country' in data_hfd,
# and coerce all country labels to lowercase

data_hfd <- data_hfd %>%
  rename(country=code)

data_hfd$country <- data_hfd$country %>% tolower





#
#
# Data management examples

# We will start by looking at how, with the data in the format above, 
# we can explore data from many countries easily. We can also generate
# derived variables simply. These examples will make extensive use of 
# dplyr functions, including the pipe %>% operator. For further details 
# see XXX. 

## Identifying the first and last year data are available by country in the HFD

data_hfd %>%
  group_by(country) %>%
  summarise(min_year=min(year), max_year=max(year)) %>%
  print(n=31)

## Identifying the first and last year data are available for by country in the HMD

data_hmd %>%
  group_by(country) %>%
  summarise(min_year=min(year), max_year=max(year)) 


# We can produce derived variables very easily using the mutate function in dplyr

data_hfd <- data_hfd %>%
  mutate(birth_rate=total/exposure)

data_hmd <- data_hmd %>%
  mutate(death_rate=death_count/population_count)


# It is also quite straightforward to combine the data dataframes as they use common 
# variable names. For example, we could work out the ratio of the birth rate to 
# the death rate as follows

data_hfd %>%
  inner_join(data_hmd) %>%
  mutate(rr=(death_count/population_count)/(total/exposure)) %>%
  select(country, year, age, rr)

# The exposure and population count values are different. We can explore the size 
# of these differences as follows:

data_hfd %>%
  inner_join(data_hmd) %>%
  mutate(dif=exposure - population_count, prop_dif=dif/population_count) %>%
  select(country, age, year, exposure, population_count, dif, prop_dif) %>%
  arrange(prop_dif)

data_hfd %>%
  inner_join(data_hmd) %>%
  mutate(dif=exposure - population_count, prop_dif=dif/population_count) %>%
  select(country, age, year, exposure, population_count, dif, prop_dif) %>%
  arrange(desc(prop_dif))


# There are two things to note in the above examples. Firstly the variable prop_dif 
# depends on the variable dif, which was created in the previous line. 
# Secondly, arrange(prop_dif) arranges the rows from lowest to highest value of prop_desc;
# enclosing prop_dif in desc instead sorts the rows from highest to lowest. 


# Data reshaping is also quite straightforward. To calculate the male to female
# population ratio, for example, we can use the spread command from the tidyr package

data_hmd %>%
  select(country, year, age, sex, population_count) %>%
  spread(sex, population_count) %>%
  mutate(sex_ratio= female/male) %>%
  select(country, year, age, sex_ratio)



# Bathtub curves 

# We can plot the relationship between mortality ratio and age over time for a range of 
# countries using ggplot2. 

