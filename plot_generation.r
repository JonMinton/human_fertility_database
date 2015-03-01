# Exploring and batch generation of graphical outputs using 'tidy' HMD and 
# HFD datasets. Examples using plyr and dplyr.


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
# produce two types of visualisation: 
# 1) small multiples, also known as trellis or lattice plots. These will be 
# produced using the ggplot2 package.  
# 2) Shaded contour plots, which present values on a Lexis surface, as 
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

# To start let's see the full list of countries with records dating back to at least 1900

data_hmd %>%
  group_by(country) %>%
  summarise(min_year=min(year), max_year=max(year)) %>%
  filter(min_year <=1900) %>%
  arrange(min_year)

# We can extract this list of countries as follows
countries_of_interest <- data_hmd %>%
  group_by(country) %>%
  summarise(min_year=min(year), max_year=max(year)) %>%
  filter(min_year <=1900) %>%
  arrange(min_year) %>%
  .$country %>%
  as.character

# As we know some of the countries are not mutually exclusive we can 
# remove them. 

countries_of_interest <-countries_of_interest[!countries_of_interest %in% c("fracnp", "gbrcenw")]

# We can use this to filter the data before passing to ggplot2

data_hmd %>%
  group_by(country, sex) %>%
  filter(country %in% countries_of_interest & sex!="total") %>%
  mutate(birth_year =year -age, death_rate=death_count/ population_count) %>%
  filter(birth_year %in% seq(from=1860, to=1940, by=20))
  
# This output can then be passed to the ggplot function within ggplot2. We can 
# then add additional information about the graphic we want to produce 
# using the '+' operator

data_hmd %>%
  group_by(country, sex) %>%
  filter(country %in% countries_of_interest & sex!="total") %>%
  mutate(birth_year =year -age, death_rate=death_count/ population_count) %>%
  filter(birth_year %in% seq(from=1860, to=1940, by=20)) %>%
  filter(age <=90) %>%
  arrange(country, sex, birth_year, age) %>%
  ggplot() + 
  geom_line(
    mapping=aes(x=age, y=death_rate, group=birth_year, colour=as.factor(birth_year))
    ) +
  scale_y_log10() +
  facet_grid(country ~ sex) +
  labs(x="Age", y="Death rate") +
  scale_colour_discrete(name="Birth year")

# The modular and layered qualities of ggplot2 mean it is relatively easy to 
# produce permutations of the above.

data_hmd %>%
  group_by(country, sex) %>%
  filter(country %in% countries_of_interest & sex!="total") %>%
  mutate(birth_year =year -age, death_rate=death_count/ population_count) %>%
  filter(birth_year %in% seq(from=1860, to=1940, by=20)) %>%
  filter(age <=90) %>%
  arrange(country, sex, birth_year, age) %>%
  ggplot() + 
  geom_line(
    mapping=aes(x=age, y=death_rate, group=sex, colour=sex)
  ) +
  scale_y_log10() +
  facet_grid(country ~ birth_year) +
  labs(x="Age", y="Death rate") +
  scale_colour_discrete(name="Sex")



# Shaded contour plots 

# Shaded contour plots combine both contour plots and level plots, using 
# both shade and contour lines to indicate how the value of something varies 
# as a function of both age and year. This allows thousands of data points to 
# be visualised in a single image.

# Within R, it is easier to use the Lattice package rather than the ggplot2
# package to produce shaded contour plots, although they are possible to 
# produce using ggplot2 or base graphics. Lattice is an alternative high level 
# data visualisation package to ggplot2, based on the Trellis package within 
# S. 


data_hmd %>%
  filter(country=="usa" & sex !="total" & age <=90) %>%
  mutate(death_rate=death_count/population_count)  %>%  
  contourplot(log(death_rate) ~ year * age |  sex, 
              data=.,
              region=TRUE,
              col.regions=rev(heat.colors(200)),
              cuts=50
              ) 

# contourplot is a lattice function. It takes as its first argument a formula.
#  log(death_rate) ~ year * age  | sex, stating that log(death_rate) should be 
# shown against year on one axis and age on the other. The | symbol indicates 
# a conditioning variable: the formula to the left should be run separately 
# for both sexes. This is equivalent to the faceting operations in ggplot2, and 
# is a core feature of Trellis/Lattice graphics. 

# An important thing to note is the . symbol. This indicates that the output from the 
# previous lines of code should be the input to this argument, rather than the 
# function's first argument. This symbol only needs to be used where the 
# first argument to a function is not the correct location for the result 
# of the earlier series of %>% operations. As most dplyr and tidyr functions, and 
# the ggplot function within ggplot2, take the data as the first argument the
# . symbol is not needed in these cases. 


# Automating the generation of image and text files 

# In a previous technical report we showed how to use the plyr package to combine many
# files into a single HFD dataset and a single HMD dataset; these are the datasets we have 
# used within all of the examples above. In this section of this technical report we 
# will show how to use the plyr package to automate the production of a large number of 
# graphs. The principles are generalisable to other kinds of automated file production, and 
# after the main examples using shaded contour plots an example involving the production of many
# text files will be presented. 

## A note: d_ply and file production as a 'side effect'

# Within the data tidying example in the previous technical report the workhorse function 
# was ldply, which took a list as an input and returned a dataframe as an output. When producing
# many file outputs the equivalent function to use is d_ply. This function takes 
# a dataframe (d) as an input, splits it up according to an argument to the function,
# and then produces no output (_) . This might seem counterintuitive, as of course
# the function generates many outputs, i.e. the image files or otherwise. 
# However, inputs and outputs in the context of plyr refer to those that exist
# in R's workspace, not those on the computer's file system. Certain R functions,
# such as read.table and write.table, interact with files, but from the perspective 
# of the R programming language these file are 'side effects' of the functions' operation.


## Shaded contour plots of population sizes.

# To produce shaded contour maps that show the changes in population size over
# time we can do the following:

dir.create("figures/population/", recursive=T)

fn <- function(x){
  this_country <- x$country[1] # All entries should be the same, but 
  # the output should only be of length 1. 
  
  dir_location <- "figures/population/"
  
  tiff(paste0(dir_location, this_country, "_population.tiff"), width=2000, height=1000)
  
  min_year = min(x$year)
  max_year = max(x$year)
  
  title <- paste0(toupper(this_country), ", population sizes\n", min_year, " to ", max_year)
  
  x %>%
    filter(age <=90 & sex!="total") %>%
    contourplot(
      population_count ~ year * age | sex, 
      data= .,
      region=T,
      col.regions=rev(heat.colors(50)),
      cuts=20,
      main=title
    ) %>%
    print
  
  dev.off()
  
  return(NULL)
}

d_ply(data_hmd, .(country), fn, .progress="text")

# The second argument of the plyr function defines the variable used to split the 
# dataset into pieces. This is the country variable. If the split were to be made
# according to a combination of two or more variables then this can be achieved 
# by passing multiple names to the argument; for example to split by both country 
# and gender the statement dply(data_hmd, .(country, sex), fn) could be used, although
# the function would have to be changed to allow appropriate labelling by sex as 
# well as country. 

# The function itself opens a TIFF graphics device, generates the file name 
# from the country name, and generates a title for the plot that defines the range of 
# years for which the data are available for that country. 
# The contents of the call to contourplot are passed to the print function. When lattice
# objects are created inside a loop or other function then they need to be called 
# explicitly in this way. Finally, dev.off() closes the graphics device.

dir.create("figures/hfd/asfr/", recursive=T)

fn <- function(x){
  this_country <- x$country[1] # All entries should be the same, but 
  # the output should only be of length 1. 
  
  dir_location <- "figures/hfd/asfr/"
  
  tiff(paste0(dir_location, this_country, "_asfr.tiff"), width=1000, height=1000)
  
  min_year = min(x$year)
  max_year = max(x$year)
  
  title <- paste0(toupper(this_country), ", Fertility Rates\n", min_year, " to ", max_year)
  
  x %>%
    filter(age <=50) %>%
    contourplot(
      asfr ~ year * age, 
      data= .,
      region=T,
      col.regions=rev(heat.colors(50)),
      cuts=20,
      main=title
    ) %>%
    print
  
  dev.off()
  
  return(NULL)
}

d_ply(data_hfd, .(country), fn, .progress="text")

# Structurally very little is different in the code above compared with the 
# population example. Reference to sex has been removed as no faceting is needed;
# this also means the plot size does not have to be as wide, and so width has been
# reduced from 2000 to 1000 pixels. Otherwise, the code is almost identical to the 
# previous example.

## Automating the production of files

# Text files can be written using functions like write.table and write.csv. As not 
# all analysts use R or other software that make it easy to automate data manipulation
# processes it can be helpful to use R to generate text outputs that can be used
# with little further processing within other software. As an example, the code below
# first calculates the population sex ratio for each age and year, then re-arranges 
# the data as a matrix, with each row a different year and each column a different year.
# It then saves these matrices as a separate text file for each country, with each 
# file labelled by the country code. 

dir.create("data/derived/hmd/sex_ratio_matrices/", recursive=TRUE)

fn <- function(x){
  this_country <- x$country[1]
  
  data_as_matrix <- x %>%
    select(-country) %>%
    spread(key=age, value=sex_ratio) 
  
  years <- data_as_matrix$year
  data_as_matrix$year <- NULL
  data_as_matrix <- as.matrix(data_as_matrix)
  rownames(data_as_matrix) <- years
  
  write.csv(
    data_as_matrix,
    file=paste0("data/derived/hmd/sex_ratio_matrices/", this_country, "_sex_ratio.csv")
    )
    
}

data_hmd %>%
  select(country, year, age, sex, population_count) %>%
  filter(age <= 90) %>%
  spread(key=sex, value=population_count) %>%
  mutate(sex_ratio = male/female) %>%
  select(country, year, age, sex_ratio) %>%
  d_ply(.(country), fn, .progress="text")
  


# Discussion

# This technical document has shown how, once data have been combined in a consistent
# and 'tidy' format, large amounts of data from the HMD and HFD can be explored, 
# re-arranged, and used to generate large numbers of outputs very quickly and easily. 
# The examples given above are just a small a small sample of the large number of 
# analyses and outputs that could be generated. By making the effort to combine data 
# in the way described in the earlier technical document additional analyses become 
# much easier and less piecemeal. 





