
# HFD ---------------------------------------------------------------------

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

# This paper presents R code that automates the merging of particular variables from many files to 
# a single dataset. The first section will discuss the simpler case of combining XX, YY and ZZ 
# into a single file from the HFD. Then, we will look at 
# the more structurally complex example of extracting and merging data from 
# the HMD.

# To help perform the merging tasks necessary, the  paper will make use of the 
# plyr and dplyr packages produced by Hadley Wickham, which implement the 
# Split-Apply-Combine paradigm discussed in XXXX. To help decide on the desired
# output structure for the merged data, we will take on board the suggestions 
# made by Wickham about 'Tidy Data' formats, the benefits of having data in 
# such a format, and the data management and processing challenges involved 
# in getting data into such a format. 

# In all cases it is assumed that the base directory of the R session contains a 
# directory called `data`, and that this directory contains [further details here].

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



# Exploring and processing the HFD 

# We the session by removing any pre-existing objects from the R workspace
# and then loading a number of R packages that make the process of data 
# tidying and batch processing easier. If these packages are not already installed
# use install.packages([PACKAGE_NAME]) in order to do so. 
rm(list=ls())

require(tidyr)
require(stringr)
require(plyr)
require(dplyr)

# With the HFD files loaded in the specified directory, we can extract a 
# list of the files that it contains as follows

file_names <- list.files(
  "data/raw_data/hfd/Files/zip_w/"
  )

# By opening up a small selection of these data files, we can see that they
# follow a consistent format. The first two lines of each file contain 
# metadata, with the first line containing a description of the dataset
# and the second line stating when the file was last modified. The third line
# contains the header names, and the data itself begins at line four. 

# Knowing this file structure, we can construct a simple function that will 
# extract this metadata from each file by reading the first few lines:

fn <- function(x){
  inputs <- readLines(
    paste("data/raw_data/hfd/Files/zip_w", x, sep="/"),
    n=3
    )
    
  out <- data.frame(
    file_name=x, 
    description=inputs[1],
    variables=inputs[3]
    )
  return(out)
}

# This function fn is applied for each of the files in file_names using 
# the plyr function ldply. ldply expects a list as an input and returns 
# a dataframe as an output. Its input, file_names, is therefore coerced 
# into a list, before each element of the list, i.e. each separate file 
#name, is passed to fn. The output from each call to fn is then passed back
# and then combined back into a list. 
summaries <- ldply(file_names, fn)

# By displaying summaries we can now see, for each file, the description
# of the data provided in the variable, and a list of the variables 
# that file contains
head(summaries)


# We can also use the contents of summaries in order to identify particular
# subsets of the data. For example, the files that contain the phrase 
# 'Lexis Square' within their description can be identified as follows:
summaries$file_name[str_detect(summaries$description, "Lexis square")]

# Similarly, the files that contain the variable 'TFR' (but 
# not TFR0, TFR1 and so on), can be identified as follows:

summaries$file_name[
  str_detect(summaries$variables, " TFR ")
    ]

##

# In this example, we will look to combine a small selection of files
# asfrRR : period fertility rates by calendar year and age (Lexis squares)
# birthsRR : Live births by calendar year and age (Lexis squares)
# cpfr: Cumulative period fertility rates (Lexis squares)
# exposRR : Female population exposure by calendar year and age (Lexis squares)

# These variables were selected because they are meaningful to combine, as each 
# variable is reported as a Lexis square, and disaggregated by Code (country), 
# age and year. Because just a small number of variables are to be combined, the 
# process of loading and joining the data will be done manually rather than within 
# a function. However if there were more variables to combine then use of 
# plyr to automate the process would be recommended. 


data_asfr <- read.table(
  file="data/raw_data/hfd/Files/zip_w/asfrRR.txt",
  header=T,
  skip=2,
  na.strings="."
)

# For typing convenience, it is useful to convert all variable names to 
# lowercase. 
names(data_asfr) <- tolower(names(data_asfr))

# To see the class that each variable has been converted to, we can use the 
# apply function to apply class to each column. 

apply(data_asfr, 2, class)

# This shows that Year, Age, and ASFR are stored as character variables, although
# they are numeric. 
# In the case of year and asfr, it is not problematic to change the class 
# using as.numeric. However as Age contains "12-" to represent 12 or under, 
# and "55+" to represent 55 or older, these non-numeric characters need to be 
# removed in order for the type conversion to be successful. A more explicit way
# of removing these characters would be with the revalue function within plyr



age_1 <- revalue(data_asfr$age, replace=c("12-" = "12", "55+" = "55"))
age_1 <- as.numeric(as.character(age_1))

# An alternative approach would be to use stringr's str_replace function
age_2 <- str_replace(data_asfr$age, "[+]|[-]", "")
age_2 <- as.numeric(as.character(age_2))

all.equal(data_asfr$age_1, data_asfr$age_2)

data_asfr$age <- age_1
rm(age_1, age_2)


# We can now do the same with the other files

data_births <- read.table(
    file="data/raw_data/hfd/Files/zip_w/birthsRR.txt",
    header=T,
    skip=2
)

names(data_births) <- tolower(names(data_births))
data_births$age <- revalue(data_births$age, replace=c("12-" = "12", "55+" = "55"))
data_births$age <- as.numeric(as.character(data_births$age))

data_cpfr <- read.table(
  file="data/raw_data/hfd/Files/zip_w/cpfrRR.txt",
  header=T,
  skip=2
)

names(data_cpfr) <- tolower(names(data_cpfr))


data_expos <- read.table(
  file="data/raw_data/hfd/Files/zip_w/exposRR.txt",
  header=T,
  skip=2
)

names(data_expos) <- tolower(names(data_expos))

# If they have consistently labelled common fields then an arbitrarily
# large number of dataframe objects can be joined together using 
# the join function within plyr and the Reduce function in base R. 

data_combined <- Reduce(
  join,
  list(
    data_asfr, 
    data_births, 
    data_cpfr,
    data_expos
  )
)


# The convenience function 'mutate' can be used to add new columns 
# to the combined data that depends on the row-wise values of other 
#columns. For example, to add a row calculating the crude birth rate
#for every year, country and age combination simply write:
data_combined <- mutate(data_combined, birth_rate=total/exposure)

# With the data in this format we can now save the file for later use

write.csv(
  data_combined,
  file="data/tidy/hfd/lexis_square_combined.csv",
  row.names=F  
)


# HMD ---------------------------------------------------------------------

# A more complex example of automated extraction and merging involves 
# extracting population counts and death counts from the HMD. The HMD 
# has a more complex directory structure as well as far more files. 
# In this section we will show how data from multiple directories 
# can be recombined from around a hundred separate files, and labelled
# according to tidy data conventions using country code labels extracted
# from the directory names rather than the contents of the files themselves

# We begin again by removing existing objects from the R workspace, as well
# as explicitly calling the garbage collection routine in order to make 
# sure enough memory has been freed for the operations that follow. Then, 
# we load the requisite libraries
rm(list=ls())
gc()

require(tidyr)
require(stringr)
require(plyr)
require(dplyr)

#The additional library above is dplyr, which is similar to plyr but focused 
# on dataframes. dplyr uses the pipe operator %>% borrowed from the maggritr 
# package. The %>% operator allowed a series of commands to be chained, meaning
# that a series of commands can be written from left to right, rather than 
# inside to outside. This greatly improves the readability of code 
# and so makes code easier to write, maintain and check. For more information 
# about the dplyr package see XXXX. 

# Both plyr and dplyr can be used at the same time, with plyr better for 
# batch processing operations involving reading from and writing to files. 
# However they use a number of the same function names, and so it is 
# recommended that, if using dplyr as well as plyr, dplyr is loaded after 
# plyr so that the more recent dplyr variants of functions mask older plyr
# variants of these functions. 



# With the data in the form specified previously, we can produce a list of all
# subdirectories containing data from individual countries using the list.dirs
# command. As we want to use this later to label the data by country as well 
# as age, year and sex, we set full.names to FALSE, and as we do not 
# want any directories within the directories we also set recursive to false

list_country_dirs <- list.dirs("data/raw_data/hmd", recursive=F, full.names=FALSE)

# This list can now be used as the main input into a function that, for each country
# directory, loads the contents of the files Population.txt and Deaths_1x1.txt within
# each directory, rearranges the data into a tidy data format, merges the data in the 
# new format together with a country code identifier, before passing this merged 
# dataframe as the output. The function below also uses some functions from the tidyr 
# package to reshape the data. 


# An additional complication with data in the HMD is that population sizes are 
# reported twice for countries in years in which the country's territory changed
# In these years, the suffice - is used to denote the population size estimates 
# before the change, and + is used to denote the population sizes estimates after 
# the changes. In order to allow this variable to be converted to a numeric format
# a single population count will be calculated using the average of the two popuation 
# sizes. This will then allow the data to be merged to death count, which are 

# reported for the complete year in all cases. The function below will do this along with 
# other batch processing operations. 

fn <- function(x){
  this_country_name <- x
  this_basedir <- paste("data/raw_data/hmd", this_country_name, "STATS", sep="/")
  
  this_populations <- read.table(
    paste0(this_basedir, "/Population.txt"),
    skip=2, header=TRUE, stringsAsFactors=FALSE, na.strings="."
    ) %>% 
    gather(key="Sex", value="population_count", Female, Male, Total, convert=TRUE) %>%
    filter(!is.na(population_count)) %>%
    mutate(Country=this_country_name) %>%
    select(Country, Year, Age, Sex, population_count)
  
  this_populations$population_count <- as.numeric(as.character(this_populations$population_count))
  if(any(is.na(this_populations$population_count))) browser()
  # condition if any of the Year values end with a - or a + symbol
  pop_plus_minus <- this_populations$Year  %>% as.character  %>% str_detect("[+-]$")
  if (any(pop_plus_minus)){ 
    this_populations$Year <- this_populations$Year  %>%  str_replace("[+-]", "")  %>% as.numeric    
    this_populations <- this_populations %>%
      group_by(Country, Year, Age, Sex) %>%
      summarise(
        population_count=mean(population_count, na.rm=T)
      ) # average of the two
  }
  this_deaths <- read.table(
    paste0(this_basedir, "/Deaths_1x1.txt"),
    skip=2, header=TRUE, stringsAsFactors=FALSE, na.strings="."
  ) %>% 
    gather(key="Sex", value="death_count", Female, Male, Total, convert=TRUE) %>%
    filter(!is.na(death_count)) %>%
    mutate(Country=this_country_name) %>%
    select(Country, Year, Age, Sex, death_count)
    
  output <- this_populations %>%
    inner_join(this_deaths)
  
  return(output)
}

# The :: function is the scope operator. This allows us to specify  which 
# package a function should be sought within. In this case we want to 
# make sure R uses the join function from plyr rather than any identically
# named join function within dplyr

# To iteratively repeatedly call fn above for each country directory, we 
# can use the ldply function in R. To see a progress report we can add 
# the argument .progress="text". This is useful for larger batch operations.

pop_death_combined <- ldply(list_country_dirs, fn, .progress="text")


# As before, for typing convenience we can change the variable 
# names and contents of country to lowercase 

names(pop_death_combined) <- tolower(names(pop_death_combined))

# For illustration, the country codes and sexes can be turned into lowercase
# using the pipe operator as follows:

pop_death_combined$country <- pop_death_combined$country %>% tolower
pop_death_combined$sex <- pop_death_combined$sex %>% tolower
  
# As with ages in the HFD, some of the age values in the age 
# field are not strictly numeric, with 110+ rather than 110. 
# To allow this to be transformed into a numeric vector 
# without creating NAs we can do the following:

pop_death_combined$age <- pop_death_combined$age %>%
  mapvalues(from="110+", to="110") %>%
  as.character %>%
  as.numeric

# The data have now been combined from 92 separate files, located
# in 46 separate directories, and formatted in a consistent, tidy data 
# format. The resulting dataset can now be saved as a comma-separated value
# file for analysis.

write.csv(pop_death_combined, file="data/tidy/hmd/lexis_square_combined.csv", row.names=F)



# Discussion 

# This technical document has illustrated how R packages like plyr, tidyr and 
# dplyr can help with batch data processing operations, extracting relevant data 
# located in multiple files in multiple locations and joining them into a single 
# file. With data in the format produced a number of additional analyses become 
# much more straightforward. A later technical document will illustrate how the 
# data produced in this technical document can be used to batch generate a large
# number of data visualisations, as well as other bespoke analyses.

