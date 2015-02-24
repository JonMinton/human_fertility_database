# the aim of this file is to understand more about the data files 
# from the HFD and to arrange it in a 'tidy' format 

# This can be done in an automated way by noting that the 
# first line of each file contains a description
# and the third line contains the variable names

rm(list=ls())
require(tidyr)
require(stringr)
require(plyr)
require(dplyr)


file_names <- list.files(
  "data/raw_data/hfd/Files/zip_w/"
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

# I'm interested in the tables that are reported by Lexis square. 
file_index <- sapply(summaries, function(x){str_detect(x$desc, "Lexis square")})
files_to_use <- names(summaries[file_index])


# Now to load together these files



fn <- function(x){
  this_file_loc <- paste("data/raw_data/hfd/Files/zip_w", x, sep="/")
  
  this_dta <- read.table(
    file=this_file_loc,
    header=T,
    skip=2,
    na.strings="."
    )
  
  this_dta$Age <- str_replace_all(this_dta$Age, "+-", "")
  this_dta$Age <- as.numeric(as.character(this_dta$Age))
  return(this_dta)
}

list_lex_sq <- llply(files_to_use, fn, .progress="text")

# list_lex_sq is now a list object, one element for each table that has been read in. 
# To be converted to a tidy data format, the values from columns 4 onwards need to be matched to 
# Code, Year, and Age. One (inefficient but comprehensible) way of doing this is as follows:

df_lex_sq <- list_lex_sq[[1]] 

for (i in 2:length(dta_lex_sq)){
  this_df <- list_lex_sq[[i]]
  df_lex_sq <- df_lex_sq   %>% left_join(this_df, by=c("Code", "Year", "Age"))
}

# When working with dataframes it is often easier to use the dplyr package. This package is similar to
# plyr but optimised for dataframes. It also has a slightly different syntactic convention: it encourages 
# the use of the 'pipe' operator %>% from the magrittr package, which makes it much easier to chain operations
# from left to right, making code much easier to understand. Additionally, almost all functions are named as verbs
# which clearly and simply describe the operation performed on the data. 

# To start, convert the dataframe to dplyr's tbl_df class.
df_lex_sq <- df_lex_sq %>%
  tbl_df

# The code above uses the pipe operator. The above is directly equivalent to tbl_df(df_lex_sq). Although 
# the benefits of writing code using a pipe are marginal in this case, when multiple operations are 
# chained the benefits to readability become very large. 

# One benefit of tbl_df object is that the head() function is no longer necessary. Typing df_lex_sq shows 
# only the first few rows, along with the dimensions of the dataframe and additional metadata
df_lex_sq

# We can see that some of the files contain the same variables: ASFR, Total, and CPFR. 
# Because they share the same name in the merged dataset '.x' and '.y' have been appended to them.
# We expect that no observation will have both (say) ASFR.x and ASFR.y, and so if one of the columns 
# has a value then the other does not. We can check this out as follows

df_lex_sq %>%
  mutate(asfr_check = is.na(ASFR.x) | is.na(ASFR.y)) %>%
  select(asfr_check) %>%
  as.vector %>%
  table


# This shows that my assumption was not correct. A number of the rows contain 
# values in both columns. The next aim should therefore be to see if the values in both rows are identical

df_lex_sq %>%
  mutate(asfr_check = is.na(ASFR.x) | is.na(ASFR.y)) %>%
  filter(asfr_check ==FALSE) %>%
  select(ASFR.x, ASFR.y) %>%
  mutate(asfr_identical = ASFR.x==ASFR.y) %>%
  select(asfr_identical) %>%
  as.vector %>%
  table

# In this case all rows evaluate as true, meaning we can collapse both columns to a single column

# Now to evaluate total and cpfr.y in the same way

df_lex_sq %>%
  mutate(total_check = is.na(Total.x) | is.na(Total.y)) %>%
  filter(total_check == FALSE) %>%
  select(Total.x, Total.y) %>%
  mutate(total_identical = Total.x == Total.y) %>%
  select(total_identical) %>%
  as.vector %>%
  table

df_lex_sq %>%
  mutate(cpfr_check = is.na(CPFR.x) | is.na(CPFR.y)) %>%
  filter(cpfr_check == FALSE) %>%
  select(CPFR.x, CPFR.y) %>%
  mutate(cpfr_identical = CPFR.x == CPFR.y) %>%
  select(cpfr_identical) %>%
  as.vector %>%
  table



# We can now create a single variable for ASFR, CPFR and Total, and then remove the duplicated variables

df_lex_sq <- df_lex_sq %>%
  mutate(
    ASFR = ifelse(is.na(ASFR.y), ASFR.x, ASFR.y),
    CPFR = ifelse(is.na(CPFR.y), CPFR.x, CPFR.y),
    Total = ifelse(is.na(Total.y), Total.x, Total.y)
  ) %>%
  select(-ASFR.x, -ASFR.y, -CPFR.x, -CPFR.y, -Total.x, -Total.y) 

# We can check that each combination of Code, Year and Age in each row is unique using the distinct function.

df_lex_sq %>%
  select(Code, Year, Age) %>%
  distinct %>%
  nrow
  
# There are 80124 rows, meaning no duplications. 


# We can now save this this combined dataset for later. 

write.csv(df_lex_sq, "data/derived_data/hfd_lexis_squares.csv", row.names=F)




# HMD ---------------------------------------------------------------------

# We will now look at doing something similar using the HMD

# The HMD is structurally more complex, as well as being longer. To start it will be good to remove existing 
# objects in the workspace to free up memory. 
rm(list=ls())
gc()



# Now to identify the directories named by country

list_country_dirs <- list.dirs("data/raw_data/hmd", recursive=F, full.names=FALSE)


# Similarly, we can extract metadata from each of the datafiles in the STATS subdirectory. To start with, we 
# will just look at the first country directory, AUS

fn <- function(x){
  
  files_in_stats <- list.files(
    paste0("data/raw_data/hmd/", x, "/STATS/"),
    pattern="\\.txt$"
    )
  fn_inner <- function(xx){
    first_three_lines <- readLines(
      paste0("data/raw_data/hmd/", x, "/STATS/", xx),
      n=3
      )
    out <- list(
      desc=first_three_lines[1],
      vars=first_three_lines[3]
    )
    return(out)
  }
  output <- llply(files_in_stats, fn_inner)
  names(output) <- files_in_stats
  return(output)
}

complex_list <- llply(list_country_dirs, fn)
names(complex_list) <- list_country_dirs

#to convert to a matrix, showing the presence or absence of variables by year

countries <- list_country_dirs
vars <- sapply(complex_list, names)

unique_vars <- vars[[1]]

for (i in 2:length(vars)){
  unique_vars <<- c(unique_vars, vars[[i]]) %>%
    unique  
}

fn <- function(x){
  out <- unique_vars %in% names(x)
  return(out)
}

country_var_matrix <- laply(complex_list, fn)
dimnames(country_var_matrix) <- list(names(complex_list), unique_vars)

# To work out how many countries contain each variable
adply(country_var_matrix, 2, function(x) table(x)[["TRUE"]])


# For now, the variables of interest include
#  Population.txt 46
#Deaths_1x1.txt 46

image(country_var_matrix)


# The aim in this example is just to extract and merge the populations.txt and deaths_1x1.txt files from 
# each directory, as well as to label the contents appropriately.

list_country_dirs <- list.dirs("data/raw_data/hmd", recursive=F, full.names=FALSE)


fn <- function(x){
  this_country_name <- x
  this_basedir <- paste("data/raw_data/hmd", this_country_name, "STATS", sep="/")
  
  this_populations <- read.table(
    paste0(this_basedir, "/Population.txt"),
    skip=2, header=TRUE
    )   %>%
    gather(key="Sex", value="population_count", Female, Male, Total) %>%
    mutate(Country=this_country_name) %>%
    select(Country, Year, Age, Sex, population_count)
  
  this_deaths <- read.table(
    paste0(this_basedir, "/Deaths_1x1.txt"),
    skip=2, header=TRUE
  ) %>%
    gather(key="Sex", value="death_count", Female, Male, Total) %>%
    mutate(Country=this_country_name) %>%
    select(Country, Year, Age, Sex, death_count)
  
  output <- this_populations %>%
    plyr::join(this_deaths, type="full")
  
  return(output)
}

pop_death_combined <- ldply(list_country_dirs, fn, .progress="text")

# This can now be written out as a csv file

write.csv(pop_death_combined, file="data/derived_data/hmd_lexis_squares.csv", row.names=F)
