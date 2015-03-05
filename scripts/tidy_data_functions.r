
# HFD and HMD compilation functions

require(tidyr)
require(stringr)
require(plyr)
require(dplyr)


hfd_compile <- function(baseloc){
  
  
  data_asfr <- read.table(
    file=paste(baseloc, "Files/zip_w/asfrRR.txt", sep="/"),
    header=T,
    skip=2,
    na.strings="."
  )
  
  # For typing convenience, it is useful to convert all variable names to 
  # lowercase. 
  names(data_asfr) <- tolower(names(data_asfr))

  data_asfr$age <- str_replace_all(data_asfr$age, "[+-]", "")
  data_asfr$age <- as.numeric(as.character(data_asfr$age))
  # We can now do the same with the other files
  
  data_births <- read.table(
    file=paste(baseloc, "Files/zip_w/birthsRR.txt", sep="/"),
    header=T,
    skip=2
  )
  
  names(data_births) <- tolower(names(data_births))
  data_births$age <- str_replace_all(data_births$age, "[+-]", "")
  data_births$age <- as.numeric(as.character(data_births$age))
  
  data_cpfr <- read.table(
    file=paste(baseloc, "Files/zip_w/cpfrRR.txt", sep="/"),
    header=T,
    skip=2
  )
  
  names(data_cpfr) <- tolower(names(data_cpfr))
  data_cpfr$age <- str_replace_all(data_cpfr$age, "[+-]", "")
  data_cpfr$age <- as.numeric(as.character(data_cpfr$age))
  
  data_expos <- read.table(
    file=paste(baseloc, "Files/zip_w/exposRR.txt", sep="/"),
    header=T,
    skip=2
  )
  
  names(data_expos) <- tolower(names(data_expos))
  data_expos$age <- str_replace_all(data_expos$age, "[+-]", "")
  data_expos$age <- as.numeric(as.character(data_expos$age))
  
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
  
  return(data_combined)  
}


hmd_compile <- function(baseloc){
  
  fn <- function(x){
    this_country_name <- x
    this_basedir <- paste(baseloc, this_country_name, "STATS", sep="/")
    
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
  
  list_country_dirs <- list.dirs(baseloc, recursive=F, full.names=FALSE)
  
  pop_death_combined <- ldply(list_country_dirs, fn, .progress="text")
  
  
  names(pop_death_combined) <- tolower(names(pop_death_combined))
  
  
  pop_death_combined$country <- pop_death_combined$country %>% tolower
  pop_death_combined$sex <- pop_death_combined$sex %>% tolower
  
  pop_death_combined$age <- pop_death_combined$age %>%
    mapvalues(from="110+", to="110") %>%
    as.character %>%
    as.numeric
  
  return(pop_death_combined)
  
}



