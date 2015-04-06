
# HFD and HMD compilation functions

source("scripts/LoadPackages.R")

RequiredPackages(
  c(
    "tidyr", "stringr",
    "plyr", "dplyr", "lattice"
    )
  )


merge_lexis_square_hfd <- function(loc){
  
  data_asfr <- read.table(
    file=paste(loc, "Files/zip_w/asfrRR.txt", sep="/"),
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
    file=paste(loc, "Files/zip_w/birthsRR.txt", sep="/"),
    header=T,
    skip=2
  )
  
  names(data_births) <- tolower(names(data_births))
  data_births$age <- str_replace_all(data_births$age, "[+-]", "")
  data_births$age <- as.numeric(as.character(data_births$age))
  
  data_cpfr <- read.table(
    file=paste(loc, "Files/zip_w/cpfrRR.txt", sep="/"),
    header=T,
    skip=2
  )
  
  names(data_cpfr) <- tolower(names(data_cpfr))
  data_cpfr$age <- str_replace_all(data_cpfr$age, "[+-]", "")
  data_cpfr$age <- as.numeric(as.character(data_cpfr$age))
  
  data_expos <- read.table(
    file=paste(loc, "Files/zip_w/exposRR.txt", sep="/"),
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


merge_lexis_square_hmd <- function(loc){
  
  fn <- function(x){
    this_country_name <- x
    this_basedir <- paste(loc, this_country_name, "STATS", sep="/")
    
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
  
  list_country_dirs <- list.dirs(loc, recursive=F, full.names=FALSE)
  
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

generate_scp_asfr <- function(
  dta,
  outdir="figures/asfr/"
  ){
  # For consistency we will  rename 'code' as 'country' in data_hfd,
  # and coerce all country labels to lowercase
  
  dta <- dta  %>% tbl_df %>%
    rename(country=code)
  
  dta$country <- dta$country %>% tolower
  
  dir.create(outdir, recursive=T)
  
  fn <- function(x){
    this_country <- x$country[1] # All entries should be the same, but 
    # the output should only be of length 1. 
    
    min_year = min(x$year)
    max_year = max(x$year)
    
    tiff(paste0(
        outdir, 
        this_country, 
        "_asfr(",
        min_year,
        "-",
        max_year,
        ").tiff"
      ), width=1000, height=1000)
    

    
    title <- paste0(toupper(this_country), ", ASFR\n", min_year, " to ", max_year)
    
    x %>%
      filter(age <=50) %>%
      contourplot(
        asfr ~ year * age, 
        data= .,
        region=T,
        col.regions=rev(heat.colors(100)),
        cuts=20,
        main=list(label=title, cex=2),
        xlab=list(cex=2),
        ylab=list(cex=2),
        scales=list(cex=2),
        colorkey=list(labels=list(cex=2)),
        col="grey",
        labels=list(col="black", cex=1.5, fontface="bold"),
        par.strip.text=list(cex=2.5)
      ) %>%
      print
    
    dev.off()
    
    return(NULL)
  }
  
  d_ply(dta, .(country), fn, .progress="text")
  
}


generate_scp_population <- function(
  dta,
  outdir="figures/population/"
){
  dta <- dta  %>% tbl_df 
  
  dta$country <- dta$country %>% tolower
  
  dta <- dta %>%
    subset(sex!="Total")
  
  dir.create(outdir, recursive=T)
  
  fn <- function(x){
    this_country <- x$country[1] # All entries should be the same, but 
    # the output should only be of length 1. 
    
    min_year = min(x$year)
    max_year = max(x$year)
    
    tiff(paste0(
      outdir, 
      this_country, 
      "_population(",
      min_year,
      "-",
      max_year,
      ").tiff"
    ), width=2000, height=1000)
    
    
    
    title <- paste0(toupper(this_country), ", Population Counts\n", min_year, " to ", max_year)
    
    x %>%
      filter(age <=90 & sex !="total") %>%
      contourplot(
        population_count ~ year * age | sex, 
        data= .,
        region=T,
        col.regions=rev(heat.colors(100)),
        cuts=50,
        main=list(label=title, cex=2),
        xlab=list(cex=2),
        ylab=list(cex=2),
        scales=list(cex=2),
        colorkey=list(labels=list(cex=2)),
        col="grey",
        labels=list(col="black", cex=1.5, fontface="bold"),
        par.strip.text=list(cex=2.5)
      ) %>%
      print
    
    dev.off()
    
    return(NULL)
  }
  
  d_ply(dta, .(country), fn, .progress="text")
  
  
}

generate_scp_logmort <- function(
  dta,
  outdir="figures/logmort/"
){
  dta <- dta  %>% tbl_df 
  
  dta$country <- dta$country %>% tolower
  
  dta <- dta %>%
    mutate(log_mort = log((death_count+0.5)/(population_count+0.5)))
  
  dir.create(outdir, recursive=T)
  
  fn <- function(x){
    this_country <- x$country[1] # All entries should be the same, but 
    # the output should only be of length 1. 
    
    min_year = min(x$year)
    max_year = max(x$year)
    
    tiff(paste0(
      outdir, 
      this_country, 
      "_logmort(",
      min_year,
      "-",
      max_year,
      ").tiff"
    ), width=2000, height=1000)
    
    
    
    title <- paste0(toupper(this_country), ", Log mortality rates\n", min_year, " to ", max_year)
    
    x %>%
      filter(age <=90 & sex !="total") %>%
      contourplot(
        log_mort ~ year * age | sex, 
        data= .,
        region=T,
        col.regions=rev(heat.colors(100)),
        cuts=50,
        main=list(label=title, cex=2),
        xlab=list(cex=2),
        ylab=list(cex=2),
        scales=list(cex=2),
        colorkey=list(labels=list(cex=2)),
        col="grey",
        labels=list(col="black", cex=1.5, fontface="bold"),
        par.strip.text=list(cex=2.5)
      ) %>%
      print
    
    dev.off()
    
    return(NULL)
  }
  
  d_ply(dta, .(country), fn, .progress="text")  
  
}

