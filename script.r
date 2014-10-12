rm(list=ls())

source("scripts/LoadPackages.R")

RequiredPackages(
  c(
    "r2stl",
    "ggplot2",
    "reshape2",
    "plyr",
    "lattice",
    "stringr"
    )
  )

data <- read.csv("data/tidy/lexis_square_combined.csv")
data$X <- NULL
data$code <- tolower(data$code)
ddply(data, .(code), summarise, min_year = min(year), max_year=max(year))

fn  <- function(x){
  tiff(
    paste0(
      "figures/asfr/", x$code[1], ".tiff"
      ),
    1000,
    1000
    )
  
  print(
  contourplot(
    asfr ~ year * age, 
    data=x, 
    region=T, 
    col.regions=rev(heat.colors(200)), 
    cuts=50, 
    main=x$code[1]
    )
  )
  dev.off()
  
  tiff(
    paste0(
      "figures/total/", x$code[1], ".tiff"
    ),
    1000,
    1000
  )
  
  print(
    contourplot(
      total ~ year * age, 
      data=x, 
      region=T, 
      col.regions=rev(heat.colors(200)), 
      cuts=50, 
      main=x$code[1]
    )
  )
  dev.off()
  
  tiff(
    paste0(
      "figures/cpfr/", x$code[1], ".tiff"
    ),
    1000,
    1000
  )
  
  print(
    contourplot(
      cpfr ~ year * age, 
      data=x, 
      region=T, 
      col.regions=rev(heat.colors(200)), 
      cuts=50, 
      main=x$code[1]
    )
  )
  dev.off()
  
  tiff(
    paste0(
      "figures/exposure/", x$code[1], ".tiff"
    ),
    1000,
    1000
  )
  
  print(
    contourplot(
      exposure ~ year * age, 
      data=x, 
      region=T, 
      col.regions=rev(heat.colors(200)), 
      cuts=50, 
      main=x$code[1]
    )
  )
  dev.off()
  
  
  tiff(
    paste0(
      "figures/birth_rate/", x$code[1], ".tiff"
    ),
    1000,
    1000
  )
  
  print(
    contourplot(
      birth_rate ~ year * age, 
      data=x, 
      region=T, 
      col.regions=rev(heat.colors(200)), 
      cuts=50, 
      main=x$code[1]
    )
  )
  dev.off()
  
  
  return(NULL)
}


d_ply(data, .(code), fn)




