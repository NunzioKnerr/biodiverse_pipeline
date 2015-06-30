##########################################################################################################
# RUN THIS SCRIPT 1st
#
#This script checks and installs a number of packages if they are not already installed on the system
#to use it just select all of the text and run it in R. It basically speeds up the process of getting an R install up and running so you don't need to manually install packages.
#
#Nunzio.Knerr@csiro.au
#updated Date:19/06/2015
#
##########################################################################################################
#
#below is a list of packages to check and install
#
stdpkgs <- c("sp", "maptools", "XML", "gridExtra", "Cairo", "rgdal", "extrafont", "grid",  "rgeos", "raster", "plyr", "dplyr", "tidyr", "ggplot2", "RColorBrewer", "colorspace", "colorRamps", "spacetime", "aqp", "spatstat", "scales", "stringr", "gWidgets", "phytools", "ape", "apTreeshape") 
otherpkgs <- c("plotKML", "maps", "mapdata") 
#
#Do not edit below

  #check and install libraries
  # script to install needed packages for R Windows
  #  -- set CRAN mirror locally
  #setInternet2() # this sets an alternative internet. good for proxy if stupid intenet blocking is done
  local({r <- getOption("repos")
         r["CRAN"] <- "http://cran.csiro.au/"
         options(repos=r)
  })
   
  allpkgs <- union(stdpkgs, otherpkgs)
  
  pkgTest <- function(x)
  {
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE)
      if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }
    
lapply(allpkgs, pkgTest)
