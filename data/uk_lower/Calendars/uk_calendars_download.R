# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

lapply(c("stringr", "dplyr", "plyr", "tidyverse", "rvest", "zoo", "lubridate"), pkgTest)

# Set Working Directory
setwd("~/GitHub/CompLegFall2019/data/uk_lower/Calendars")

# Create empty vector
dates <- character(0)

# Loop thru dates
for (yr in 1990:1990) {
  for (mon in 1:12) {
    for (da in 1:31) {
      
      # add 0 to single digit month/day
      if(da<10) {
        day = paste0("0",da)
      }
      if(da>=10) {
        day = da
      }
      if(mon<10) {
        mo = paste0("0",mon)
      }
      if(mon>=10) {
        mo = mon
      }
      
      # base url
      base = "https://hansard.parliament.uk/Commons/"
      
      # construct url
      url = paste0(base,yr,"-",mo,"-",day)
      
      # check if there is a meeting
      cont <- url %>%
        read_html() %>%
        html_node("#sectionTree > .no-children a") %>%
        html_text()
      
      # add to vector
      if(!is.na(cont)) {
        date <- paste0(mo,"/",day,"/",yr)
        dates <- c(dates,date)
      }
    }
  }
}

