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

lapply(c("stringr", "dplyr", "plyr", "tidyverse", "rvest", "zoo"), pkgTest)

# Set Working Directory
setwd("~/GitHub/CompLegFall2019/data/canadaTextParsing/Questions")

# read in table of contents files
tables <- list.files(pattern="*.html", full.names=TRUE, recursive=FALSE)

# download questions
for (i in 1:length(tables)) {
  # read table of contents
  list <- read_html(tables[i]) %>% html_nodes(".Link") %>% html_text()
  attr <- read_html(tables[i]) %>% html_nodes(".Link") %>% html_attrs()
  loc <- match("Questions",list)
  
  # make url
  name <- unlist(str_split(tables[i], "_|\\."))
  url <- paste0("https://www.ourcommons.ca",attr[[loc]][2])

  # make file name
  filename <- str_c("Questionurl/questions_", name[5], "_", name[6], "_", name[7], ".html", collapse = "")
  
  # download file
  if (!is.na(url)&!is.na(loc)) {
    download.file(url, filename, quiet = TRUE)
  }
 
  # random delay
  Sys.sleep(runif(1, 0, 0.25))
}

