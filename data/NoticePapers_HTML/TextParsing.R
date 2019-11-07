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

# set working directory
setwd("~/GitHub/CompLegFall2019/data/NoticePapers_HTML")

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

lapply(c("stringr", "dplyr", "plyr", "tidyverse", "rvest", "zoo"), pkgTest)

##################################################
# read in data
##################################################

# file names
files <- list.files(pattern="*.html", full.names=TRUE, recursive=FALSE)

#################################
# function to parse HTML
#################################
info <- str_split(files[1], "_|\\.")
info <- unlist(info)

testa<- files
testa <- read_html(files[2])

# In all the titles that are bolded, date remains at the third top of the page
date <- testa %>%
  html_node("p:nth-child(3) b") %>%
  html_text()

# All bolded question numbers. Include other general information
num <- testa %>%
  html_nodes("b") %>%
  html_text()
num <- num[8:length(num)]
n <- length(num)

# Paragraphs
text <-testa %>%
  html_nodes(".ItemPara") %>%
  html_text()

# Headings of the Paragraphs. "Private Members' Business" not included
heading <-testa %>%
  html_nodes(".RubricTitle") %>%
  html_text()

# Supplement number of each question number
# Should be separated from the question number
sup <- testa %>% 
  html_nodes(".Footnote , sup") %>%
  html_text()
sup <- sup[1:n]

# Create Output
op <- data.frame(num,
                 sup,
                 text,
                 SittingDate = date)
                 #heading)
