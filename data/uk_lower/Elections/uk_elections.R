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

lapply(c("stringr", "dplyr", "plyr", "tidyverse", "rvest", "zoo", "lubridate", "XML"), pkgTest)

# Set WD
setwd("~/GitHub/CompLegFall2019/data/uk_lower/Elections")

# Read downloaded data
elec <- read_csv("1918-2017election_results.csv")

# Function for capitalizing the first letter or each word
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1, 1)), tolower(substring(c, 2)),
        sep="", collapse=" ")
}

# Function to apply the previous function onto dataframe
capitalize_str <- function(charcter_string){
  sapply(charcter_string, CapStr)
}

# Reformat constituency
ele <- elec %>%
  mutate(Unconsted = as.numeric(total_votes<0)) %>%
  #mutate(constituency=replace(constituency,constituency ==constituency[15097], "Ynys Môn")) %>%
  mutate(constituency_name = capitalize_str(constituency))
ele$total_votes[is.na(ele$turnout)] <- 0
ele$turnout[is.na(ele$turnout)] <- 0
# ele$constituency[15097] <- "Ynys Môn"
ele$constituency_id <- NULL
ele$X20 <- NULL
ele$constituency <- NULL

# Reformat constituency for better merge
term <- rbind(c("&","and"),c("And","and"),c("Of","of"))
for (i in 1:nrow(term)){
  ele$constituency_name <- gsub(term[i,1],term[i,2], ele$constituency_name, perl = TRUE)
}

# Read constituency ID from previous dataset
cons <- read_csv("../Constituencies/uk_lower_constituencies.csv") %>%
  select(constituency_name,constituency_path)

# Merge constituency ID
check <- merge(ele,cons,by = "constituency_name", all = T) %>%
  distinct()
