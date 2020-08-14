# set wd
setwd("~/GitHub/CompLegFall2019/data/us_upper_congress/Committee")

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

lapply(c("stringr", "dplyr", "plyr", "tidyverse", "rvest", "zoo", "lubridate", "XML", "jsonlite"), pkgTest)



# Import Data
dfcom <- read.csv("https://query.data.world/s/dzhl7dp7crduavgqzedkl674u4wit2", header=TRUE, stringsAsFactors=FALSE)

# Reformat data
housecom <- dfcom %>%
  filter(house_committee_id!="" | str_detect(parent, "House Committee")) %>%
  mutate(chamber_number = 1) %>%
  arrange(name) %>%
  mutate(committee_number = 1:nrow(housecom)) %>%
  mutate(committee_id = house_committee_id)
housecom$type[housecom$house_committee_id==""] <- "housesub"

sencom <- dfcom %>%
  filter(senate_committee_id!="" | str_detect(parent, "Senate Committee")) %>%
  mutate(chamber_number = 2) %>%
  arrange(name) %>%
  mutate(committee_number = 1:nrow(sencom)) %>%
  mutate(committee_id = senate_committee_id)
sencom$type[sencom$senate_committee_id==""] <- "senatesub"

# merge house and senate
uscom <- rbind(housecom,sencom) %>%
  arrange(chamber_number,committee_number) %>%
  mutate(observation_number = 1:(nrow(housecom)+nrow(sencom))) %>%
  arrange(chamber_number,committee_number) %>%
  mutate(chamber_path = paste0("/chamber-",chamber_number)) %>%
  mutate(committee_path = paste0(chamber_path, "/committee-",committee_number)) %>%
  mutate(observation_path = committee_path) %>%
  select(observation_path, chamber_path, committee_path, observation_number, chamber_number, committee_number,
         committee_name=name, parent_name=parent, past_names=pastNames, type, committee_id)

# data output
write.csv(uscom, "us_committee.csv")
