# set wd
setwd("~/GitHub/CompLegFall2019/data/us_upper_congress/Speech")

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

# read in data
# data source https://data.stanford.edu/congress_text#download-data 

# function to capitalize string
cap <- function(x) {
  x = tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


# for congress 1997-1999

for (i in 97:99) {
  SpeakerMap <- read.delim(paste0("hein-daily/0",i,"_SpeakerMap.txt"),
                           header = TRUE, sep = "|", stringsAsFactors = F)
  speeches <- read.delim(paste0("hein-daily/speeches_0",i,".txt"),
                         header = TRUE, sep = "|", stringsAsFactors = F)
  descr <- read.delim(paste0("hein-daily/descr_0",i,".txt"),
                      header = TRUE, sep = "|", stringsAsFactors = F) %>%
    select("speech_id", "date", speech_number = "number_within_file","speaker","char_count","word_count") %>%
    mutate(congress = i)
  
  # The SpeakerMap file doesn't contain the speeches given by the VP thus doesn't contain all speeches
  comb <- merge(SpeakerMap,descr, by = "speech_id") %>%
    merge(speeches, by = "speech_id") %>%
    mutate(last_name = cap(lastname)) %>%
    mutate(first_name = cap(firstname)) %>%
    mutate(full_name = paste(first_name,last_name)) %>%
    mutate(parliament_path = paste0("/parliament-",i)) %>%
    mutate(chamber_number = (chamber=="S")+1) %>%
    mutate(chamber_path = paste0(parliament_path,"/chamber-",chamber_number)) %>%
    mutate(speech_path = paste0(chamber_path,"/speech-",speech_number)) %>%
    select(parliament_path,chamber_path,speech_path,
           parliament_number=congress, chamber_number, speech_number, speech_id,
           speech_date = date, full_name, first_name, last_name, 
           state, gender, party, district, nonvoting, speaker,
           char_count, word_count, paragraph_text = speech)
  
  # create a variable name to store speeches for each congress
  var_name = paste0("us_house_speech_",i,".csv")
  #assign(var_name,comb)
  
  #export data
  write.csv(comb, var_name)
}

# for congress 100-114

for (i in 100:114) {
  SpeakerMap <- read.delim(paste0("hein-daily/",i,"_SpeakerMap.txt"),
                           header = TRUE, sep = "|", stringsAsFactors = F)
  speeches <- read.delim(paste0("hein-daily/speeches_",i,".txt"),
                         header = TRUE, sep = "|", stringsAsFactors = F)
  descr <- read.delim(paste0("hein-daily/descr_",i,".txt"),
                      header = TRUE, sep = "|", stringsAsFactors = F) %>%
    select("speech_id", "date", speech_number = "number_within_file","speaker","char_count","word_count") %>%
    mutate(congress = i)
  
  # The SpeakerMap file doesn't contain the speeches given by the VP thus doesn't contain all speeches
  comb <- merge(SpeakerMap,descr, by = "speech_id") %>%
    merge(speeches, by = "speech_id") %>%
    mutate(last_name = cap(lastname)) %>%
    mutate(first_name = cap(firstname)) %>%
    mutate(full_name = paste(first_name,last_name)) %>%
    mutate(parliament_path = paste0("/parliament-",i)) %>%
    mutate(chamber_number = (chamber=="S")+1) %>%
    mutate(chamber_path = paste0(parliament_path,"/chamber-",chamber_number)) %>%
    mutate(speech_path = paste0(chamber_path,"/speech-",speech_number)) %>%
    select(parliament_path,chamber_path,speech_path,
           parliament_number=congress, chamber_number, speech_number, speech_id,
           speech_date = date, full_name, first_name, last_name, 
           state, gender, party, district, nonvoting, speaker,
           char_count, word_count, paragraph_text = speech)
  
  # create a variable name to store speeches for each congress
  var_name = paste0("us_house_speech_",i,".csv")
  #assign(var_name,comb)
  
  #export data
  write.csv(comb, var_name)
}
