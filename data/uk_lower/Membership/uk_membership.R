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

# working directoy
setwd("~/GitHub/CompLegFall2019/data/uk_lower/raw_characteristics")

# Read XML
files <- list.files(pattern="*.xml", full.names=TRUE, recursive=FALSE)
committes <- xmlToDataFrame(files[1])
constituencies <- xmlToDataFrame(files[2])
parties <- xmlToDataFrame(files[9]) 
parliament <- xmlToDataFrame(files[8])
other <- xmlToDataFrame(files[7])

# Clean data
name <- constituencies %>%
  mutate(chamber_name = House) %>%
  mutate(full_name = DisplayAs) %>%
  mutate(last_name = str_replace(ListAs,"(.*)(,\\s)(.*)","\\1")) %>%
  mutate(first_name = str_replace(ListAs,"(.*)(,\\s)(.*)","\\3")) %>%
  mutate(constituency_name = MemberFrom) %>%
  mutate(party_name = Party) %>%
  mutate(start_date = str_extract(HouseStartDate,"\\d{4}-\\d{2}-\\d{2}")) %>%
  mutate(end_date = str_extract(HouseEndDate,"\\d{4}-\\d{2}-\\d{2}")) %>%
  select("chamber_name","full_name","last_name","first_name","constituency_name","party_name","start_date","end_date") %>%
  arrange(last_name,first_name) %>%
  mutate(member_number = 1:nrow(constituencies)) %>%
  mutate(chamber_number = (chamber_name=="Lords")+1) %>%
  arrange(chamber_number,member_number) %>%
  mutate(observation_number = 1:nrow(constituencies)) %>%
  mutate(chamber_path = paste0("/chamber-",chamber_number)) %>%
  mutate(member_path = paste0(chamber_path, "/member-", member_number)) %>%
  mutate(observation_path = member_path)

# Read constituency data
consti <- read.csv("../Constituencies/uk_lower_constituencies.csv") %>%
  select("constituency_name",constituency_ID="constituency_path")

# Merge constituency ID
member <- merge(name, consti, by = "constituency_name", all = T) %>%
  filter(!is.na(observation_path)) %>%
  select("observation_path", "chamber_path", "member_path", "observation_number", "chamber_number", "member_number",
         "chamber_name", "full_name", "first_name", "last_name", "constituency_name", "constituency_ID", "party_name", "start_date", "end_date") %>%
  arrange(observation_number)

# Output
write.csv(member, "uk_lower_members.csv")

# Membership
parli <- member %>%
  select(chamber_path1 = "chamber_path", member_path1 = "member_path","chamber_number", "member_number",
         "chamber_name", "full_name", "first_name", "last_name", "constituency_name", "constituency_ID", "party_name", "start_date", "end_date") %>%
  mutate(member_ID = member_path1) %>%
  mutate(start_term = as.factor(start_date)) %>%
  mutate(end_term = as.factor(end_date))
