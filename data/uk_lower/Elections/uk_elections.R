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
elec <- read.csv("1918-2017election_results.csv",stringsAsFactors = F)

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
ele$constituency_id <- NULL
ele$X <- NULL
ele$constituency <- NULL

# Change negative votes of uncontested back into zero
ele$total_votes[is.na(ele$turnout)] <- 0
ele$turnout[is.na(ele$turnout)] <- 0
ele$con_votes[ele$con_votes<0] <- 0
ele$lab_votes[ele$lab_votes<0] <- 0
ele$oth_votes[ele$oth_votes<0] <- 0

# Reformat constituency for better merge
term <- rbind(c("&","and"),c("And","and"),c("Of","of"))
for (i in 1:nrow(term)){
  ele$constituency_name <- gsub(term[i,1],term[i,2], ele$constituency_name, perl = TRUE)
}

# Read constituency ID from previous dataset
cons <- read.csv("../Constituencies/uk_lower_constituencies.csv", stringsAsFactors = F) %>%
  select(constituency_name,constituency_path,constituency_number,parliament_path)

# Merge constituency ID
el <- merge(ele,cons,by = "constituency_name") %>%
  distinct() %>%
  filter(election!="")%>%
  group_by(election) %>%
  dplyr::mutate(election_number = group_indices())%>%
  mutate(election_path = paste0("/election-",election_number,constituency_path)) %>%
  arrange(election_number,constituency_number) %>%
  mutate(observation_number = 1:length(election_number)) %>%
  mutate(observation_path = election_path)

# Rearrange columns
e <- el %>%
  select(observation_path, election_path, constituency_path,
         observation_number, election_number, constituency_number,
         election_year = election,
         constituency_name, constituency_ID = parliament_path,
         country_name = country.region, electorate, seats, uncontested = Unconsted,
         con_votes, con_share, lib_votes, lib_share, lab_votes, lab_share, natSW_votes, natSW_share, oth_votes, oth_share,
         total_votes, turnout)

# Output
write.csv(e, "uk_lower_elections.csv")
