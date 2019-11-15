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

# changing the dates to parliament numbers
oridate <- levels(parli$start_term)
oridate[oridate>="2017-06-08"] = 57
oridate[oridate<"2017-06-08" & oridate >= "2015-05-07"] = 56
oridate[oridate>="2010-05-06" & oridate < "2015-05-07"] = 55
oridate[oridate<"2010-05-06" & oridate >= "2005-05-05"] = 54
oridate[oridate>="2001-06-07" & oridate < "2005-05-05"] = 53
oridate[oridate<"2001-06-07" & oridate >= "1997-05-01"] = 52
oridate[oridate>="1992-04-09" & oridate < "1997-05-01"] = 51
oridate[oridate<"1992-04-09" & oridate >= "1987-06-11"] = 50
oridate[oridate>="1983-06-09" & oridate < "1987-06-11"] = 49
oridate[oridate<"1983-06-09" & oridate >= "1979-05-03"] = 48
oridate[oridate>="1974-10-10" & oridate < "1979-05-03"] = 47
oridate[oridate<"1974-10-10" & oridate >= "1974-02-28"] = 46
oridate[oridate>="1970-06-18" & oridate < "1974-02-28"] = 45
oridate[oridate<"1970-06-18" & oridate >= "1966-03-31"] = 44
oridate[oridate>="1964-10-15" & oridate < "1966-03-31"] = 43
oridate[oridate<"1964-10-15" & oridate >= "1959-10-08"] = 42
oridate[oridate>="1955-05-26" & oridate < "1959-10-08"] = 41
oridate[oridate<"1955-05-26" & oridate >= "1951-10-25"] = 40
oridate[oridate>="1950-02-23" & oridate < "1951-10-25"] = 39
oridate[oridate<"1950-02-23" & oridate >= "1945-07-05"] = 38
oridate[oridate>="1935-11-14" & oridate < "1945-07-05"] = 37
levels(parli$start_term) <- oridate

oridate <- levels(parli$end_term)
oridate[oridate>="2017-06-08"] = 57
oridate[oridate<"2017-06-08" & oridate >= "2015-05-07"] = 56
oridate[oridate>="2010-05-06" & oridate < "2015-05-07"] = 55
oridate[oridate<"2010-05-06" & oridate >= "2005-05-05"] = 54
oridate[oridate>="2001-06-07" & oridate < "2005-05-05"] = 53
oridate[oridate<"2001-06-07" & oridate >= "1997-05-01"] = 52
oridate[oridate>="1992-04-09" & oridate < "1997-05-01"] = 51
oridate[oridate<"1992-04-09" & oridate >= "1987-06-11"] = 50
oridate[oridate>="1983-06-09" & oridate < "1987-06-11"] = 49
oridate[oridate<"1983-06-09" & oridate >= "1979-05-03"] = 48
oridate[oridate>="1974-10-10" & oridate < "1979-05-03"] = 47
oridate[oridate<"1974-10-10" & oridate >= "1974-02-28"] = 46
oridate[oridate>="1970-06-18" & oridate < "1974-02-28"] = 45
oridate[oridate<"1970-06-18" & oridate >= "1966-03-31"] = 44
oridate[oridate>="1964-10-15" & oridate < "1966-03-31"] = 43
oridate[oridate<"1964-10-15" & oridate >= "1959-10-08"] = 42
oridate[oridate>="1955-05-26" & oridate < "1959-10-08"] = 41
oridate[oridate<"1955-05-26" & oridate >= "1951-10-25"] = 40
levels(parli$end_term) <- oridate

# change factors to numerics and take out NA's
sett <- parli%>%
  mutate(start = as.numeric(as.character(start_term))) %>%
  mutate(end = as.numeric(as.character(end_term)))
sett$end[is.na(sett$end)]=57
sett1 <- sett %>%
  mutate(freq = end-start+1)

# expand the data frame to one entry for each parliament
ship <- sett1 %>%
  uncount(weights = freq, .id = "n", .remove = F) %>%
  mutate(parliament_number = start+n-1)

# clean the data frame
mem <- ship %>%
  arrange(parliament_number,chamber_number,member_number) %>%
  mutate(observation_number = 1:nrow(ship)) %>%
  mutate(parliament_path = paste0("/parliament-",parliament_number)) %>%
  mutate(chamber_path = paste0(parliament_path,chamber_path1)) %>%
  mutate(member_path = paste0(parliament_path,member_path1)) %>%
  mutate(observation_path = member_path) %>%
  select("observation_path", "parliament_path", "chamber_path", "member_path", "observation_number", "parliament_number", "chamber_number", "member_number",
         "chamber_name", "full_name", "first_name", "last_name", "member_ID", "constituency_name", "constituency_ID", "party_name", "start_date", "end_date")

# Output
write.csv(mem, "uk_lower_chamber_membership.csv")
