# Setwd
setwd("~/Documents/GitHub/CompLegFall2019/data/us_upper_congress/Committee Membership")

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

# import data
# data source http://web.mit.edu/17.251/www/data_page.html
df = read.delim("hrc8097.mit.txt", header = F, sep = "\n", dec = ".")
df = read.delim("hrc98102.mit.txt", header = F, sep = "\n", dec = ".")
df = read.delim("snc80102.mit.txt", header = F, sep = "\n", dec = ".")

# create an empty list frame for data storage
cmlist <- list()
cmlist1 <- list()
cmlist2 <- list()

# convert from codebook
for (i in 1:nrow(df)) {
  # state code
  s = str_sub(df[i,],4,5)
  if(s =="01") State = "CT"
  if(s =="02") State = "ME"
  if(s =="03") State = "MA"
  if(s =="04") State = "NH"
  if(s =="05") State = "RI"
  if(s =="06") State = "VT"
  if(s =="11") State = "DE"
  if(s =="12") State = "NJ"
  if(s =="13") State = "NY"
  if(s =="14") State = "PA"
  if(s =="21") State = "IL"
  if(s =="22") State = "IN"
  if(s =="23") State = "MI"
  if(s =="24") State = "OH"
  if(s =="25") State = "WI"
  if(s =="31") State = "IA"
  if(s =="32") State = "KS"
  if(s =="33") State = "MN"
  if(s =="34") State = "MO"
  if(s =="35") State = "NE"
  if(s =="36") State = "ND"
  if(s =="37") State = "SD"
  if(s =="41") State = "AL"
  if(s =="42") State = "AR"
  if(s =="43") State = "FL"
  if(s =="44") State = "GA"
  if(s =="45") State = "LA"
  if(s =="46") State = "MS"
  if(s =="47") State = "NC"
  if(s =="48") State = "SC"
  if(s =="49") State = "TX"
  if(s =="50") State = "VA"
  if(s =="51") State = "KY"
  if(s =="52") State = "MD"
  if(s =="53") State = "OK"
  if(s =="54") State = "TN"
  if(s =="56") State = "WV"
  if(s =="61") State = "AZ"
  if(s =="62") State = "CO"
  if(s =="63") State = "ID"
  if(s =="64") State = "MT"
  if(s =="65") State = "NV"
  if(s =="66") State = "NM"
  if(s =="67") State = "UT"
  if(s =="68") State = "WY"
  if(s =="71") State = "CA"
  if(s =="72") State = "OR"
  if(s =="73") State = "WA"
  if(s =="74") State = "AK"
  if(s =="75") State = "HI"
  if(s =="55") State = "DC"
  if(s =="81") State = "GU"
  if(s =="82") State = "PR"
  if(s =="83") State = "VI"
  if(s =="84") State = "AS"
  
  # party
  p = str_sub(df[i,],11,13)
  if(p == "100") Party = "Democratic"
  if(p == "137") Party = "Democratic-Farmer_Labor"
  if(p == "200") Party = "Republican"
  if(p == "328") Party = "Independent"
  if(p == "350") Party = "American Labor"
  if(p == "360") Party = "Conservative"
  if(p == "999") Party = "Party Affliation Unknown"
  
  # period of service
  service = as.numeric(str_sub(df[i,],14,14))
  if(service<6){
    chamber = "House"
    service_period = service - 1
  } else{
    chamber = "Senate"
    service_period = service - 6
  }
  
  # member name
  full_name = trimws(str_sub(df[i,],21,45))
  
  # committee
  c = str_sub(df[i,],47,47)
  if(c == "1") committee_type = "Standing"
  if(c == "2") committee_type = "Select"
  if(c == "3") committee_type = "Special"
  if(c == "4") committee_type = "Party"
  if(c == "5") committee_type = "Subcommittee"
  if(c == "0") committee_type = "Inapplicable"
  
  congress = as.numeric(str_sub(df[i,],48,50))

  # committee name
  # reference codebook
  committee_name <- str_sub(df[i,],51,53)
  
  # committee service period
  cn <- str_sub(df[i,],61,61)
  if(cn == "1") committee_period = "Temporary"
  if(cn == "2") committee_period = "Only"
  if(cn == "3") committee_period = "1st"
  if(cn == "4") committee_period = "2nd"
  if(cn == "5") committee_period = "3rd"
  if(cn == "0") committee_period = "Inapplicable"
  
  # dates
  start_date = str_sub(df[i,],68,75) %>%
    str_replace_all(" ","0") %>%
    as.Date(format="%m%d%Y")
  
  end_date = str_sub(df[i,],76,83) %>%
    str_replace_all(" ","0") %>%
    as.Date(format="%m%d%Y")
  
  # form data frame
  df_m <- data.frame(full_name, State, Party, chamber, service_period, congress, 
                     committee_type, committee_name, committee_period,
                     start_date, end_date)
  #cmlist[[i]] <- df_m
  #cmlist1[[i]] <- df_m
  cmlist2[[i]] <- df_m
}

# reorganize data
df8097 <-do.call(rbind,cmlist)
df98102 <-do.call(rbind,cmlist1)
dfsen <- do.call(rbind,cmlist2)
  
# clean
house <- rbind(df8097,df98102) %>%
  mutate(parliament_path = paste0("/parliament-",congress)) %>%
  mutate(chamber_path = paste0(parliament_path,"/chamber-1")) %>%
  group_by(committee_name) %>%
  dplyr::mutate(member_number = dplyr::row_number()) %>%
  dplyr::mutate(committee_number = group_indices()) %>%
  mutate(committee_path = paste0(chamber_path,"/committee-",committee_number)) %>%
  mutate(member_path = paste0(committee_path,".member-",member_number)) %>%
  arrange(congress, member_number) %>%
  mutate(observation_number = 1:(nrow(df8097)+nrow(df98102)))

senate <- dfsen %>%
  mutate(parliament_path = paste0("/parliament-",congress)) %>%
  mutate(chamber_path = paste0(parliament_path,"/chamber-2")) %>%
  group_by(committee_name) %>%
  dplyr::mutate(member_number = dplyr::row_number()) %>%
  dplyr::mutate(committee_number = group_indices()) %>%
  mutate(committee_path = paste0(chamber_path,"/committee-",committee_number)) %>%
  mutate(member_path = paste0(committee_path,".member-",member_number)) %>%
  arrange(congress, member_number) %>%
  mutate(observation_number = 1:nrow(dfsen))

# export
write.csv(house,"us_house_committee_membership.csv")
write.csv(senate,"us_senate_committee_membership.csv")
