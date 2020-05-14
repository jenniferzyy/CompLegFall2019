# set wd
setwd("~/GitHub/CompLegFall2019/data/us_upper_congress/Member")

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

# Another useful dataset that lacks congress parameter
# data source https://github.com/unitedstates/congress-legislators
# Read data from data source
#cur <- read.csv("legislators-current.csv")
his <- read.csv("legislators-historical.csv") %>%
  filter(is.na(senate_class)) %>%
  select(last_name, first_name, district)


# Import data from three different data source for different parameteers


# Import data before 2017
# This data sourse lacks district parameter
# Source: https://data.world/chrisengelsma/united-states-senators-1789-2017
#df_sen <- fromJSON("https://query.data.world/s/qf6mow6r4jlv22mdibaipayh3hjvi2")
df_rep <- fromJSON("https://query.data.world/s/hlddzxoxeovvmlzj4zmu6mfptcf2q3")

# Import data after 2017 from Congress.gov
hr_search <- read.csv("houserep_member_search_result.csv", stringsAsFactors = F) 

# Reformat Data
dfmemlist <- list()
# first part of the list contains members who are no longer in the house
for (i in 1:length(df_rep[[1]][[1]])) {
  dfmem <- data.frame(df_rep[[1]][[1]][[i]],
                      first_name = df_rep[[1]][[2]][[i]], 
                      last_name = df_rep[[1]][[3]][[i]],
                      middle_name = df_rep[[1]][[4]][[i]], stringsAsFactors = F)
  dfmemlist[[i]]=dfmem
}
repmemhis <- do.call(rbind, dfmemlist)

# Merge historical members and select recent members from the 93rd congress to 2017
mergehis <- merge(repmemhis,his,by=c("first_name","last_name")) %>% distinct() %>%
  mutate(year_start = as.numeric(str_replace(year_start,"(\\d{4})-(\\d{4})","\\1"))) %>%
  mutate(year_end = as.numeric(str_replace(year_end,"(\\d{4})-(\\d{4})","\\2"))) %>%
  filter(year_end>1973) %>%
  mutate(full_name = paste(first_name,middle_name,last_name)) %>%
  mutate(full_name = str_replace(full_name,"  "," "))

hrsear <- hr_search %>%
  mutate(full_name = str_remove(Name,"Representative |Senator |Resident Commissioner ")) %>%
  separate(full_name, into = c("last_name", "first_name"), sep=", ") %>%
  mutate(first_name = str_remove(first_name," \".+\"")) %>% #remove nicknames
  mutate(full_name = paste(first_name, last_name)) %>%
  separate(first_name, into = c("first_name", "middle_name"), sep = " ") %>%
  mutate(state = state.abb[match(hr_search$State,state.name)]) %>%
  mutate(Terms = str_remove(Terms, "(.+\\| )?House: ")) %>%
  mutate(Terms = str_replace(Terms, "Present", "2020")) %>%
  mutate(year_start = as.numeric(str_extract(Terms, "^\\d{4}"))) %>%
  mutate(year_end = as.numeric(str_extract(Terms, "\\d{4}$"))) %>%
  select("first_name","last_name","Party","state","year_start","year_end","middle_name","District","full_name")

colnames(hrsear)[3] <- "party"
colnames(hrsear)[8] <- "district"

# merge data before and after 2017
hrmem3 <- rbind(mergehis,hrsear) %>%
  arrange(last_name,first_name) %>%
  mutate(member_number = 1:(nrow(hrsear)+nrow(mergehis))) 

# reformat member data
hrmem4 <- hrmem3 %>%
  select("first_name","last_name","party","state","year_start","year_end","middle_name","district","full_name", "member_number") %>%
  mutate(chamber_number = 1) %>%
  arrange(chamber_number, member_number) %>%
  mutate(observation_number = 1:nrow(hrmem3)) %>%
  mutate(chamber_path = paste0("/chamber-",chamber_number)) %>%
  mutate(member_path = paste0(chamber_path,"/member-",member_number)) %>%
  mutate(member_ID = member_path) %>%
  mutate(observation_path = member_path) %>%
  select("observation_path", "chamber_path", "member_path", "observation_number", "chamber_number", "member_number",
         "full_name", "first_name", "middle_name", "last_name", "member_ID", "state", "district", "party", "year_start", "year_end")


# add parliament number based on years
hrmem <- hrmem3 %>%
  mutate(freq = year_end - year_start + 1) %>%
  uncount(weights = freq, .id = "n", .remove = F) %>%
  mutate(parliament_number = (year_start+n+1)%/%2-894)

# reformat membership data
hrmem1 <- hrmem %>%
  select("first_name","last_name","party","state","year_start","year_end","middle_name","district","full_name", "parliament_number", "member_number") %>%
  distinct() %>%
  mutate(chamber_number = 1) 

hrmem2 <- hrmem1 %>%
  arrange(parliament_number, chamber_number, member_number) %>%
  mutate(observation_number = 1:nrow(hrmem1)) %>%
  mutate(parliament_path = paste0("/parliament-",parliament_number)) %>%
  mutate(chamber_path = paste0(parliament_path,"/chamber-",chamber_number)) %>%
  mutate(member_path = paste0(chamber_path,"/member-",member_number)) %>%
  mutate(member_ID = paste0("/chamber-",chamber_number,"/member-",member_number)) %>%
  mutate(observation_path = member_path) %>%
  select("observation_path", "parliament_path", "chamber_path", "member_path", "observation_number", "parliament_number", "chamber_number", "member_number",
         "full_name", "first_name", "middle_name", "last_name", "member_ID", "state", "district", "party", "year_start", "year_end")

# export data
write.csv(hrmem4, "us_house_member.csv")
write.csv(hrmem2, "us_house_membership.csv")
