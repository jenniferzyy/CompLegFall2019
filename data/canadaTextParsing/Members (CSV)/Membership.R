# Set working directory
setwd("~/GitHub/CompLegFall2019/data/canadaTextParsing/Members (CSV)")

# Setup
library(tidyverse)
library(stringr)

# Read Data
member <- read.csv("canada_members_updated.csv",encoding = "UTF-8", stringsAsFactors = F) %>%
  select("full_name", "member_number") %>%
  mutate(member_ID = paste0("/chamber-1/member-",member_number))
member$member_number <- NULL
mem42 <- read.csv("canada_commons_members_parl42.csv",stringsAsFactors = F) %>%
  select(Constituency)
consti <- read.csv("canada_constituencies.csv",encoding = "UTF-8", stringsAsFactors = F) %>%
  select(constituency_name)
colnames(consti)[1] <- "Constituency"

# Reassign constituency numbers
cons <- rbind(mem42,consti)%>%
  distinct() %>%
  arrange(Constituency) %>%
  mutate(number = 1:length(Constituency)) %>%
  mutate(constituency_ID = paste0("/chamber-1/constituency-",number))
cons$number <- NULL

# Read Membership Data
file_names <- list.files(pattern="*.csv", full.names=TRUE, recursive=FALSE)
df <- do.call(rbind, lapply(file_names, function(x) cbind(read.csv(x,encoding="UTF-8",na.strings=c("", "NA"), stringsAsFactors = F), parliament_number=strsplit(x,'_|\\.')[[1]][5])))

# Merge Constituency ID and Member ID
membership <- merge(df, cons, by = "Constituency", encoding="UTF-8", all = T) %>%
  mutate(parliament_path = paste0("/parliament-",str_extract(parliament_number,"\\d+"))) %>%
  mutate(chamber_path = paste0(parliament_path,"/chamber-1")) %>%
  mutate(full_name = paste(First.Name, Last.Name)) %>%
  separate(Start.Date, c("start_date","time1"), sep = "\\s") %>%
  separate(End.Date, c("end_date","time2"), sep = "\\s") %>%
  merge(member, by = "full_name", encoding="UTF-8", all = T)
membership <- membership[!is.na(membership$First.Name),]
membership$parliament_number <- str_extract(membership$parliament_number,"\\d+")
membership$Honorific.Title <- NULL
membership$Province...Territory <- NULL
membership$time1 <- NULL
membership$time2 <- NULL

colnames(membership)[2:5] <- c("constituency_name", "first_name", "last_name", "party_name")

# Assign Member Number
can_membership <- membership %>%
  arrange(parliament_number,last_name) %>%
  mutate(observation_number = 1:nrow(membership)) %>%
  group_by(parliament_number) %>%
  mutate(member_number = 1:length(parliament_number)) %>%
  mutate(member_path = paste0(chamber_path,"/member-",member_number)) %>%
  mutate(observation_path = member_path) %>%
  mutate(chamber_number = 1) %>%
  mutate(chamber_name = "House of Commons")

# Rearrange Columns
can_membership <- can_membership[c("observation_path", "parliament_path", "chamber_path", "member_path", "observation_number", "parliament_number", "chamber_number", "member_number", "chamber_name", "full_name", "first_name", "last_name", "member_ID", "constituency_name", "party_name", "start_date", "end_date")]

# Output data
write.csv(can_membership, "canada_chamber_membership_updated.csv")
