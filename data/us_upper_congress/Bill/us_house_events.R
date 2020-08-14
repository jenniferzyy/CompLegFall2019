# set wd
setwd("~/GitHub/CompLegFall2019/data/us_upper_congress/Bill")

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils",
                      "package:datasets","package:methods","package:base")
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


# number of bills in each congress
num = c(17690, 15863, 14414, 8455, 7457, 6442, 5743, 5585,
        5977, 6212, 5310, 4344, 4874, 5681, 5767, 5431,
        6436, 7340, 6570, 6729, 5893, 6536, 7401, 6798)

# Create empty lists to store dataframes
dflist = list()
datalist = list()

# Import member data for member ID
hr_mem <- read.csv("../Member/us_house_member.csv")

# Create index of bill number
obs_number = 0

# text available starting from the 101th congress
# Loop through congress and bills
for (cong in 101:116) {
  for (i in 1:num[cong-92]) {
    # Form base url and read html
    base_url = paste0("https://www.congress.gov/bill/",cong,"th-congress/house-bill/",i) 
    base = base_url %>%
      read_html()
    
    # Parse Elements
    
    # Title
    bill_title <- base %>%
      html_node(".legDetail") %>%
      html_text() %>%
      str_extract(".+\\d+[th|rd|nd|st]") %>%
      str_remove(paste0(cong,"[th|rd|nd|st]$"))
    
    # Member
    # extract href link of member
    member_base <- base %>%
      html_node("tr:nth-child(1) a") %>%
      html_attr("href")
    member <- paste0("https://www.congress.gov", member_base) %>%
      read_html()
    
    # member name
    member_name <- member %>%
      html_node(".legDetail") %>%
      html_text() %>%
      str_extract("Representative.+\\(") %>%
      str_remove("^Representative ") %>%
      str_remove(" \\($")
    
    # state and district
    consti <- member %>%
      html_nodes("td") %>%
      html_text()
    consti <- consti[2]
    state <- str_split(consti,",")[[1]][1]
    district <- str_split(str_split(consti,",")[[1]][2]," ")[[1]][3] %>%
      as.numeric()
    
    # member ID
    member_id <- hr_mem$member_ID[which(hr_mem$full_name==member_name & hr_mem$district==district)]
    if(is_empty(member_id)){member_id = "na "}
    
    #event
    event_date <- paste0(base_url,"/all-actions") %>%
      read_html() %>%
      html_nodes("td.date") %>%
      html_text()
    event_chamber <- paste0(base_url,"/all-actions") %>%
      read_html() %>%
      html_nodes(".date+ td") %>%
      html_text()
    event_description <- paste0(base_url,"/all-actions") %>%
      read_html() %>%
      html_nodes("td.actions") %>%
      html_text()
    
    # number of events
    evenum <- length(event_date)
    event_number = 1:evenum
    
    # chamber number
    # use 1 for lower house - house of rep
    chamber_number = 1
    
    # bill number
    bill_number = i
    
    # observation
    observation_number = obs_number:(obs_number+evenum-1)
    obs_number = obs_number + evenum
    
    # sort
    congress_path = paste0("/congress-", cong)
    chamber_path = paste0(congress_path, "/chamber-", chamber_number)
    bill_path = paste0(chamber_path,"bill-",bill_number)
    observation_path = bill_path
    
    # construct data frame
    df = data.frame(bill_title, member_name, member_id, state, district, 
                    event_date, event_chamber, event_description, 
                    congress_number = cong, chamber_number, bill_number, event_number, observation_number,
                    congress_path, chamber_path, bill_path, observation_path, stringsAsFactors = FALSE) %>%
      mutate(event_path = paste0(bill_path,"/event-", event_number))
    dflist[[i]] <- df
  }
  dat = do.call(rbind, datalist)
  datalist[[i]]<- dat
}
house_bill = do.call(rbind, datalist)

# Data output
write.csv(house_bill, "us_house_bill_events.csv")

