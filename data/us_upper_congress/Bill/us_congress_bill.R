# set wd
setwd("~/GitHub/CompLegFall2019/data/us_upper_congress/Bill")

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
observation_number = 0

# Loop through congress and bills
for (cong in 93:116) {
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
      #str_extract(".+\\.\\d+th") %>%
      str_remove("\\d+[th|rd|nd|st]$")
    
    # Bill Type
    bill_type = "bill"
    
    # for now, the constructed url only contains house bills
    # but in a case that the loop contains all bill types, use the following code
    # 
    # type <- bill_title%>%
    #   str_remove("\\d+.+")
    # bill_type <- type %>%
    #   str_replace("H.R.|S.", "Bills") %>%
    #   str_replace("H.Amdt.|S.Amdt.", "Amendments") %>%
    #   str_replace("H.Res.|S.Res.", "Resolutions") %>%
    #   str_replace("H.J.Res.|S.J.Res.", "Joint Resolutions") %>%
    #   str_replace("H.Con.Res|S.Con.Res.", "Concurrent Resolutions")
    
    # Text
    bill_text <- paste0(base_url,"/text") %>%
      read_html() %>%
      html_node(".generated-html-container")%>%
      html_text() 
    
    # Committees 
    committee <- base %>%
      html_node("tr:nth-child(2) td") %>%
      html_text()
    
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
      html_nodes(".lateral01 td") %>%
      html_text()
    state <- consti[1]
    district <- consti[2]%>%
      str_extract("\\d+")%>%
      as.numeric()
    
    # member ID
    member_id <- hr_mem$member_ID[which(hr_mem$full_name==member_name & hr_mem$district==district)]
    
    # date introduced
    date <- paste0(base_url,"/actions") %>%
      read_html() %>%
      html_nodes(".date") %>%
      html_text() %>%
      tail(n = 1) %>%
      mdy()
    
    # chamber number
    # use 1 for lower house - house of rep
    chamber_number = 1
    
    # bill number
    bill_number = i
    observation_number = observation_number+1
    
    # sort
    congress_path = paste0("/congress-", cong)
    chamber_path = paste0(congress_path, "/chamber-", chamber_number)
    bill_path = paste0(chamber_path,"bill-",bill_number)
    observation_path = bill_path
    
    # construct data frame
    df = data.frame(date, bill_title, bill_type, bill_text, member_name, member_id, state, district, committee, 
                    congress = cong, chamber_number, bill_number, observation_number,
                    congress_path, chamber_path, bill_path, observation_path, stringsAsFactors = FALSE)
    dflist[[i]] <- df
  }
  dat = do.call(rbind, datalist)
  datalist[[i]]<- dat
}
house_bill = do.call(rbind, datalist)

# Data output
write.csv(house_bill, "us_house_bill.csv")

