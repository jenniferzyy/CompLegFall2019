# set wd
setwd("~/Documents/GitHub/CompLegFall2019/data/us_upper_congress/Bill")

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
bill_num = c(17690, 15863, 14414, 8455, 7457, 6442, 5743, 5585,
        5977, 6212, 5310, 4344, 4874, 5681, 5767, 5431,
        6436, 7340, 6570, 6729, 5893, 6536, 7401, 6798)

# Create empty lists to store dataframes
dflist = list()
datalist = list()


# Create index of bill number
obs_number = 0

# bill text sare only available starting from the 101st congress
# date of bill version only available starting from the 103rd congress
# the current congress is 116
# Loop through congress and bills
for (cong in 103:116) {
  for (i in 1:bill_num[cong-92]) {
    # Form base url and read html
    base_url = paste0("https://www.congress.gov/bill/",cong,"th-congress/house-bill/",i) 
    base = base_url %>%
      read_html()
    text_url = paste0(base_url,"/text")%>%
      read_html()
    
    # Parse Elements
    
    # Title
    bill_title <- base %>%
      html_node(".legDetail") %>%
      html_text() %>%
      str_extract(".+\\d+[th|rd|nd|st]") %>%
      str_remove(paste0(cong,"[th|rd|nd|st]$"))
    
    # date introduced
    date_introduced <- paste0(base_url,"/actions") %>%
      read_html() %>%
      html_nodes(".date") %>%
      html_text() %>%
      tail(n = 1) %>%
      mdy()
    
    # number of versions
    num <- text_url %>% 
      html_node(".optional") %>% 
      html_text() %>% 
      str_extract("\\d+")
    
    # only one version
    if(num=="1"|is.na(num)){
      version = "Introduced in House"
      date_version = date_introduced
      version_number = 1
      bill_text = paste0(base_url,"/text/","?format=txt") %>%
        read_html() %>%
        html_node("#billTextContainer") %>%
        html_text()
      obs_number = obs_number+1
      vsd <- data.frame(version, date_version, bill_text, version_number, "observation_number"=obs_number)
    } else{
    # list of all versions
    version = text_url %>% html_nodes(xpath = '//*[(@id = "textVersion")]/option') %>% 
      html_text()
    vsd <- as.data.frame(version) %>% 
      mutate(date_version = str_extract(version,"\\d+/\\d+/\\d+")) %>%
      mutate(version = str_remove(version," \\(.+\\)")) %>%
      mutate(date_version = mdy(date_version))
    # vs = text_url %>% html_node("#textVersion") %>% html_text() %>%strsplit(")")
    # vsd <- data.frame(matrix(unlist(vs), nrow=4, byrow=T),stringsAsFactors=FALSE)%>% 
    #   separate(matrix.unlist.vs...nrow...4..byrow...T., into = c("version","date_version"),sep = " \\(")
    abbr <- text_url %>% html_nodes(xpath = '//*[(@id = "textVersion")]/option') %>% html_attr("value")
    vsd$bill_text <- 0
    vsd$version_number <- 0
    vsd$observation_number <- 0
    for (j in 1:as.numeric(num)) {
      text <- paste0(base_url,"/text/",abbr[j],"?format=txt") %>%
        read_html() %>%
        html_node("#billTextContainer") %>%
        html_text()
      vsd$bill_text[j] = text
      vsd$version_number[j] = j
      obs_number = obs_number+1
      vsd$observation_number <- obs_number
    }
    }
    
    # Bill Type
    bill_type = "bill"
    
    # chamber number
    # use 1 for lower house - house of rep
    chamber_number = 1
    
    # bill number
    bill_number = i
    
    
    # sort
    congress_path = paste0("/congress-", cong)
    chamber_path = paste0(congress_path, "/chamber-", chamber_number)
    bill_path = paste0(chamber_path,"bill-",bill_number)
    observation_path = bill_path
    
    # output txt
    bill_label = paste0("BillVersion/","congress-",cong,"-chamber-",chamber_number,"-bill-",bill_number,"-version",version_number,".txt")
    write.table(bill_text,file = bill_label)
    bill_text = bill_label
    
    # construct data frame
    df = data.frame(date_introduced, bill_title, bill_type, vsd,
                    congress_number = cong, chamber_number, bill_number, 
                    congress_path, chamber_path, bill_path, observation_path, stringsAsFactors = FALSE) %>%
      mutate(version_path = paste0(bill_path,"/version-",version_number))
    dflist[[i]] <- df
  }
  dat = do.call(rbind, dflist)
  datalist[[i]]<- dat
}
house_bill = do.call(rbind, datalist)

# Data output
write.csv(house_bill, "us_house_bill_versions.csv")

