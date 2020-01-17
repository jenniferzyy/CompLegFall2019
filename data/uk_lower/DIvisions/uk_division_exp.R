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

# Set Wd
setwd("~/GitHub/CompLegFall2019/data/uk_lower/DIvisions")

# Read data
files <- list.files(pattern="*commonsdivisions", full.names=TRUE, recursive=FALSE)
div <- do.call(rbind, lapply(files, function(x) cbind(read_csv(x))))
members <- read.csv("../Membership/uk_lower_members.csv",stringsAsFactors = F, encoding = "UTF-8")
#remove extra space between first and last name
members$full_name <- str_replace_all(members$full_name,"\\s\\s"," ")


# Construct URL
url <- "http://lda.data.parliament.uk/commonsdivisions/"
id <- str_extract(div$uri, "\\d{6,7}")
link <- paste0(url,id,".xml")

# Call API
for (i in 1:length(id)) {
  # make file name
  file <- str_c(getwd(), "/", "commonsdivisions","/", "divisions_page_", i, ".xml", collapse = "")
  # download file
  tryCatch(download.file(link[i], file, quiet = TRUE), 
           error = function(e) print(paste(file, 'questions missing')))
}

# read XML files
setwd("~/GitHub/CompLegFall2019/data/uk_lower/DIvisions/commonsdivisions")
f_xml <- list.files(pattern="divisions_page_", full.names=TRUE, recursive=FALSE)

# parse XML
parse_divisions <- function(index) {
  content <- xmlToList(xmlParse(read_xml(f_xml[index])))[[3]]
  division_date <- content[[11]][[1]]
  n = as.numeric(str_extract(f_xml[index],"\\d+"))
  if(n<=719) {
    votes <- content[[17]]
    division_number <- content[[12]]
  }
  if(n>719) {
    division_number <- content[[13]]
    votes <- content[[18]]
  }
  
  #initiate variables
  member_name = 0
  member_ID = 0
  constituency_name = 0
  constituency_ID = 0
  yea = 0
  
  #create a row for each member/vote
  for (i in 1:length(votes)) {
    vote <- votes[[i]]
    if (is.list(vote[[3]])) {
      member_name[i] <- vote[[3]][[1]]
    } else {
      member_name[i] <- vote[[3]]
    }
    
    # row number in uk_lower_members
    num = which(str_detect(members$full_name,member_name[i]))[1]
    if (!is_empty(num)) {
      member_ID[i] <- members$member_path[num]
      constituency_name[i] <- members$constituency_name[num]
      constituency_ID[i] <- members$constituency_ID[num]
    }
    
    yea[i] <- str_detect(vote[[4]],"Aye")
  }
  nay = !yea
  vote_i <- data.frame(division_number,division_date,member_name,member_ID,constituency_name,constituency_ID,yea,nay)
  
  return(vote_i)
}

# loop through XML files
div_votes <- NULL
for (i in 1:length(f_xml)) {
  temp <- parse_divisions(i)
  div_votes <- rbind(div_votes,temp)
}

# # save temporary output
# setwd("..")
# write.csv(div_votes,"temp_exp.csv")

# organize API outputs
out <- div_votes %>%
  mutate(yea=as.logical(yea)) %>%
  mutate(date_char=as.character(division_date))

# add parliament number  
out$parliament_number[out$date_char>"2017-06-08"]=57
out$parliament_number[out$date_char>"2015-05-07" & out$date_char<="2017-06-08"]=56
out$parliament_number[out$date_char>"2010-05-06" & out$date_char<="2015-05-07"]=55
out$parliament_number[out$date_char<="2010-05-06" & out$date_char>"2005-05-05"]=54
out$parliament_number[out$date_char<="2005-05-05"]=53

# data cleaning and reformat
out_div_exp <- out %>%
  mutate(parliament_path = paste0("/parliament-",parliament_number)) %>%
  mutate(chamber_number=1) %>%
  arrange(division_date) %>%
  mutate(parliament_number = as.factor(parliament_number)) %>%
  group_by(parliament_number) %>%
  dplyr::mutate(session_number = row_number()) %>%
  mutate(session_path = paste0(parliament_path, "/session-", session_number)) %>%
  mutate(chamber_path = paste0(session_path,"/chamber-",chamber_number)) %>%
  mutate(division_path = paste0(chamber_path,"/division-",division_number)) %>%
  mutate(observation_path = division_path) %>%
  arrange(parliament_number, session_number, chamber_number, division_number) %>%
  mutate(observation_number = 1:nrow(out))

# data output
div_exp <- out_div_exp %>%
  select(observation_path, parliament_path, session_path, chamber_path, division_path, vote_path,
         observation_number, parliament_number, session_number, chamber_number, division_number, vote_number,
         division_date = date_char, member_name, member_ID, constituency_name, constituency_ID, yea, nay)

write.csv(div_exp, "uk_lower_divisions_expanded.csv")