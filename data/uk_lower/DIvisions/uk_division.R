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

# Construct URL
url <- "http://lda.data.parliament.uk/commonsdivisions/"
id <- str_extract(div$uri, "\\d{6,7}")
link <- paste0(url,id,".xml")
bills <- read.csv("bill.csv", stringsAsFactors = F)

# Initiate variables
yea = 0
nay = 0
didnot = 0
division_number = 0
session_date = 0
result = 0
bill_ID = 0

# Call API
for (i in 1724:length(id)) {
  content <- xmlToList(xmlParse(link[i]))[[3]]
  yea[i] <- as.numeric(content[[3]][[1]])
  nay[i] <- as.numeric(content[[8]][[1]])
  didnot[i] <- as.numeric(content[[5]][[1]])
  
  # starting from 03/26/2015, description is added to xml and affects the later orders
  if(i<=719) {
    division_number[i] <- content[[12]]
    session_date[i] <- content[[14]][[1]]
  }
  if(i>719) {
    division_number[i] <- content[[13]]
    session_date[i] <- content[[15]][[1]]
  }
  result[i] <- c("Agreed to", "Negatived to")[which.max(c(yea[i],nay[i]))]
  index <- str_which(div$title[i],bills$bill_title)
  if(!is_empty(index)){
    bill_ID[i] <- bills$bill_path[index[which.max(nchar(bills$bill_title[index]))]]
  }
}

# remove difference in row numbers
bill_ID[i]=0
bill_ID[i]=NA

# organize API outputs
out <- data.frame(div, yea, nay, didnot, session_date, division_number, result, bill_ID) %>%
  mutate(date_char=as.character(date))

# add parliament number  
out$parliament_number[out$date_char>"2017-06-08"]=57
out$parliament_number[out$date_char>"2015-05-07" & out$date_char<="2017-06-08"]=56
out$parliament_number[out$date_char>"2010-05-06" & out$date_char<="2015-05-07"]=55
out$parliament_number[out$date_char<="2010-05-06" & out$date_char>"2005-05-05"]=54
out$parliament_number[out$date_char<="2005-05-05"]=53

# save temporary result (in case of R session termination)
# write.csv(out,"temp.csv")

out_div <- out %>%
  mutate(parliament_path = paste0("/parliament-",parliament_number)) %>%
  group_by(parliament_number) %>%
  arrange(session_date) %>%
  mutate(session_number = 1:length(parliament_number))
