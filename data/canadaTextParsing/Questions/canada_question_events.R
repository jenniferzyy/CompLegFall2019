# Set working directory
setwd("~/GitHub/CompLegFall2019/data/canadaTextParsing/Questions")

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

# load questions
q <- read.csv("canada_questions_updated.csv", stringsAsFactors = F) %>%
  select("parliament_ID", "session_ID", "item_ID", "item_number", "member_number", "full_name", "constituency") %>%
  distinct() %>%
  arrange(item_ID)

# import
link <- read_html(url("https://www.ourcommons.ca/DocumentViewer/en/42-1/house/status-business/page-12"))

# parse elements

# Text paragraphs
text <- link %>%
  html_nodes(".ReviewItemEntry") %>%
  html_text()

# Question numbers
qnum <- text %>%
  str_extract("Q-\\d+") %>%
  str_remove("2$") %>%
  na.locf()

# Event
events <- text %>%
  str_replace("(.*)( - [A-Z][a-z]+\\s[0-9]{1,2}, [0-9]{4})","\\1") %>%
  str_remove(".* - ") %>%
  str_remove(" \\(.*\\)")

# Date
date <- text %>%
  str_extract("[A-Z][a-z]+\\s[0-9]{1,2}, [0-9]{4}") %>%
  mdy() %>%
  str_replace_all("-","/")

# Data Frame
df <- data.frame(item_number = qnum,
                 date = date,
                 event = events) %>%
  group_by(item_number) %>%
  dplyr::mutate(event_ID = row_number()) #dplyr:: is necessary in front of mutate since it also appears in other packages and would cause "should only be called in a data context" error

# Merge
qeve <- merge(df, q, by = "item_number") %>%
  mutate(num = as.numeric(str_extract(item_number, "\\d+"))) %>%
  arrange(num) %>%
  mutate(key_ID = 1:length(num))
colnames(qeve)[1] = "question_number"
colnames(qeve)[7] = "question_ID"
colnames(qeve)[8] = "member_ID"
colnames(qeve)[9] = "member_name"
qevents <- qeve %>%
  select("key_ID","parliament_ID",	"session_ID",	"question_ID",	"question_number", "member_ID",	"member_name",	"constituency",	"event_ID",	"date",	"event")

# Output
write.csv(qevents, "canada_question_events_updated.csv", fileEncoding = "UTF-8")
