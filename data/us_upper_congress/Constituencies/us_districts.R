# Setwd
setwd("~/Documents/GitHub/CompLegFall2019/data/us_upper_congress/Constituencies")

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

# Source
wiki <- read_html("https://en.wikipedia.org/wiki/List_of_United_States_congressional_districts")

# states
state_names <- wiki %>%
  html_nodes(".mw-headline") %>%
  html_text()
state_names <- state_names[7:63]
nonvoting <- c("Alaska","American Samoa","District of Columbia","Guam","Northern Mariana Islands", "Philippines", "Puerto Rico","U.S. Virgin Islands", "Wyoming")
state <- setdiff(state_names,nonvoting)

# districts 
district_brief <- wiki %>%
  html_nodes(".navigation-not-searchable+ ul li") %>%
  html_text()
district_brief[549] = "Dakota Territory: 1861–1889 (obsolete since statehood)"
district_brief[606] = "Territory: (obsolete since statehood)"

# district matching state
j = 1 # state index
districts <- data.frame(district_brief)
districts$state[1]="Alabama"
for (i in 2:length(district_brief)) {
  firstchar <- !is.na(as.numeric(substr(district_brief[i-1],0,1)))
  nextchar <- is.na(as.numeric(substr(district_brief[i],0,1)))
  if(firstchar & nextchar){j = j+1}
  if(i==104){j=j+1}
  districts$state[i]=state[j]
}

# nonvoting states
district_nv <- c("Territory: 1906–1959 (obsolete since statehood)","At-large: 1959–present","Territory: 1869–1890 (obsolete since statehood)","At-large: 1890–present")
state_nv <- c("Alaska","Alaska","Wyoming","Wyoming")
df_nv <- data.frame("district_brief"=district_nv,"state"=state_nv)

districts_df <- rbind(districts,df_nv) %>%
  separate(district_brief,into = c("district","year"),sep = ":") %>%
  mutate(number = str_extract(district, "\\d+")) 
districts_df$number[is.na(districts_df$number)] = districts_df$district[is.na(districts_df$number)]

# organize output
districts_df$district<-NULL
colnames(districts_df)[3]<-"district"

us_district <- districts_df %>%
  arrange(state) %>%
  group_by(state) %>%
  dplyr::mutate(state_number = group_indices()) %>%
  mutate(observation_number = 1:nrow(districts_df)) %>%
  mutate(state_path = paste0("/state-",state_number)) %>%
  mutate(district_path =paste0(state_path,"/district-",district)) %>%
  mutate(observation_path = district_path)

# Output
write.csv(us_district,"us_congress_districts.csv")
