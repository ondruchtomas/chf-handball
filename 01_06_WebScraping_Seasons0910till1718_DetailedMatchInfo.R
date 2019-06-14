###### Load packages ####
library(xml2)
library(rvest)
library(lubridate)
library(stringi)
library(stringr)
library(dplyr)
library(readr)

#### Load data from csv files now, so that webscraping is not performed again ####

Season.0910.match.info <- read_csv("csv_basic_match_info/Season0910_basic_info.csv", 
                                   locale = locale(encoding = "windows-1252"))
Season.1011.match.info <- read_csv("csv_basic_match_info/Season1011_basic_info.csv", 
                                   locale = locale(encoding = "windows-1252"))
Season.1112.match.info <- read_csv("csv_basic_match_info/Season1112_basic_info.csv", 
                                   locale = locale(encoding = "windows-1252"))
Season.1213.match.info <- read_csv("csv_basic_match_info/Season1213_basic_info.csv", 
                                   locale = locale(encoding = "windows-1252"))
Season.1314.match.info <- read_csv("csv_basic_match_info/Season1314_basic_info.csv", 
                                   locale = locale(encoding = "windows-1252"))
Season.1415.match.info <- read_csv("csv_basic_match_info/Season1415_basic_info.csv", 
                                   locale = locale(encoding = "windows-1252"))
Season.1516.match.info <- read_csv("csv_basic_match_info/Season1516_basic_info.csv", 
                                   locale = locale(encoding = "windows-1252"))
Season.1617.match.info <- read_csv("csv_basic_match_info/Season1617_basic_info.csv", 
                                   locale = locale(encoding = "windows-1252"))
Season.1718.match.info <- read_csv("csv_basic_match_info/Season1718_basic_info.csv", 
                                   locale = locale(encoding = "windows-1252"))



#### 1718 DETAILED INFO - COMPLETED ####
Season.1718.detailed <- Season.1718.match.info
Season.1718.detailed <- data.frame(Season.1718.detailed,
                                    "Match.Date" = rep(NA, nrow(Season.1718.detailed)),
                                    "Halftime.Score" = rep(NA, nrow(Season.1718.detailed)),
                                    "Attendance" = rep(NA, nrow(Season.1718.detailed)),
                                    "Seven.m.goals" = rep(NA, nrow(Season.1718.detailed)),
                                    "Referees" = rep(NA, nrow(Season.1718.detailed)),
                                    "Home.Coach" = rep(NA, nrow(Season.1718.detailed)),
                                    "Home.Assistant" = rep(NA, nrow(Season.1718.detailed)),
                                    "Away.Coach" = rep(NA, nrow(Season.1718.detailed)),
                                    "Away.Assistant" = rep(NA, nrow(Season.1718.detailed)),
                                    "Phase" = rep(NA, nrow(Season.1718.detailed)))

#### Final matches ####
## Find separate webpages
links.lst <- "http://muzi.2017.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=3023&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2017.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
match.season.id <- 0
for (match.ID.i in 1:length(links)) {
match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                      length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1718.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1718.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1718.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1718.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1718.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1718.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1718.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1718.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1718.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1718.detailed$Phase[match.season.id] <- "FinalsMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
  

#### 3rd place matches ####
## Find separate webpages
links.lst <- "http://muzi.2017.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=3024&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2017.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1718.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1718.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1718.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1718.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1718.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1718.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1718.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1718.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1718.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1718.detailed$Phase[match.season.id] <- "3rdPlaceMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)

#### 5th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2017.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=3025&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2017.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1718.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1718.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1718.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1718.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1718.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1718.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1718.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1718.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1718.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1718.detailed$Phase[match.season.id] <- "5thPlaceMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)




#### 7th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2017.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=3026&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2017.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1718.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1718.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1718.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1718.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1718.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1718.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1718.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1718.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1718.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1718.detailed$Phase[match.season.id] <- "7thPlaceMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)

#### Semifinal matches ####
## Find separate webpages
links.lst <- "http://muzi.2017.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=3017&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2017.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1718.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1718.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1718.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1718.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1718.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1718.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1718.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1718.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1718.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1718.detailed$Phase[match.season.id] <- "SemifinalsMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### 5-8th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2017.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=3018&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2017.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1718.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1718.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1718.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1718.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1718.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1718.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1718.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1718.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1718.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1718.detailed$Phase[match.season.id] <- "5till8placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### Quarterfinals matches ####
## Find separate webpages
links.lst <- "http://muzi.2017.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=3015&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2017.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1718.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1718.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1718.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1718.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1718.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1718.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1718.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1718.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1718.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1718.detailed$Phase[match.season.id] <- "QuarterfinalsMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### 9-12th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2017.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=3016&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2017.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1718.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1718.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1718.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1718.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1718.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1718.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1718.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1718.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1718.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1718.detailed$Phase[match.season.id] <- "9till12placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### Classic season matches ####
## Find separate webpages
links.lst <- "http://muzi.2017.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2870&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2017.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1718.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1718.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1718.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1718.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1718.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1718.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1718.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1718.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1718.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1718.detailed$Phase[match.season.id] <- "RegularPhaseMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### Save as csv ####
write.csv(Season.1718.detailed, file = "csv_detailed_match_info/Season1718_detailed_info.csv",row.names=FALSE, na="")


#### 1617 DETAILED INFO - COMPLETED ####
Season.1617.detailed <- Season.1617.match.info
Season.1617.detailed <- data.frame(Season.1617.detailed,
                                   "Match.Date" = rep(NA, nrow(Season.1617.detailed)),
                                   "Halftime.Score" = rep(NA, nrow(Season.1617.detailed)),
                                   "Attendance" = rep(NA, nrow(Season.1617.detailed)),
                                   "Seven.m.goals" = rep(NA, nrow(Season.1617.detailed)),
                                   "Referees" = rep(NA, nrow(Season.1617.detailed)),
                                   "Home.Coach" = rep(NA, nrow(Season.1617.detailed)),
                                   "Home.Assistant" = rep(NA, nrow(Season.1617.detailed)),
                                   "Away.Coach" = rep(NA, nrow(Season.1617.detailed)),
                                   "Away.Assistant" = rep(NA, nrow(Season.1617.detailed)),
                                   "Phase" = rep(NA, nrow(Season.1617.detailed)))

#### Final matches ####
## Find separate webpages
links.lst <- "http://muzi.2016.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2831&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2016.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
match.season.id <- 0
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1617.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1617.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1617.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1617.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1617.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1617.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1617.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1617.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1617.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1617.detailed$Phase[match.season.id] <- "FinalsMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)


#### 3rd place matches ####
## Find separate webpages
links.lst <- "http://muzi.2016.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2832&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2016.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1617.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1617.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1617.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1617.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1617.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1617.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1617.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1617.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1617.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1617.detailed$Phase[match.season.id] <- "3rdPlaceMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)

#### 5th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2016.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2833&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2016.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1617.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1617.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1617.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1617.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1617.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1617.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1617.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1617.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1617.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1617.detailed$Phase[match.season.id] <- "5thPlaceMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)




#### 7th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2016.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2834&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2016.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1617.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1617.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1617.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1617.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1617.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1617.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1617.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1617.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1617.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1617.detailed$Phase[match.season.id] <- "7thPlaceMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)

#### Semifinal matches ####
## Find separate webpages
links.lst <- "http://muzi.2016.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2820&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2016.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1617.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1617.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1617.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1617.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1617.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1617.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1617.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1617.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1617.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1617.detailed$Phase[match.season.id] <- "SemifinalsMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### 5-8th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2016.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2821&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2016.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1617.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1617.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1617.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1617.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1617.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1617.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1617.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1617.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1617.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1617.detailed$Phase[match.season.id] <- "5till8placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### Quarterfinals matches ####
## Find separate webpages
links.lst <- "http://muzi.2016.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2818&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2016.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1617.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1617.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1617.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1617.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1617.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1617.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1617.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1617.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1617.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1617.detailed$Phase[match.season.id] <- "QuarterfinalsMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### 9-12th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2016.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2819&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2016.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1617.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1617.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1617.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1617.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1617.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1617.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1617.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1617.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1617.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1617.detailed$Phase[match.season.id] <- "9till12placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### Classic season matches ####
## Find separate webpages
links.lst <- "http://muzi.2016.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2677&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2016.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1617.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1617.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1617.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1617.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1617.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1617.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1617.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1617.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1617.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1617.detailed$Phase[match.season.id] <- "RegularPhaseMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### Save as csv ####
write.csv(Season.1617.detailed, file = "csv_detailed_match_info/Season1617_detailed_info.csv",row.names=FALSE, na="")












#### 1516 DETAILED INFO - COMPLETED ####
Season.1516.detailed <- Season.1516.match.info
Season.1516.detailed <- data.frame(Season.1516.detailed,
                                   "Match.Date" = rep(NA, nrow(Season.1516.detailed)),
                                   "Halftime.Score" = rep(NA, nrow(Season.1516.detailed)),
                                   "Attendance" = rep(NA, nrow(Season.1516.detailed)),
                                   "Seven.m.goals" = rep(NA, nrow(Season.1516.detailed)),
                                   "Referees" = rep(NA, nrow(Season.1516.detailed)),
                                   "Home.Coach" = rep(NA, nrow(Season.1516.detailed)),
                                   "Home.Assistant" = rep(NA, nrow(Season.1516.detailed)),
                                   "Away.Coach" = rep(NA, nrow(Season.1516.detailed)),
                                   "Away.Assistant" = rep(NA, nrow(Season.1516.detailed)),
                                   "Phase" = rep(NA, nrow(Season.1516.detailed)))

#### Final matches ####
## Find separate webpages
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2627&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2015.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
match.season.id <- 0
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1516.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1516.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1516.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1516.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1516.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1516.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1516.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1516.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1516.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1516.detailed$Phase[match.season.id] <- "FinalsMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)


#### 3rd place matches ####
## Find separate webpages
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2628&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2015.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1516.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1516.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1516.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1516.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1516.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1516.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1516.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1516.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1516.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1516.detailed$Phase[match.season.id] <- "3rdPlaceMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)

#### 5th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2619&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2015.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1516.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1516.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1516.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1516.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1516.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1516.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1516.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1516.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1516.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1516.detailed$Phase[match.season.id] <- "5thPlaceMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)




#### 7th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2620&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2015.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1516.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1516.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1516.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1516.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1516.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1516.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1516.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1516.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1516.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1516.detailed$Phase[match.season.id] <- "7thPlaceMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)

#### Semifinal matches ####
## Find separate webpages
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2617&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2015.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1516.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1516.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1516.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1516.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1516.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1516.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1516.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1516.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1516.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1516.detailed$Phase[match.season.id] <- "SemifinalsMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### 5-8th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2618&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2015.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1516.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1516.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1516.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1516.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1516.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1516.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1516.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1516.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1516.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1516.detailed$Phase[match.season.id] <- "5till8placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### Quarterfinals matches ####
## Find separate webpages
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2606&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2015.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1516.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1516.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1516.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1516.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1516.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1516.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1516.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1516.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1516.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1516.detailed$Phase[match.season.id] <- "QuarterfinalsMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### 9th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2616&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2015.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1516.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1516.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1516.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1516.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1516.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1516.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1516.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1516.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1516.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1516.detailed$Phase[match.season.id] <- "9till12placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### 11th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2615&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2015.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1516.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1516.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1516.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1516.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1516.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1516.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1516.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1516.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1516.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1516.detailed$Phase[match.season.id] <- "9till12placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### 9-12th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2607&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2015.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1516.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1516.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1516.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1516.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1516.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1516.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1516.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1516.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1516.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1516.detailed$Phase[match.season.id] <- "9till12placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### Classic season matches ####
## Find separate webpages
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2453&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2015.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1516.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1516.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1516.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1516.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1516.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1516.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1516.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1516.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1516.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1516.detailed$Phase[match.season.id] <- "RegularPhaseMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### Save as csv ####
write.csv(Season.1516.detailed, file = "csv_detailed_match_info/Season1516_detailed_info.csv",row.names=FALSE, na="")
















#### 1415 DETAILED INFO - COMPLETED ####
Season.1415.detailed <- Season.1415.match.info
Season.1415.detailed <- data.frame(Season.1415.detailed,
                                   "Match.Date" = rep(NA, nrow(Season.1415.detailed)),
                                   "Halftime.Score" = rep(NA, nrow(Season.1415.detailed)),
                                   "Attendance" = rep(NA, nrow(Season.1415.detailed)),
                                   "Seven.m.goals" = rep(NA, nrow(Season.1415.detailed)),
                                   "Referees" = rep(NA, nrow(Season.1415.detailed)),
                                   "Home.Coach" = rep(NA, nrow(Season.1415.detailed)),
                                   "Home.Assistant" = rep(NA, nrow(Season.1415.detailed)),
                                   "Away.Coach" = rep(NA, nrow(Season.1415.detailed)),
                                   "Away.Assistant" = rep(NA, nrow(Season.1415.detailed)),
                                   "Phase" = rep(NA, nrow(Season.1415.detailed)))

#### Final matches ####
## Find separate webpages
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2431&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2014.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
match.season.id <- 0
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1415.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1415.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1415.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1415.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1415.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1415.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1415.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1415.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1415.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1415.detailed$Phase[match.season.id] <- "FinalsMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)


#### 3rd place matches ####
## Find separate webpages
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2432&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2014.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1415.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1415.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1415.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1415.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1415.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1415.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1415.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1415.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1415.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1415.detailed$Phase[match.season.id] <- "3rdPlaceMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)

#### 5th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2433&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2014.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1415.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1415.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1415.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1415.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1415.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1415.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1415.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1415.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1415.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1415.detailed$Phase[match.season.id] <- "5thPlaceMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)




#### 7th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2434&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2014.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1415.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1415.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1415.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1415.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1415.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1415.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1415.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1415.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1415.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1415.detailed$Phase[match.season.id] <- "7thPlaceMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)

#### Semifinal matches ####
## Find separate webpages
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2426&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2014.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1415.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1415.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1415.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1415.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1415.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1415.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1415.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1415.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1415.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1415.detailed$Phase[match.season.id] <- "SemifinalsMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### 5-8th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2429&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2014.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1415.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1415.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1415.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1415.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1415.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1415.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1415.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1415.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1415.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1415.detailed$Phase[match.season.id] <- "5till8placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### Quarterfinals matches ####
## Find separate webpages
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2421&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2014.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1415.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1415.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1415.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1415.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1415.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1415.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1415.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1415.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1415.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1415.detailed$Phase[match.season.id] <- "QuarterfinalsMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### 9th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2430&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2014.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1415.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1415.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1415.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1415.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1415.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1415.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1415.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1415.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1415.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1415.detailed$Phase[match.season.id] <- "9till12placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### 11th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2427&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2014.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1415.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1415.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1415.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1415.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1415.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1415.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1415.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1415.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1415.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1415.detailed$Phase[match.season.id] <- "9till12placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### 9-12th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2422&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2014.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1415.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1415.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1415.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1415.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1415.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1415.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1415.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1415.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1415.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1415.detailed$Phase[match.season.id] <- "9till12placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### Classic season matches ####
## Find separate webpages
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2284&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2014.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1415.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1415.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1415.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1415.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1415.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1415.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1415.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1415.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1415.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1415.detailed$Phase[match.season.id] <- "RegularPhaseMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### Save as csv ####
write.csv(Season.1415.detailed, file = "csv_detailed_match_info/Season1415_detailed_info.csv",row.names=FALSE, na="")

















#### 1314 DETAILED INFO - COMPLETED ####
Season.1314.detailed <- Season.1314.match.info
Season.1314.detailed <- data.frame(Season.1314.detailed,
                                   "Match.Date" = rep(NA, nrow(Season.1314.detailed)),
                                   "Halftime.Score" = rep(NA, nrow(Season.1314.detailed)),
                                   "Attendance" = rep(NA, nrow(Season.1314.detailed)),
                                   "Seven.m.goals" = rep(NA, nrow(Season.1314.detailed)),
                                   "Referees" = rep(NA, nrow(Season.1314.detailed)),
                                   "Home.Coach" = rep(NA, nrow(Season.1314.detailed)),
                                   "Home.Assistant" = rep(NA, nrow(Season.1314.detailed)),
                                   "Away.Coach" = rep(NA, nrow(Season.1314.detailed)),
                                   "Away.Assistant" = rep(NA, nrow(Season.1314.detailed)),
                                   "Phase" = rep(NA, nrow(Season.1314.detailed)))

#### 1-6 place matches ####
## Find separate webpages
links.lst <- "http://muzi.2013.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2230&roundindex=10"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2013.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
match.season.id <- 0
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1314.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1314.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1314.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1314.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1314.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1314.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1314.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1314.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1314.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1314.detailed$Phase[match.season.id] <- "1till6placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)


#### 7-12 place matches ####
## Find separate webpages
links.lst <- "http://muzi.2013.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2231&roundindex=10"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2013.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1314.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1314.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1314.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1314.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1314.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1314.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1314.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1314.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1314.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1314.detailed$Phase[match.season.id] <- "7till12placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)

#### Classic season matches ####
## Find separate webpages
links.lst <- "http://muzi.2013.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2039&roundindex=10"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2013.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1314.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1314.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1314.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1314.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1314.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1314.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1314.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1314.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1314.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1314.detailed$Phase[match.season.id] <- "RegularPhaseMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### Save as csv ####
write.csv(Season.1314.detailed, file = "csv_detailed_match_info/Season1314_detailed_info.csv",row.names=FALSE, na="")

















#### 1213 DETAILED INFO - COMPLETED ####
Season.1213.detailed <- Season.1213.match.info
Season.1213.detailed <- data.frame(Season.1213.detailed,
                                   "Match.Date" = rep(NA, nrow(Season.1213.detailed)),
                                   "Halftime.Score" = rep(NA, nrow(Season.1213.detailed)),
                                   "Attendance" = rep(NA, nrow(Season.1213.detailed)),
                                   "Seven.m.goals" = rep(NA, nrow(Season.1213.detailed)),
                                   "Referees" = rep(NA, nrow(Season.1213.detailed)),
                                   "Home.Coach" = rep(NA, nrow(Season.1213.detailed)),
                                   "Home.Assistant" = rep(NA, nrow(Season.1213.detailed)),
                                   "Away.Coach" = rep(NA, nrow(Season.1213.detailed)),
                                   "Away.Assistant" = rep(NA, nrow(Season.1213.detailed)),
                                   "Phase" = rep(NA, nrow(Season.1213.detailed)))

#### 1-6 place matches ####
## Find separate webpages
links.lst <- "http://muzi.2012.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=1953&roundindex=10"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2012.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
match.season.id <- 0
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1213.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1213.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1213.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1213.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1213.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1213.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1213.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1213.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1213.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1213.detailed$Phase[match.season.id] <- "1till6placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)


#### 7-12 place matches ####
## Find separate webpages
links.lst <- "http://muzi.2012.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=1954&roundindex=10"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2012.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1213.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1213.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1213.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1213.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1213.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1213.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1213.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1213.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1213.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1213.detailed$Phase[match.season.id] <- "7till12placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)

#### Classic season matches ####
## Find separate webpages
links.lst <- "http://muzi.2012.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=1455&roundindex=10"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2012.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1213.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1213.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1213.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1213.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1213.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1213.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1213.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1213.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1213.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1213.detailed$Phase[match.season.id] <- "RegularPhaseMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### Save as csv ####
write.csv(Season.1213.detailed, file = "csv_detailed_match_info/Season1213_detailed_info.csv",row.names=FALSE, na="")

















#### 1112 DETAILED INFO - NOT COMPLETED ####
Season.1112.detailed <- Season.1112.match.info
Season.1112.detailed <- data.frame(Season.1112.detailed,
                                   "Match.Date" = rep(NA, nrow(Season.1112.detailed)),
                                   "Halftime.Score" = rep(NA, nrow(Season.1112.detailed)),
                                   "Attendance" = rep(NA, nrow(Season.1112.detailed)),
                                   "Seven.m.goals" = rep(NA, nrow(Season.1112.detailed)),
                                   "Referees" = rep(NA, nrow(Season.1112.detailed)),
                                   "Home.Coach" = rep(NA, nrow(Season.1112.detailed)),
                                   "Home.Assistant" = rep(NA, nrow(Season.1112.detailed)),
                                   "Away.Coach" = rep(NA, nrow(Season.1112.detailed)),
                                   "Away.Assistant" = rep(NA, nrow(Season.1112.detailed)),
                                   "Phase" = rep(NA, nrow(Season.1112.detailed)))

#### 1-6 place matches ####
## Find separate webpages
links.lst <- "http://muzi.2011.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=1658&roundindex=10"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2011.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
match.season.id <- 0
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1112.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1112.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1112.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1112.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1112.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1112.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1112.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1112.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1112.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1112.detailed$Phase[match.season.id] <- "1till6placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)


#### 7-12 place matches ####
## Find separate webpages
links.lst <- "http://muzi.2011.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=1659&roundindex=10"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2011.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1112.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1112.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1112.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1112.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1112.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1112.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1112.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1112.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1112.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1112.detailed$Phase[match.season.id] <- "7till12placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)

#### Classic season matches ####
## Find separate webpages
links.lst <- "http://muzi.2011.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=1455&roundindex=10"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2011.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1112.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1112.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1112.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1112.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1112.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1112.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1112.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1112.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1112.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1112.detailed$Phase[match.season.id] <- "RegularPhaseMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### Save as csv ####
write.csv(Season.1112.detailed, file = "csv_detailed_match_info/Season1112_detailed_info.csv",row.names=FALSE, na="")

















#### 1011 DETAILED INFO - NOT COMPLETED ####
Season.1011.detailed <- Season.1011.match.info
Season.1011.detailed <- data.frame(Season.1011.detailed,
                                   "Match.Date" = rep(NA, nrow(Season.1011.detailed)),
                                   "Halftime.Score" = rep(NA, nrow(Season.1011.detailed)),
                                   "Attendance" = rep(NA, nrow(Season.1011.detailed)),
                                   "Seven.m.goals" = rep(NA, nrow(Season.1011.detailed)),
                                   "Referees" = rep(NA, nrow(Season.1011.detailed)),
                                   "Home.Coach" = rep(NA, nrow(Season.1011.detailed)),
                                   "Home.Assistant" = rep(NA, nrow(Season.1011.detailed)),
                                   "Away.Coach" = rep(NA, nrow(Season.1011.detailed)),
                                   "Away.Assistant" = rep(NA, nrow(Season.1011.detailed)),
                                   "Phase" = rep(NA, nrow(Season.1011.detailed)))

#### 1-6 place matches - KONTUMACNI ZAPAS ZUBRI-HRANICE (RADEK C.29) - NUTNO DOPLNIT DO BASIC DATAFRAMU ####
## Find separate webpages
links.lst <- "http://muzi.2010.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=1432&roundindex=10"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2010.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
match.season.id <- 0
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1011.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1011.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1011.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1011.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1011.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1011.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1011.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1011.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1011.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1011.detailed$Phase[match.season.id] <- "1till6placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)


#### 7-12 place matches ####
## Find separate webpages
links.lst <- "http://muzi.2010.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=1431&roundindex=10"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2010.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1011.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1011.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1011.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1011.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1011.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1011.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1011.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1011.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1011.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1011.detailed$Phase[match.season.id] <- "7till12placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)

#### Classic season matches ####
## Find separate webpages
links.lst <- "http://muzi.2010.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=95&roundindex=22"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2010.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.1011.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.1011.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.1011.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.1011.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.1011.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.1011.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.1011.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.1011.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.1011.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.1011.detailed$Phase[match.season.id] <- "RegularPhaseMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### Save as csv ####
write.csv(Season.1011.detailed, file = "csv_detailed_match_info/Season1011_detailed_info.csv",row.names=FALSE, na="")
















#### 0910 DETAILED INFO - COMPLETED ####
Season.0910.detailed <- Season.0910.match.info
Season.0910.detailed <- data.frame(Season.0910.detailed,
                                   "Match.Date" = rep(NA, nrow(Season.0910.detailed)),
                                   "Halftime.Score" = rep(NA, nrow(Season.0910.detailed)),
                                   "Attendance" = rep(NA, nrow(Season.0910.detailed)),
                                   "Seven.m.goals" = rep(NA, nrow(Season.0910.detailed)),
                                   "Referees" = rep(NA, nrow(Season.0910.detailed)),
                                   "Home.Coach" = rep(NA, nrow(Season.0910.detailed)),
                                   "Home.Assistant" = rep(NA, nrow(Season.0910.detailed)),
                                   "Away.Coach" = rep(NA, nrow(Season.0910.detailed)),
                                   "Away.Assistant" = rep(NA, nrow(Season.0910.detailed)),
                                   "Phase" = rep(NA, nrow(Season.0910.detailed)))

#### Final matches ####
## Find separate webpages
links.lst <- "http://muzi.2009.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=257&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2009.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
match.season.id <- 0
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.0910.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.0910.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.0910.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.0910.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.0910.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.0910.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.0910.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.0910.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.0910.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.0910.detailed$Phase[match.season.id] <- "FinalsMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)


#### 3rd place matches ####
## Find separate webpages
links.lst <- "http://muzi.2009.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=258&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2009.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.0910.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.0910.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.0910.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.0910.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.0910.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.0910.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.0910.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.0910.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.0910.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.0910.detailed$Phase[match.season.id] <- "3rdPlaceMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)

#### Semifinal matches ####
## Find separate webpages
links.lst <- "http://muzi.2009.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=246&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2009.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.0910.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.0910.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.0910.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.0910.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.0910.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.0910.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.0910.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.0910.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.0910.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.0910.detailed$Phase[match.season.id] <- "SemifinalsMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### 5-8th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2009.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=248&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2009.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.0910.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.0910.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.0910.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.0910.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.0910.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.0910.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.0910.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.0910.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.0910.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.0910.detailed$Phase[match.season.id] <- "5till8placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### Quarterfinals matches ####
## Find separate webpages
links.lst <- "http://muzi.2009.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=238&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2009.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.0910.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.0910.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.0910.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.0910.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.0910.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.0910.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.0910.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.0910.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.0910.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.0910.detailed$Phase[match.season.id] <- "QuarterfinalsMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### 9-12th place matches ####
## Find separate webpages
links.lst <- "http://muzi.2009.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=247&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2009.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.0910.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.0910.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.0910.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.0910.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.0910.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.0910.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.0910.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.0910.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.0910.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.0910.detailed$Phase[match.season.id] <- "9till12placeMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### Classic season matches ####
## Find separate webpages
links.lst <- "http://muzi.2009.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=95&roundindex=1"

# Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_children() %>% html_attr("href")
links <- links[grepl("match_report", links)]
links <- paste0("http://muzi.2009.archiv.chf.cz/", links)

## Do the webscraping - extract info from separate webpages
for (match.ID.i in 1:length(links)) {
  match.season.id <- match.season.id + 1
  links.site.detail <- read_html(links[match.ID.i])
  
  # Get home-coach and assistant info
  home.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl"]') %>%
    html_children() %>% html_children() %>% html_text()
  home.coach.info <- home.coach.info[c(length(home.coach.info)-5,
                                       length(home.coach.info)-2)]
  # Get away-coach and assistant info
  away.coach.info <- links.site.detail %>% html_nodes(xpath='//table[@class="tbl right"]') %>%
    html_children() %>% html_children() %>% html_text()
  away.coach.info <- away.coach.info[c(length(away.coach.info)-3,
                                       length(away.coach.info))]
  
  # Get general match info
  general.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="stats"]') %>%
    html_children() %>% html_children() %>% html_text()
  
  #Insert the extracted info into the season dataset
  Season.0910.detailed$Match.Date[match.season.id] <- strsplit(general.match.info[8]," ")[[1]][1]
  Season.0910.detailed$Halftime.Score[match.season.id] <- general.match.info[1]
  Season.0910.detailed$Attendance[match.season.id] <- general.match.info[4]
  Season.0910.detailed$Seven.m.goals[match.season.id] <- paste0(c(general.match.info[2],general.match.info[3]), collapse = " : ")
  Season.0910.detailed$Referees[match.season.id] <- paste0(c(general.match.info[5],general.match.info[7]), collapse = ", ")
  Season.0910.detailed$Home.Coach[match.season.id] <- home.coach.info[1]
  Season.0910.detailed$Home.Assistant[match.season.id] <- home.coach.info[2]
  Season.0910.detailed$Away.Coach[match.season.id] <- away.coach.info[1]
  Season.0910.detailed$Away.Assistant[match.season.id] <- away.coach.info[2]
  Season.0910.detailed$Phase[match.season.id] <- "RegularPhaseMatch"
}
rm(away.coach.info, home.coach.info, general.match.info,
   links, links.lst, links.site.detail, match.ID.i, links.site)
#### Save as csv ####
write.csv(Season.0910.detailed, file = "csv_detailed_match_info/Season0910_detailed_info.csv",row.names=FALSE, na="")










