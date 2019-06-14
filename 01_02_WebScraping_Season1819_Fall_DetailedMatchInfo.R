## LOAD SEASON 2018/19 DETAILED MATCH DATA FROM CHF.CZ WEBSITE
# Fall.1819.match.info.extended is expected to be present in workspace

## 1. load packages
library(xml2)
library(rvest)
library(lubridate)
library(stringi)
library(stringr)
library(dplyr)

## 2. Get ready the dataframe structure with variables expected to be extracted from the webpage
Fall.1819.match.detailed.info <- data.frame(Fall.1819.match.info.extended,
                                  "Halftime.Score" = rep(NA, nrow(Fall.1819.match.info.extended)),
                                  "Attendance" = rep(NA, nrow(Fall.1819.match.info.extended)),
                                  "Seven.m.goals" = rep(NA, nrow(Fall.1819.match.info.extended)),
                                  "Referees" = rep(NA, nrow(Fall.1819.match.info.extended)),
                                  "Home.Coach" = rep(NA, nrow(Fall.1819.match.info.extended)),
                                  "Home.Assistant" = rep(NA, nrow(Fall.1819.match.info.extended)),
                                  "Away.Coach" = rep(NA, nrow(Fall.1819.match.info.extended)),
                                  "Away.Assistant" = rep(NA, nrow(Fall.1819.match.info.extended)))

## 3. Do the webscraping - find separate webpages
links.lst <- "http://www.extraligahazene.cz/tables/results/3071"

# 3.1 Read the webpage and find relevant nodes to be read further
links.site <- read_html(links.lst)
links <- links.site %>% html_nodes(".results") %>% html_children() %>%
  html_nodes(".text-center") %>% html_children() %>%  html_attr("href")
links <- paste0("http://www.extraligahazene.cz", links)
   
## 3. Do the webscraping - extract info from separate webpages

# 3.1. Nahrani prvnich jedenacti zapasu - 12ty zapas je odlozeny zapas Dukla - Karvina, ktery se bude hrat 20.02.
for (match.ID.i in 1:length(links)) {
  links.site.detail <- read_html(links[match.ID.i])

  # 3.1.1. Get match detailed info
  links.detail.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="container-wrapper"]') %>%
    html_children() %>% html_nodes(xpath='//table[@id="record-match-info"]') %>% html_text()
  links.detail.match.info <- gsub("\n                ", ";",links.detail.match.info)
  links.detail.match.info <- gsub("\n            ", ";",links.detail.match.info)
  details.parsed <- strsplit(links.detail.match.info,";")
  details.parsed <- as.data.frame(details.parsed, stringsAsFactors = FALSE)
  details.parsed <- as.data.frame(details.parsed[-c(1,3,5,7,9,10,11,12),], stringsAsFactors = FALSE)
  names(details.parsed)[1] <- "Fall.1819.match.detailed.Data"

  # 3.1.2. Insert extracted data into the reference dataset
  Fall.1819.match.detailed.info$Halftime.Score[match.ID.i] <- details.parsed$Fall.1819.match.detailed.Data[1]
  Fall.1819.match.detailed.info$Attendance[match.ID.i] <- details.parsed$Fall.1819.match.detailed.Data[2]
  Fall.1819.match.detailed.info$Seven.m.goals[match.ID.i] <- details.parsed$Fall.1819.match.detailed.Data[3]
  Fall.1819.match.detailed.info$Referees[match.ID.i] <- details.parsed$Fall.1819.match.detailed.Data[4]
  
  # 3.1.3. Get coach and assistant data
  links.coach.info <- links.site.detail %>% html_nodes(xpath='//div[@id="container-wrapper"]') %>%
    html_children() %>% html_nodes(xpath='//div[@class="treners-wrapper mobile-block"]') %>% html_children() %>%
    html_text()
  links.coach.info <- links.coach.info[c(2,4,6,8)]
  links.coach.info <- gsub("\n                ", "",links.coach.info)
  links.coach.info <- gsub("\n            ", "",links.coach.info)
  
  # 3.1.4. Insert extracted coach data into the reference dataset
  Fall.1819.match.detailed.info$Home.Coach[match.ID.i] <- links.coach.info[1]
  Fall.1819.match.detailed.info$Home.Assistant[match.ID.i] <- links.coach.info[3]
  Fall.1819.match.detailed.info$Away.Coach[match.ID.i] <- links.coach.info[2]
  Fall.1819.match.detailed.info$Away.Assistant[match.ID.i] <- links.coach.info[4]
}

# # 3.2. Nahrani dat pro zapasy 13-66
# for (match.ID.i in 12:length(links)) {
#   links.site.detail <- read_html(links[match.ID.i])
#   
#   # 3.2.1. Get match detailed info
#   links.detail.match.info <- links.site.detail %>% html_nodes(xpath='//div[@id="container-wrapper"]') %>%
#     html_children() %>% html_nodes(xpath='//table[@id="record-match-info"]') %>% html_text()
#   links.detail.match.info <- gsub("\n                ", ";",links.detail.match.info)
#   links.detail.match.info <- gsub("\n            ", ";",links.detail.match.info)
#   details.parsed <- strsplit(links.detail.match.info,";")
#   details.parsed <- as.data.frame(details.parsed, stringsAsFactors = FALSE)
#   details.parsed <- as.data.frame(details.parsed[-c(1,3,5,7,9,10,11,12),], stringsAsFactors = FALSE)
#   names(details.parsed)[1] <- "Fall.1819.match.detailed.Data"
#   
#   # 3.2.2. Insert extracted data into the reference dataset
#   Fall.1819.match.detailed.info$Halftime.Score[match.ID.i+1] <- details.parsed$Fall.1819.match.detailed.Data[1]
#   Fall.1819.match.detailed.info$Attendance[match.ID.i+1] <- details.parsed$Fall.1819.match.detailed.Data[2]
#   Fall.1819.match.detailed.info$Seven.m.goals[match.ID.i+1] <- details.parsed$Fall.1819.match.detailed.Data[3]
#   Fall.1819.match.detailed.info$Referees[match.ID.i+1] <- details.parsed$Fall.1819.match.detailed.Data[4]
#   
#   # 3.2.3. Get coach and assistant data
#   links.coach.info <- links.site.detail %>% html_nodes(xpath='//div[@id="container-wrapper"]') %>%
#     html_children() %>% html_nodes(xpath='//div[@class="treners-wrapper mobile-block"]') %>% html_children() %>%
#     html_text()
#   links.coach.info <- links.coach.info[c(2,4,6,8)]
#   links.coach.info <- gsub("\n                ", "",links.coach.info)
#   links.coach.info <- gsub("\n            ", "",links.coach.info)
#   
#   # 3.2.4. Insert extracted coach data into the reference dataset
#   Fall.1819.match.detailed.info$Home.Coach[match.ID.i+1] <- links.coach.info[1]
#   Fall.1819.match.detailed.info$Home.Assistant[match.ID.i+1] <- links.coach.info[3]
#   Fall.1819.match.detailed.info$Away.Coach[match.ID.i+1] <- links.coach.info[2]
#   Fall.1819.match.detailed.info$Away.Assistant[match.ID.i+1] <- links.coach.info[4]
# }

## 4. Create separate columns for Home.Team.Halftime.Goals a Away.Team.Halftime.Goals
Fall.1819.match.detailed.info <- data.frame(Fall.1819.match.detailed.info,
                                  "Home.Team.Halftime.Goals" = rep(NA, nrow(Fall.1819.match.detailed.info)),
                                  "Away.Team.Halftime.Goals" = rep(NA, nrow(Fall.1819.match.detailed.info)))

Halftime.parsed <- strsplit(Fall.1819.match.detailed.info$Halftime.Score,":")
for (i in 1:length(Fall.1819.match.detailed.info$Halftime.Score)) {
  Fall.1819.match.detailed.info$Home.Team.Halftime.Goals[i] <- as.numeric(Halftime.parsed[[i]][1])
  Fall.1819.match.detailed.info$Away.Team.Halftime.Goals[i] <- as.numeric(Halftime.parsed[[i]][2])
}
Fall.1819.match.detailed.info <- Fall.1819.match.detailed.info %>% select(-Halftime.Score) #Remove unneccessary column "Halftime.Score"

## 5. Create separate columns for Home.Team.7m.Goals a Away.Team.7m.Goals
Fall.1819.match.detailed.info <- data.frame(Fall.1819.match.detailed.info,
                                  "Home.Team.7m.Goals" = rep(NA, nrow(Fall.1819.match.detailed.info)),
                                  "Away.Team.7m.Goals" = rep(NA, nrow(Fall.1819.match.detailed.info)))

Seven.m.parsed <- strsplit(Fall.1819.match.detailed.info$Seven.m.goals,":")
for (i in 1:length(Fall.1819.match.detailed.info$Seven.m.goals)) {
  Fall.1819.match.detailed.info$Home.Team.7m.Goals[i] <- as.numeric(Seven.m.parsed[[i]][1])
  Fall.1819.match.detailed.info$Away.Team.7m.Goals[i] <- as.numeric(Seven.m.parsed[[i]][2])
}
Fall.1819.match.detailed.info <- Fall.1819.match.detailed.info %>% select(-Seven.m.goals) #Remove unneccessary column "Halftime.Score"

#### Save as csv ####
write.csv(Fall.1819.match.detailed.info, file = "csv_detailed_match_info/Fall.1819.match.detailed.info.csv",row.names=FALSE, na="")

## 6. Rename first column to "Fall.1819.Match.ID"
names(Fall.1819.match.detailed.info)[1] <- "Local.Match.ID"
## 6. Delete unneccessary structures from the workspace
rm(match.ID.i, links.lst, links.coach.info, links, links.site,
   links.site.detail, details.parsed, links.detail.match.info,
   Halftime.parsed, Seven.m.parsed, i)
