## LOAD SEASON 2018/19 SPRING BASIC MATCH DATA FROM CHF.CZ WEBSITE

## 1. load packages
library(xml2)
library(rvest)
library(lubridate)
library(stringi)
library(stringr)
library(dplyr)

## 2. create dataframe of 2018/19 teams
Team_Name = c("HCB Karviná", "SKKP Handball Brno", "KH Koprivnice", "HC ROBE Zubrí",
              "Talent ROBSTAV-M.A.T.Plzen", "Pepino SKP Frýdek-Místek",
              "TJ Tatran Litovel", "TJ Sokol Nové Veselí", "TJ Cement Hranice",
              "HC Dukla Praha", "HBC Ronal Jicín", "HK FCC Mesto Lovosice")
Team_Init = c("KAR", "BRN", "KOP", "ZUB", "PLZ", "FRM", "LIT", "NOV", "HRA", "DUK",
              "JIC", "LOV")
teams <- data.frame("Team_Name" = Team_Name, "Team_Init" = Team_Init, stringsAsFactors = FALSE)

## 3. scrape CHF basic match data from the website
links.lst <- "http://www.extraligahazene.cz/tables/results/3071?seasonId=5975&part=3071"

# 3.1. Get the expected dataset structure ready
Spring.1819.match.info <- data.frame("Home.Team.Name" = character(),
                         "Result" = character(),
                         "Away.Team.Name" = character(),
                         "Match.Date" = character(),
                         stringsAsFactors = FALSE)

# 3.2. Do the web-scraping
links.site <- read_html(links.lst)
# 3.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol - 3nasobni children jsou entity pro jednotlive zapasy
links.names <- links.site %>% html_nodes(".results") %>% html_children() %>% html_children() %>% html_children() %>% html_text()
# 3.2.2. Odstran znaky "\n                " ze ziskanych dat
links.names <- links.names[which(links.names != "\n                ")]
# 3.2.3. Nahrej nactene udaje ze zapasu do dataframu Spring.1819.match.info
for (i in 1:length(links.names)) {
  Spring.1819.match.info[ceiling(i / 4), ifelse(i %% 4 == 0,4,i %% 4)] <- links.names[i]
}


## 4. Edit the data types

# 4.1. Transform the score-column from string format "XX:YY" into two columns for host and away team
Spring.1819.match.info <- cbind(Spring.1819.match.info, "Home.Team.Goals" = rep(NA, length(Spring.1819.match.info$Result)),
                    "Away.Team.Goals" = rep(NA, length(Spring.1819.match.info$Result)))
Results.parsed <- strsplit(Spring.1819.match.info$Result,":")
for (i in 1:length(Spring.1819.match.info$Result)) {
  Spring.1819.match.info$Home.Team.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Spring.1819.match.info$Away.Team.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Spring.1819.match.info <- Spring.1819.match.info %>% select(-Result) #Remove unneccessary column "Result"

# 4.2. Set the Match.Date datatype as Date
Spring.1819.match.info$Match.Date <- as.Date(Spring.1819.match.info$Match.Date, format = "%d.%m.%Y")

## 5. Edit Czech-language inconsistencies...cannot be fixed by setting Czech encoding
#Koprivnice
Spring.1819.match.info$Home.Team.Name <- gsub(Spring.1819.match.info[3,1],teams[3,1],Spring.1819.match.info$Home.Team.Name)
Spring.1819.match.info$Away.Team.Name <- gsub(Spring.1819.match.info[9,2],teams[3,1],Spring.1819.match.info$Away.Team.Name)
#Zubri
Spring.1819.match.info$Home.Team.Name <- gsub(Spring.1819.match.info[9,1],teams[4,1],Spring.1819.match.info$Home.Team.Name)
Spring.1819.match.info$Away.Team.Name <- gsub(Spring.1819.match.info[4,2],teams[4,1],Spring.1819.match.info$Away.Team.Name)
#Plzen
Spring.1819.match.info$Away.Team.Name <- gsub(Spring.1819.match.info[4,1],teams[5,1],Spring.1819.match.info$Away.Team.Name)
Spring.1819.match.info$Home.Team.Name <- gsub(Spring.1819.match.info[4,1],teams[5,1],Spring.1819.match.info$Home.Team.Name)
#Jicin
Spring.1819.match.info$Home.Team.Name <- gsub(Spring.1819.match.info[17,1],teams[11,1],Spring.1819.match.info$Home.Team.Name)
Spring.1819.match.info$Away.Team.Name <- gsub(Spring.1819.match.info[1,2],teams[11,1],Spring.1819.match.info$Away.Team.Name)
#Lovosice
Spring.1819.match.info$Home.Team.Name <- gsub(Spring.1819.match.info[12,1],teams[12,1],Spring.1819.match.info$Home.Team.Name)
Spring.1819.match.info$Away.Team.Name <- gsub(Spring.1819.match.info[2,2],teams[12,1],Spring.1819.match.info$Away.Team.Name)

## 6. Assign a unique ID to each match
Spring.1819.match.info <- data.frame("Match.ID" = 1:nrow(Spring.1819.match.info), Spring.1819.match.info)

## 7. Assign home team initials and away team initials to each team in the Spring.1819.match.info dataset
Home.Team.Init <- merge(Spring.1819.match.info, teams, by.x = "Home.Team.Name",
                                   by.y = "Team_Name", all.x = TRUE)
Away.Team.Init <- merge(Home.Team.Init, teams, by.x = "Away.Team.Name",
                        by.y = "Team_Name", all.x = TRUE)

## 8. Reorder rows into original order
Spring.1819.match.info.extended <- Away.Team.Init[order(Away.Team.Init$Match.ID), ]
row.names(Spring.1819.match.info.extended) <- 1:nrow(Spring.1819.match.info.extended)

## 9. Edit the names and order of dataframe's columns
# Prejmenovani nove vytvorenych sloupcu
names(Spring.1819.match.info.extended)[7] <- "Home.Team.Init"
names(Spring.1819.match.info.extended)[8] <- "Away.Team.Init"
# Srovnani sloupcu do pozadovaneho poradi
Spring.1819.match.info.extended <- Spring.1819.match.info.extended[,c(3,7,8,2,1,4,5,6)]

## 10. Set Team initials columns as of type factor
Spring.1819.match.info.extended$Home.Team.Init <- factor(Spring.1819.match.info.extended$Home.Team.Init)
Spring.1819.match.info.extended$Away.Team.Init <- factor(Spring.1819.match.info.extended$Away.Team.Init)

## 11. Remove unneccessary structures from the workspace
rm(i, teams, Spring.1819.match.info, links.site,
   Home.Team.Init, Away.Team.Init,
   Results.parsed, links.lst, Team_Init, Team_Name,
   links.names)
