## LOAD SEASON 2017/18 BACK TILL 2009/10 BASIC MATCH DATA FROM CHF.CZ WEBSITE

#### Load Packages ####
library(xml2)
library(rvest)
library(lubridate)
library(stringi)
library(stringr)
library(dplyr)
library(readr)


##################### 2017/18...192ROWS ##############################

## 1.1. create dataframe of 2018/19 teams
# Team_Name = c("HCB Karviná", "SKKP Handball Brno", "KH Koprivnice", "HC ROBE Zubrí",
#               "Talent ROBSTAV-M.A.T.Plzen", "Pepino SKP Frýdek-Místek",
#               "HBC Strakonice 1921", "TJ Sokol Nové Veselí", "TJ Cement Hranice",
#               "HC Dukla Praha", "HBC Ronal Jicín", "HK FCC Mesto Lovosice")
# Team_Init = c("KAR", "BRN", "KOP", "ZUB", "PLZ", "FRM", "STR", "NOV", "HRA", "DUK",
#               "JIC", "LOV")
# teams <- data.frame("Team_Name" = Team_Name, "Team_Init" = Team_Init, stringsAsFactors = FALSE)
# rm(Team_Name, Team_Init)

# 1.2. Get the expected dataset structure ready
Season.1718.match.info <- data.frame("Home.Team.Name" = character(),
                                     "Result" = character(),
                                     "Away.Team.Name" = character(),
                                     stringsAsFactors = FALSE)

#### Final matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2017.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=3023&roundindex=22"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol - 3nasobni children jsou entity pro jednotlive zapasy
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1718.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1718.match.info[i,1] <- strsplit(links.names[2*(i-1)+1]," - ")[[1]][1]
  Season.1718.match.info[i,2] <- links.names[2*(i-1)+2]
  Season.1718.match.info[i,3] <- strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 3rd place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2017.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=3024&roundindex=22"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1718.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1718.match.info <- rbind(Season.1718.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 5th place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2017.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=3025&roundindex=22"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1718.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1718.match.info <- rbind(Season.1718.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)


#### 7th place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2017.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=3026&roundindex=22"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1718.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1718.match.info <- rbind(Season.1718.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### Semifinal matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2017.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=3017&roundindex=22"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1718.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1718.match.info <- rbind(Season.1718.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 5-8th place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2017.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=3018&roundindex=22"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1718.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1718.match.info <- rbind(Season.1718.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### Quarterfinals matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2017.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=3015&roundindex=22"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1718.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1718.match.info <- rbind(Season.1718.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 9-12th place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2017.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=3016&roundindex=22"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol

links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()

# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1718.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1718.match.info <- rbind(Season.1718.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}
# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)


#### Classic season matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2017.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2870&roundindex=22"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol

links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()

# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1718.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1718.match.info <- rbind(Season.1718.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}
# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)



#### Save as csv ####
write.csv(Season.1718.match.info, file = "csv_basic_match_info/Season1718_basic_info.csv",row.names=FALSE, na="")











##################### 2016/17...186ROWS ##############################

# 1.2. Get the expected dataset structure ready
Season.1617.match.info <- data.frame("Home.Team.Name" = character(),
                                     "Result" = character(),
                                     "Away.Team.Name" = character(),
                                     stringsAsFactors = FALSE)
#### Final matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2016.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2831&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol - 3nasobni children jsou entity pro jednotlive zapasy
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]')  %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1617.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1617.match.info[i,1] <- strsplit(links.names[2*(i-1)+1]," - ")[[1]][1]
  Season.1617.match.info[i,2] <- links.names[2*(i-1)+2]
  Season.1617.match.info[i,3] <- strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 3rd place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2016.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2832&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1617.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1617.match.info <- rbind(Season.1617.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 5th place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2016.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2833&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1617.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1617.match.info <- rbind(Season.1617.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)


#### 7th place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2016.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2834&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1617.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1617.match.info <- rbind(Season.1617.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### Semifinal matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2016.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2820&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1617.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1617.match.info <- rbind(Season.1617.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 5-8th place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2016.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2821&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1617.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1617.match.info <- rbind(Season.1617.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### Quarterfinals matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2016.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2818&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1617.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1617.match.info <- rbind(Season.1617.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 9-12th place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2016.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2819&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol

links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()

# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1617.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1617.match.info <- rbind(Season.1617.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}
# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)


#### Classic season matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2016.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2677&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol

links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()

# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1617.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1617.match.info <- rbind(Season.1617.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}
# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)





#### Save as csv ####
write.csv(Season.1617.match.info, file = "csv_basic_match_info/Season1617_basic_info.csv",row.names=FALSE, na="")









##################### 2015/16 ##############################

# 1.2. Get the expected dataset structure ready
Season.1516.match.info <- data.frame("Home.Team.Name" = character(),
                                     "Result" = character(),
                                     "Away.Team.Name" = character(),
                                     stringsAsFactors = FALSE)
#### Final matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2627&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol - 3nasobni children jsou entity pro jednotlive zapasy
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1516.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1516.match.info[i,1] <- strsplit(links.names[2*(i-1)+1]," - ")[[1]][1]
  Season.1516.match.info[i,2] <- links.names[2*(i-1)+2]
  Season.1516.match.info[i,3] <- strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 3rd place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2628&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1516.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1516.match.info <- rbind(Season.1516.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 5th place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2619&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1516.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1516.match.info <- rbind(Season.1516.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)


#### 7th place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2620&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1516.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1516.match.info <- rbind(Season.1516.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### Semifinal matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2617&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1516.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1516.match.info <- rbind(Season.1516.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 5-8th place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2618&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1516.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1516.match.info <- rbind(Season.1516.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### Quarterfinals matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2606&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1516.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1516.match.info <- rbind(Season.1516.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 9th place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2616&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1516.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1516.match.info <- rbind(Season.1516.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 11th place matches  ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2615&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1516.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1516.match.info <- rbind(Season.1516.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 9-12th place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2607&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol

links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()

# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1516.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1516.match.info <- rbind(Season.1516.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}
# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)


#### Classic season matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2015.archiv.chf.cz/view_tables.aspx?cmpid=1500&cmppartid=2453&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol

links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()

# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1516.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1516.match.info <- rbind(Season.1516.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}
# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)






#### Save as csv ####
write.csv(Season.1516.match.info, file = "csv_basic_match_info/Season1516_basic_info.csv",row.names=FALSE, na="")








##################### 2014/15...COMPLETE, READY TO SAVE ##############################

# 1.2. Get the expected dataset structure ready
Season.1415.match.info <- data.frame("Home.Team.Name" = character(),
                                     "Result" = character(),
                                     "Away.Team.Name" = character(),
                                     stringsAsFactors = FALSE)
#### Final matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2431&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol - 3nasobni children jsou entity pro jednotlive zapasy
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1415.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1415.match.info[i,1] <- strsplit(links.names[2*(i-1)+1]," - ")[[1]][1]
  Season.1415.match.info[i,2] <- links.names[2*(i-1)+2]
  Season.1415.match.info[i,3] <- strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 3rd place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2432&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1415.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1415.match.info <- rbind(Season.1415.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 5th place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2433&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1415.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1415.match.info <- rbind(Season.1415.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)


#### 7th place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2434&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1415.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1415.match.info <- rbind(Season.1415.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### Semifinal matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2426&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1415.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1415.match.info <- rbind(Season.1415.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 5-8th place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2429&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1415.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1415.match.info <- rbind(Season.1415.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### Quarterfinals matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2421&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1415.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1415.match.info <- rbind(Season.1415.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 9th place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2430&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1415.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1415.match.info <- rbind(Season.1415.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 11th place matches  ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2427&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1415.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1415.match.info <- rbind(Season.1415.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 9-12th place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2422&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol

links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()

# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1415.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1415.match.info <- rbind(Season.1415.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}
# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)


#### Classic season matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2014.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2284&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol

links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()

# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1415.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1415.match.info <- rbind(Season.1415.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}
# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)








#### Save as csv ####
write.csv(Season.1415.match.info, file = "csv_basic_match_info/Season1415_basic_info.csv",row.names=FALSE, na="")






##################### 2013/14...COMPLETE, READY TO SAVE ##############################

# 1.2. Get the expected dataset structure ready
Season.1314.match.info <- data.frame("Home.Team.Name" = character(),
                                     "Result" = character(),
                                     "Away.Team.Name" = character(),
                                     stringsAsFactors = FALSE)
#### Nadstavba 1-6 ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2013.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2230&roundindex=10"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol - 3nasobni children jsou entity pro jednotlive zapasy
links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1314.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1314.match.info[i,1] <- strsplit(links.names[2*(i-1)+1]," - ")[[1]][1]
  Season.1314.match.info[i,2] <- links.names[2*(i-1)+2]
  Season.1314.match.info[i,3] <- strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### Nadstavba 7-12 ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2013.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2231&roundindex=10"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1314.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1314.match.info <- rbind(Season.1314.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### Classic season matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2013.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=2039&roundindex=10"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol

links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()

# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1314.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1314.match.info <- rbind(Season.1314.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}
# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)











#### Save as csv ####
write.csv(Season.1314.match.info, file = "csv_basic_match_info/Season1314_basic_info.csv",row.names=FALSE, na="")





##################### 2012/13...COMPLETE, READY TO SAVE ##############################

# 1.2. Get the expected dataset structure ready
Season.1213.match.info <- data.frame("Home.Team.Name" = character(),
                                     "Result" = character(),
                                     "Away.Team.Name" = character(),
                                     stringsAsFactors = FALSE)
#### Nadstavba 1-6 ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2012.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=1953&roundindex=10"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol - 3nasobni children jsou entity pro jednotlive zapasy
links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1213.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1213.match.info[i,1] <- strsplit(links.names[2*(i-1)+1]," - ")[[1]][1]
  Season.1213.match.info[i,2] <- links.names[2*(i-1)+2]
  Season.1213.match.info[i,3] <- strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### Nadstavba 7-11 ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2012.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=1954&roundindex=10"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1213.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1213.match.info <- rbind(Season.1213.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### Classic season matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2012.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=1455&roundindex=10"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol

links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()

# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1213.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1213.match.info <- rbind(Season.1213.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}
# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)













#### Save as csv ####
write.csv(Season.1213.match.info, file = "csv_basic_match_info/Season1213_basic_info.csv",row.names=FALSE, na="")






##################### 2011/12...COMPLETE, READY TO SAVE ##############################

# 1.2. Get the expected dataset structure ready
Season.1112.match.info <- data.frame("Home.Team.Name" = character(),
                                     "Result" = character(),
                                     "Away.Team.Name" = character(),
                                     stringsAsFactors = FALSE)
#### Nadstavba 1-6 ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2011.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=1658&roundindex=10"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol - 3nasobni children jsou entity pro jednotlive zapasy
links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1112.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1112.match.info[i,1] <- strsplit(links.names[2*(i-1)+1]," - ")[[1]][1]
  Season.1112.match.info[i,2] <- links.names[2*(i-1)+2]
  Season.1112.match.info[i,3] <- strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### Nadstavba 7-12 ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2011.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=1659&roundindex=10"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1112.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1112.match.info <- rbind(Season.1112.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### Classic season matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2011.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=1455&roundindex=10"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol

links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()

# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1112.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1112.match.info <- rbind(Season.1112.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}
# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)
















#### Save as csv ####
write.csv(Season.1112.match.info, file = "csv_basic_match_info/Season1112_basic_info.csv",row.names=FALSE, na="")






##################### 2010/11...COMPLETE, READY TO SAVE ##############################

# 1.2. Get the expected dataset structure ready
Season.1011.match.info <- data.frame("Home.Team.Name" = character(),
                                     "Result" = character(),
                                     "Away.Team.Name" = character(),
                                     stringsAsFactors = FALSE)
#### Nadstavba 1-6 ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2010.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=1432&roundindex=10"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol - 3nasobni children jsou entity pro jednotlive zapasy
links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1011.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1011.match.info[i,1] <- strsplit(links.names[2*(i-1)+1]," - ")[[1]][1]
  Season.1011.match.info[i,2] <- links.names[2*(i-1)+2]
  Season.1011.match.info[i,3] <- strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### Nadstavba 7-12 ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2010.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=1431&roundindex=10"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1011.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1011.match.info <- rbind(Season.1011.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### Classic season matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2010.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=95&roundindex=10"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol

links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()

# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.1011.match.info
for (i in 1:(length(links.names)/2)) {
  Season.1011.match.info <- rbind(Season.1011.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}
# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)



















#### Save as csv ####
write.csv(Season.1011.match.info, file = "csv_basic_match_info/Season1011_basic_info.csv",row.names=FALSE, na="")








##################### 2009/10...COMPLETE, READY TO SAVE ##############################


# 1.2. Get the expected dataset structure ready
Season.0910.match.info <- data.frame("Home.Team.Name" = character(),
                                     "Result" = character(),
                                     "Away.Team.Name" = character(),
                                     stringsAsFactors = FALSE)

#### Final matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2009.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=257&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol - 3nasobni children jsou entity pro jednotlive zapasy
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.0910.match.info
for (i in 1:(length(links.names)/2)) {
  Season.0910.match.info[i,1] <- strsplit(links.names[2*(i-1)+1]," - ")[[1]][1]
  Season.0910.match.info[i,2] <- links.names[2*(i-1)+2]
  Season.0910.match.info[i,3] <- strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 3rd place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2009.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=258&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.0910.match.info
for (i in 1:(length(links.names)/2)) {
  Season.0910.match.info <- rbind(Season.0910.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### Semifinal matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2009.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=246&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.0910.match.info
for (i in 1:(length(links.names)/2)) {
  Season.0910.match.info <- rbind(Season.0910.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 5-8th place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2009.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=248&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.0910.match.info
for (i in 1:(length(links.names)/2)) {
  Season.0910.match.info <- rbind(Season.0910.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### Quarterfinals matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2009.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=238&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol
links.names <- links.site %>%
  html_nodes(xpath='//div[@id="ctl00_ctl00_ContentHolder_ContentHolder_round_list_ctl00_ctl00_DataPanel_0"]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()
# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.0910.match.info
for (i in 1:(length(links.names)/2)) {
  Season.0910.match.info <- rbind(Season.0910.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}

# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)

#### 9-12th place matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2009.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=247&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol

links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()

# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.0910.match.info
for (i in 1:(length(links.names)/2)) {
  Season.0910.match.info <- rbind(Season.0910.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}
# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)


#### Classic season matches ####

## 2. scrape CHF basic match data from the website
links.lst <- "http://muzi.2009.archiv.chf.cz/view_tables.aspx?cmpid=49&cmppartid=95&roundindex=1"

# 2.2. Do the web-scraping
links.site <- read_html(links.lst)
# 2.2.1. Nacti webovou stranku a najdi tabulku s vysledky kol

links.names <- links.site %>%
  html_nodes(xpath='//div[contains(@id,"ctl00_ctl00_ContentHolder_ContentHolder_round_list")]') %>%
  html_children() %>% html_children() %>% html_children() %>% html_children() %>%
  html_text()

# 2.2.2. Odstran znaky "\n                " ze ziskanych dat
for (i in 1:length(links.names)) {
  links.names[i] <- gsub("\n                    ","",links.names[i])
  links.names[i] <- gsub("\n                ","",links.names[i])
}
links.names <- links.names[which(links.names != "    video    online")]

# 2.2.3. Nahrej nactene udaje ze zapasu do dataframu Season.0910.match.info
for (i in 1:(length(links.names)/2)) {
  Season.0910.match.info <- rbind(Season.0910.match.info,
                                  c(strsplit(links.names[2*(i-1)+1]," - ")[[1]][1],
                                    links.names[2*(i-1)+2],
                                    strsplit(links.names[2*(i-1)+1]," - ")[[1]][2]))
}
# 3. Refresh the workspace - clean unneccessary structures
rm(links.names, links.lst, links.site, i)





#### Save as csv ####
write.csv(Season.0910.match.info, file = "csv_basic_match_info/Season0910_basic_info.csv",row.names=FALSE, na="")









#######################################################################
############ Basic match info webscraping complete ########################
#######################################################################

##### Load data from csv files now, so that webscraping is not performed again #### 

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




##### EDIT DATA 1718 ######
## 3. Edit the score column

# 3.1. Transform the score-column from string format "XX:YY" into two columns for host and away team
Season.1718.match.info <- cbind(Season.1718.match.info, "Home.Team.Goals" = rep(NA, length(Season.1718.match.info$Result)),
                    "Away.Team.Goals" = rep(NA, length(Season.1718.match.info$Result)))
Results.parsed <- strsplit(Season.1718.match.info$Result," : ")
for (i in 1:length(Season.1718.match.info$Result)) {
  Season.1718.match.info$Home.Team.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1718.match.info$Away.Team.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1718.match.info <- Season.1718.match.info %>% select(-Result) #Remove unneccessary column "Result"

## Remove NA-valued rows
Season.1718.match.info <- Season.1718.match.info[complete.cases(Season.1718.match.info), ]

## Save the edited dataframe as csv
write.csv(Season.1718.match.info, file = "csv_basic_match_info/Season1718_basic_info.csv",row.names=FALSE, na="")


##### EDIT DATA 1617 ######
## 3. Edit the score column

# 3.1. Transform the score-column from string format "XX:YY" into two columns for host and away team
Season.1617.match.info <- cbind(Season.1617.match.info, "Home.Team.Goals" = rep(NA, length(Season.1617.match.info$Result)),
                                "Away.Team.Goals" = rep(NA, length(Season.1617.match.info$Result)))
Results.parsed <- strsplit(Season.1617.match.info$Result," : ")
for (i in 1:length(Season.1617.match.info$Result)) {
  Season.1617.match.info$Home.Team.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1617.match.info$Away.Team.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1617.match.info <- Season.1617.match.info %>% select(-Result) #Remove unneccessary column "Result"

## Remove NA-valued rows
Season.1617.match.info <- Season.1617.match.info[complete.cases(Season.1617.match.info), ]

## Save the edited dataframe as csv
write.csv(Season.1617.match.info, file = "csv_basic_match_info/Season1617_basic_info.csv",row.names=FALSE, na="")


##### EDIT DATA 1516 ######
## 3. Edit the score column

# 3.1. Transform the score-column from string format "XX:YY" into two columns for host and away team
Season.1516.match.info <- cbind(Season.1516.match.info, "Home.Team.Goals" = rep(NA, length(Season.1516.match.info$Result)),
                                "Away.Team.Goals" = rep(NA, length(Season.1516.match.info$Result)))
Results.parsed <- strsplit(Season.1516.match.info$Result," : ")
for (i in 1:length(Season.1516.match.info$Result)) {
  Season.1516.match.info$Home.Team.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1516.match.info$Away.Team.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1516.match.info <- Season.1516.match.info %>% select(-Result) #Remove unneccessary column "Result"

## Remove NA-valued rows
Season.1516.match.info <- Season.1516.match.info[complete.cases(Season.1516.match.info), ]

## Save the edited dataframe as csv
write.csv(Season.1516.match.info, file = "csv_basic_match_info/Season1516_basic_info.csv",row.names=FALSE, na="")


##### EDIT DATA 1415 ######
## 3. Edit the score column

# 3.1. Transform the score-column from string format "XX:YY" into two columns for host and away team
Season.1415.match.info <- cbind(Season.1415.match.info, "Home.Team.Goals" = rep(NA, length(Season.1415.match.info$Result)),
                                "Away.Team.Goals" = rep(NA, length(Season.1415.match.info$Result)))
Results.parsed <- strsplit(Season.1415.match.info$Result," : ")
for (i in 1:length(Season.1415.match.info$Result)) {
  Season.1415.match.info$Home.Team.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1415.match.info$Away.Team.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1415.match.info <- Season.1415.match.info %>% select(-Result) #Remove unneccessary column "Result"

## Remove NA-valued rows
Season.1415.match.info <- Season.1415.match.info[complete.cases(Season.1415.match.info), ]

## Save the edited dataframe as csv
write.csv(Season.1415.match.info, file = "csv_basic_match_info/Season1415_basic_info.csv",row.names=FALSE, na="")


##### EDIT DATA 1314 ######
## 3. Edit the score column

# 3.1. Transform the score-column from string format "XX:YY" into two columns for host and away team
Season.1314.match.info <- cbind(Season.1314.match.info, "Home.Team.Goals" = rep(NA, length(Season.1314.match.info$Result)),
                                "Away.Team.Goals" = rep(NA, length(Season.1314.match.info$Result)))
Results.parsed <- strsplit(Season.1314.match.info$Result," : ")
for (i in 1:length(Season.1314.match.info$Result)) {
  Season.1314.match.info$Home.Team.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1314.match.info$Away.Team.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1314.match.info <- Season.1314.match.info %>% select(-Result) #Remove unneccessary column "Result"

## Remove NA-valued rows
Season.1314.match.info <- Season.1314.match.info[complete.cases(Season.1314.match.info), ]

## Save the edited dataframe as csv
write.csv(Season.1314.match.info, file = "csv_basic_match_info/Season1314_basic_info.csv",row.names=FALSE, na="")


##### EDIT DATA 1213 ######
## 3. Edit the score column

# 3.1. Transform the score-column from string format "XX:YY" into two columns for host and away team
Season.1213.match.info <- cbind(Season.1213.match.info, "Home.Team.Goals" = rep(NA, length(Season.1213.match.info$Result)),
                                "Away.Team.Goals" = rep(NA, length(Season.1213.match.info$Result)))
Results.parsed <- strsplit(Season.1213.match.info$Result," : ")
for (i in 1:length(Season.1213.match.info$Result)) {
  Season.1213.match.info$Home.Team.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1213.match.info$Away.Team.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1213.match.info <- Season.1213.match.info %>% select(-Result) #Remove unneccessary column "Result"

## Remove NA-valued rows
Season.1213.match.info <- Season.1213.match.info[complete.cases(Season.1213.match.info), ]

## Save the edited dataframe as csv
write.csv(Season.1213.match.info, file = "csv_basic_match_info/Season1213_basic_info.csv",row.names=FALSE, na="")


##### EDIT DATA 1112 ######
## 3. Edit the score column

# 3.1. Transform the score-column from string format "XX:YY" into two columns for host and away team
Season.1112.match.info <- cbind(Season.1112.match.info, "Home.Team.Goals" = rep(NA, length(Season.1112.match.info$Result)),
                                "Away.Team.Goals" = rep(NA, length(Season.1112.match.info$Result)))
Results.parsed <- strsplit(Season.1112.match.info$Result," : ")
for (i in 1:length(Season.1112.match.info$Result)) {
  Season.1112.match.info$Home.Team.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1112.match.info$Away.Team.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1112.match.info <- Season.1112.match.info %>% select(-Result) #Remove unneccessary column "Result"

## Remove NA-valued rows
Season.1112.match.info <- Season.1112.match.info[complete.cases(Season.1112.match.info), ]

## Save the edited dataframe as csv
write.csv(Season.1112.match.info, file = "csv_basic_match_info/Season1112_basic_info.csv",row.names=FALSE, na="")


##### EDIT DATA 1011 ######
## 3. Edit the score column

# 3.1. Transform the score-column from string format "XX:YY" into two columns for host and away team
Season.1011.match.info <- cbind(Season.1011.match.info, "Home.Team.Goals" = rep(NA, length(Season.1011.match.info$Result)),
                                "Away.Team.Goals" = rep(NA, length(Season.1011.match.info$Result)))
Results.parsed <- strsplit(Season.1011.match.info$Result," : ")
for (i in 1:length(Season.1011.match.info$Result)) {
  Season.1011.match.info$Home.Team.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1011.match.info$Away.Team.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1011.match.info <- Season.1011.match.info %>% select(-Result) #Remove unneccessary column "Result"

## Remove NA-valued rows
Season.1011.match.info <- Season.1011.match.info[complete.cases(Season.1011.match.info), ]

## Save the edited dataframe as csv
write.csv(Season.1011.match.info, file = "csv_basic_match_info/Season1011_basic_info.csv",row.names=FALSE, na="")


##### EDIT DATA 0910 ######
## 3. Edit the score column

# 3.1. Transform the score-column from string format "XX:YY" into two columns for host and away team
Season.0910.match.info <- cbind(Season.0910.match.info, "Home.Team.Goals" = rep(NA, length(Season.0910.match.info$Result)),
                                "Away.Team.Goals" = rep(NA, length(Season.0910.match.info$Result)))
Results.parsed <- strsplit(Season.0910.match.info$Result," : ")
for (i in 1:length(Season.0910.match.info$Result)) {
  Season.0910.match.info$Home.Team.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.0910.match.info$Away.Team.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.0910.match.info <- Season.0910.match.info %>% select(-Result) #Remove unneccessary column "Result"

## Remove NA-valued rows
Season.0910.match.info <- Season.0910.match.info[complete.cases(Season.0910.match.info), ]

## Save the edited dataframe as csv
write.csv(Season.0910.match.info, file = "csv_basic_match_info/Season0910_basic_info.csv",row.names=FALSE, na="")



#### CLEANING ####
rm(Results.parsed, i)
