## MERGE 0910/1819Fall DATASET AND DAY INFO AND TEAM STATS INFO ##

###### Load packages ####
library(xml2)
library(rvest)
library(lubridate)
library(stringi)
library(stringr)
library(dplyr)
library(readr)

#### LOAD 03_01 DATA FROM CSV FILE (NO DATA WAS ADDED IN 02_02) ####
S0910_1819Fall.days.TS <- read_csv("csv_merged/S0910_1819Fall.merged.basic.csv", 
                                          locale = locale(encoding = "windows-1252"))
#### MERGE SEASONS WITH DAY-INFO ####
S0910_1819Fall.days.TS <- merge(days, S0910_1819Fall.days.TS,
                                by.x = "Date", by.y = "Match.Date",
                                all.y = TRUE)
## Reorder rows into original order
S0910_1819Fall.days.TS <- S0910_1819Fall.days.TS[order(S0910_1819Fall.days.TS$Global.Match.ID), ]
row.names(S0910_1819Fall.days.TS) <- 1:nrow(S0910_1819Fall.days.TS)

#### CREATE HOME.TEAM.INIT AND AWAY.TEAM.INIT VARIABLES ####
## Note: All Team-names contained in Home.Team.Name are also in Away.Team.Name
team.levels <- levels(factor(S0910_1819Fall.days.TS$Away.Team.Name))
##Load Home.Team.Inits
S0910_1819Fall.days.TS <- S0910_1819Fall.days.TS %>% mutate("Home.Team.Init" = ifelse(grepl("Lovo", Home.Team.Name), "LOV",
                                                                                      ifelse(grepl("Kar",Home.Team.Name), "KAR",
                                                                                             ifelse(grepl("Brn",Home.Team.Name), "BRN",
                                                                                                    ifelse(grepl("Duk",Home.Team.Name),"DUK",
                                                                                                           ifelse(grepl("Hran",Home.Team.Name),"HRA",
                                                                                                                  ifelse(grepl("lov",Home.Team.Name), "DVK",
                                                                                                                         ifelse(grepl("dek",Home.Team.Name), "FRM",
                                                                                                                                ifelse(grepl("Zub",Home.Team.Name), "ZUB",
                                                                                                                                       ifelse(grepl("Ji",Home.Team.Name), "JIC",
                                                                                                                                              ifelse(grepl("Stra",Home.Team.Name), "STR",
                                                                                                                                                     ifelse(grepl("Zl",Home.Team.Name), "ZLN",
                                                                                                                                                            ifelse(grepl("Kop",Home.Team.Name), "KOP",
                                                                                                                                                                   ifelse(grepl("rov",Home.Team.Name), "PRE",
                                                                                                                                                                          ifelse(grepl("Plz",Home.Team.Name), "PLZ",
                                                                                                                                                                                 ifelse(grepl("Lito",Home.Team.Name), "LIT",
                                                                                                                                                                                        ifelse(grepl("ebo",Home.Team.Name), "TRE",
                                                                                                                                                                                               ifelse(grepl("esel",Home.Team.Name), "NOV",NA))))))))))))))))))
##Load Away.Team.Inits
S0910_1819Fall.days.TS <- S0910_1819Fall.days.TS %>% mutate("Away.Team.Init" = ifelse(grepl("Lovo", Away.Team.Name), "LOV",
                                                                                      ifelse(grepl("Kar",Away.Team.Name), "KAR",
                                                                                             ifelse(grepl("Brn",Away.Team.Name), "BRN",
                                                                                                    ifelse(grepl("Duk",Away.Team.Name),"DUK",
                                                                                                           ifelse(grepl("Hran",Away.Team.Name),"HRA",
                                                                                                                  ifelse(grepl("lov",Away.Team.Name), "DVK",
                                                                                                                         ifelse(grepl("dek",Away.Team.Name), "FRM",
                                                                                                                                ifelse(grepl("Zub",Away.Team.Name), "ZUB",
                                                                                                                                       ifelse(grepl("Ji",Away.Team.Name), "JIC",
                                                                                                                                              ifelse(grepl("Stra",Away.Team.Name), "STR",
                                                                                                                                                     ifelse(grepl("Zl",Away.Team.Name), "ZLN",
                                                                                                                                                            ifelse(grepl("Kop",Away.Team.Name), "KOP",
                                                                                                                                                                   ifelse(grepl("rov",Away.Team.Name), "PRE",
                                                                                                                                                                          ifelse(grepl("Plz",Away.Team.Name), "PLZ",
                                                                                                                                                                                 ifelse(grepl("Lito",Away.Team.Name), "LIT",
                                                                                                                                                                                        ifelse(grepl("ebo",Away.Team.Name), "TRE",
                                                                                                                                                                                               ifelse(grepl("esel",Away.Team.Name), "NOV",NA))))))))))))))))))
#### MERGE BIG DATASET WITH TEAM-STATS FOR EACH SEASON ####
##Create auxiliary variables concatenating Home.Team.Init_Season and Away.Team.Init_Season
S0910_1819Fall.days.TS <- S0910_1819Fall.days.TS %>% mutate("HTI_Season" = paste(Home.Team.Init,Season,sep = "_"),
                                                            "ATI_Season" = paste(Away.Team.Init,Season,sep = "_"))
##Create such auxiliary variables in all Season.XXXX.ts datasets, too
Season.0910.ts <- data.frame(Season.0910.ts, "7m.Shots.Acc" = rep(NA, nrow(Season.0910.ts)))
names(Season.0910.ts)[6] <- names(Season.1011.ts)[6]
Season.0910.ts <- Season.0910.ts %>% mutate("Team_Season" = paste(Team,"2009/2010",sep = "_"))
Season.1011.ts <- Season.1011.ts %>% mutate("Team_Season" = paste(Team,"2010/2011",sep = "_"))
Season.1112.ts <- Season.1112.ts %>% mutate("Team_Season" = paste(Team,"2011/2012",sep = "_"))
Season.1213.ts <- Season.1213.ts %>% mutate("Team_Season" = paste(Team,"2012/2013",sep = "_"))
Season.1314.ts <- Season.1314.ts %>% mutate("Team_Season" = paste(Team,"2013/2014",sep = "_"))
Season.1415.ts <- Season.1415.ts %>% mutate("Team_Season" = paste(Team,"2014/2015",sep = "_"))
Season.1516.ts <- Season.1516.ts %>% mutate("Team_Season" = paste(Team,"2015/2016",sep = "_"))
Season.1617.ts <- Season.1617.ts %>% mutate("Team_Season" = paste(Team,"2016/2017",sep = "_"))
Season.1718.ts <- Season.1718.ts %>% mutate("Team_Season" = paste(Team,"2017/2018",sep = "_"))
#Merge all team-stats datasets into one
S0910_1718.ts <- rbind(Season.0910.ts,Season.1011.ts,Season.1112.ts,Season.1213.ts,
                       Season.1314.ts,Season.1415.ts,Season.1516.ts,Season.1617.ts,
                       Season.1718.ts)
##Create team-stats variables in big dataset
S0910_1819Fall.days.TS <- merge(S0910_1819Fall.days.TS, S0910_1718.ts,
                                by.x = "HTI_Season", by.y = "Team_Season",
                                all.x = TRUE)
names(S0910_1819Fall.days.TS)[29:ncol(S0910_1819Fall.days.TS)] <- c("HT.Break.Goals",
                                                                    "HT.Goalkeepers.Perc",
                                                                    "HT.Goalkeepers.Saves",
                                                                    "HT.Shots.Acc.Perc",
                                                                    "HT.7m.Shots.Acc")
S0910_1819Fall.days.TS <- merge(S0910_1819Fall.days.TS, S0910_1718.ts,
                                by.x = "ATI_Season", by.y = "Team_Season",
                                all.x = TRUE)
names(S0910_1819Fall.days.TS)[35:ncol(S0910_1819Fall.days.TS)] <- c("AT.Break.Goals",
                                                                    "AT.Goalkeepers.Perc",
                                                                    "AT.Goalkeepers.Saves",
                                                                    "AT.Shots.Acc.Perc",
                                                                    "AT.7m.Shots.Acc")
#### EDITING THE DATASET STRUCTURE ####
##Delete unneccessary variables
S0910_1819Fall.days.TS <- S0910_1819Fall.days.TS[,-c(1,2,28,34)]
## Reorder rows into original order
S0910_1819Fall.days.TS <- S0910_1819Fall.days.TS[order(S0910_1819Fall.days.TS$Global.Match.ID), ]
row.names(S0910_1819Fall.days.TS) <- 1:nrow(S0910_1819Fall.days.TS)
## Reorder columns into a more reasonable ordering
S0910_1819Fall.days.TS <- S0910_1819Fall.days.TS[,c(5,6,24,25,7,8,9,10,1,18,17,11,12,13,14,15,16,20,21,22,23,26,27,28,29,30,31,32,33,34,35,2,3,4,19)]

#### SAVE THE DATASET STRUCTURE AS CSV FILE ####
write.csv(S0910_1819Fall.days.TS, file = "csv_merged/S0910_1819Fall.days.TS.csv",row.names=FALSE, na="")
#### SAVE THE MERGED TEAM-STATS DATASET ####
write.csv(S0910_1718.ts, file = "csv_merged/S0910_1718.ts.csv",row.names=FALSE, na="")
