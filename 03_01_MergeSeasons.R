## MERGE DATAFRAMES FROM ALL INSPECTED SEASONS ##

###### Load packages ####
library(xml2)
library(rvest)
library(lubridate)
library(stringi)
library(stringr)
library(dplyr)
library(readr)

#### LOAD 02_01 DATA FROM CSV FILE (NO DATA WAS ADDED IN 02_02) ####
Season.0910.detailed.extended <- read_csv("csv_detailed_extended_match_info/Season0910_detailed_extended_info.csv", 
                                             locale = locale(encoding = "windows-1252"))
Season.1011.detailed.extended <- read_csv("csv_detailed_extended_match_info/Season1011_detailed_extended_info.csv", 
                                             locale = locale(encoding = "windows-1252"))
Season.1112.detailed.extended <- read_csv("csv_detailed_extended_match_info/Season1112_detailed_extended_info.csv", 
                                             locale = locale(encoding = "windows-1252"))
Season.1213.detailed.extended <- read_csv("csv_detailed_extended_match_info/Season1213_detailed_extended_info.csv", 
                                             locale = locale(encoding = "windows-1252"))
Season.1314.detailed.extended <- read_csv("csv_detailed_extended_match_info/Season1314_detailed_extended_info.csv", 
                                             locale = locale(encoding = "windows-1252"))
Season.1415.detailed.extended <- read_csv("csv_detailed_extended_match_info/Season1415_detailed_extended_info.csv", 
                                             locale = locale(encoding = "windows-1252"))
Season.1516.detailed.extended <- read_csv("csv_detailed_extended_match_info/Season1516_detailed_extended_info.csv", 
                                             locale = locale(encoding = "windows-1252"))
Season.1617.detailed.extended <- read_csv("csv_detailed_extended_match_info/Season1617_detailed_extended_info.csv", 
                                             locale = locale(encoding = "windows-1252"))
Season.1718.detailed.extended <- read_csv("csv_detailed_extended_match_info/Season1718_detailed_extended_info.csv", 
                                             locale = locale(encoding = "windows-1252"))
Season.1819Fall.detailed.extended <- read_csv("csv_detailed_extended_match_info/Season1819Fall_detailed_extended_info.csv", 
                                                 locale = locale(encoding = "windows-1252"))

#### MERGE ARCHIVE DATASETS 09/10-17/18 ####
S0910_1718.detailed.extended <- rbind(Season.1718.detailed.extended, Season.1617.detailed.extended,
      Season.1516.detailed.extended, Season.1415.detailed.extended,
      Season.1314.detailed.extended, Season.1213.detailed.extended,
      Season.1112.detailed.extended, Season.1011.detailed.extended,
      Season.0910.detailed.extended)

# Set the Match.Date datatype as Date
S0910_1718.detailed.extended$Match.Date <- as.Date(S0910_1718.detailed.extended$Match.Date, format = "%d.%m.%Y")

#### MERGE ARCHIVE DATASETS 09/10-18/19Fall ####
S0910_1819Fall.detailed.extended <- rbind(Season.1819Fall.detailed.extended, S0910_1718.detailed.extended)



#### ADD GLOBAL MATCH ID VARIABLE ####
S0910_1819Fall.detailed.extended <- data.frame("Global.Match.ID" = c(1:nrow(S0910_1819Fall.detailed.extended)),
                                               S0910_1819Fall.detailed.extended)
#### SAVE MERGED DATASETS ####
write.csv(S0910_1819Fall.detailed.extended, file = "csv_merged/S0910_1819Fall.merged.basic.csv",row.names=FALSE, na="")

#DALSI SOUBORY:
#03_02 - Merge S0910_1819Fall AND DAY INFO AND TEAM STATS
   #DELETE 02_02 DATASET, LOAD THE DATASETS IN 03_03
    