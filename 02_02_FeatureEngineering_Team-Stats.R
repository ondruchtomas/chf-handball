#### FEATURE ENGINEERING - ONLY DATA LOADING

###### Load packages ####
library(xml2)
library(rvest)
library(lubridate)
library(stringi)
library(stringr)
library(dplyr)
library(readr)

#### LOAD EXTENDED 02_01 DATA FROM CSV FILE ####
Season.0910.detailed.extended.ts <- read_csv("csv_detailed_extended_match_info/Season0910_detailed_extended_info.csv", 
                                          locale = locale(encoding = "windows-1252"))
Season.1011.detailed.extended.ts <- read_csv("csv_detailed_extended_match_info/Season1011_detailed_extended_info.csv", 
                                          locale = locale(encoding = "windows-1252"))
Season.1112.detailed.extended.ts <- read_csv("csv_detailed_extended_match_info/Season1112_detailed_extended_info.csv", 
                                          locale = locale(encoding = "windows-1252"))
Season.1213.detailed.extended.ts <- read_csv("csv_detailed_extended_match_info/Season1213_detailed_extended_info.csv", 
                                          locale = locale(encoding = "windows-1252"))
Season.1314.detailed.extended.ts <- read_csv("csv_detailed_extended_match_info/Season1314_detailed_extended_info.csv", 
                                          locale = locale(encoding = "windows-1252"))
Season.1415.detailed.extended.ts <- read_csv("csv_detailed_extended_match_info/Season1415_detailed_extended_info.csv", 
                                          locale = locale(encoding = "windows-1252"))
Season.1516.detailed.extended.ts <- read_csv("csv_detailed_extended_match_info/Season1516_detailed_extended_info.csv", 
                                          locale = locale(encoding = "windows-1252"))
Season.1617.detailed.extended.ts <- read_csv("csv_detailed_extended_match_info/Season1617_detailed_extended_info.csv", 
                                          locale = locale(encoding = "windows-1252"))
Season.1718.detailed.extended.ts <- read_csv("csv_detailed_extended_match_info/Season1718_detailed_extended_info.csv", 
                                          locale = locale(encoding = "windows-1252"))
Season.1819Fall.detailed.extended.ts <- read_csv("csv_detailed_extended_match_info/Season1819Fall_detailed_extended_info.csv", 
                                              locale = locale(encoding = "windows-1252"))


#### LOAD TEAM-STATS DATA FROM CSV FILE ####
Season.0910.ts <- read_csv("csv_team_stats/Season0910_Team_Stats.csv", 
                                             locale = locale(encoding = "windows-1252"))
Season.1011.ts <- read_csv("csv_team_stats/Season1011_Team_Stats.csv", 
                                             locale = locale(encoding = "windows-1252"))
Season.1112.ts <- read_csv("csv_team_stats/Season1112_Team_Stats.csv", 
                                             locale = locale(encoding = "windows-1252"))
Season.1213.ts <- read_csv("csv_team_stats/Season1213_Team_Stats.csv", 
                                             locale = locale(encoding = "windows-1252"))
Season.1314.ts <- read_csv("csv_team_stats/Season1314_Team_Stats.csv", 
                                             locale = locale(encoding = "windows-1252"))
Season.1415.ts <- read_csv("csv_team_stats/Season1415_Team_Stats.csv", 
                                             locale = locale(encoding = "windows-1252"))
Season.1516.ts <- read_csv("csv_team_stats/Season1516_Team_Stats.csv", 
                                             locale = locale(encoding = "windows-1252"))
Season.1617.ts <- read_csv("csv_team_stats/Season1617_Team_Stats.csv", 
                                             locale = locale(encoding = "windows-1252"))
Season.1718.ts <- read_csv("csv_team_stats/Season1718_Team_Stats.csv", 
                                             locale = locale(encoding = "windows-1252"))
