#### FEATURE ENGINEERING

###### Load packages ####
library(xml2)
library(rvest)
library(lubridate)
library(stringi)
library(stringr)
library(dplyr)
library(readr)



#### LOAD SEASON DATA FROM CSV FILE ####
Season.0910.detailed.extended <- read_csv("csv_detailed_match_info/Season0910_detailed_info.csv", 
                                   locale = locale(encoding = "windows-1252"))
Season.1011.detailed.extended <- read_csv("csv_detailed_match_info/Season1011_detailed_info.csv", 
                                          locale = locale(encoding = "windows-1252"))
Season.1112.detailed.extended <- read_csv("csv_detailed_match_info/Season1112_detailed_info.csv", 
                                          locale = locale(encoding = "windows-1252"))
Season.1213.detailed.extended <- read_csv("csv_detailed_match_info/Season1213_detailed_info.csv", 
                                          locale = locale(encoding = "windows-1252"))
Season.1314.detailed.extended <- read_csv("csv_detailed_match_info/Season1314_detailed_info.csv", 
                                          locale = locale(encoding = "windows-1252"))
Season.1415.detailed.extended <- read_csv("csv_detailed_match_info/Season1415_detailed_info.csv", 
                                          locale = locale(encoding = "windows-1252"))
Season.1516.detailed.extended <- read_csv("csv_detailed_match_info/Season1516_detailed_info.csv", 
                                          locale = locale(encoding = "windows-1252"))
Season.1617.detailed.extended <- read_csv("csv_detailed_match_info/Season1617_detailed_info.csv", 
                                          locale = locale(encoding = "windows-1252"))
Season.1718.detailed.extended <- read_csv("csv_detailed_match_info/Season1718_detailed_info.csv", 
                                          locale = locale(encoding = "windows-1252"))
Season.1819Fall.detailed.extended <- read_csv("csv_detailed_match_info/Season1819Fall_detailed_info.csv", 
                                          locale = locale(encoding = "windows-1252"))


#### FALL 1819 PART OF SEASON ####

## 1. Add info about Season to the dataset
Season.1819Fall.detailed.extended <- data.frame(Season.1819Fall.detailed.extended,
                  "Season" = "2018/2019")

## 2. Add info about part of season to the dataset
Season.1819Fall.detailed.extended <- data.frame(Season.1819Fall.detailed.extended,
                  "Season.Phase" = "RegularPhaseMatch")

## 3. Add info if the match was broadcasted by Ceska televize
    # 0 = not broadcasted. 1 = broadcasted
    # source: https://www.ceskatelevize.cz/porady/10132144283-hazena/dily/
Season.1819Fall.detailed.extended <- data.frame(Season.1819Fall.detailed.extended,
                  "CT.Broadcast" = 0)
Season.1819Fall.detailed.extended$CT.Broadcast[66] <- 1 #LOV-DUK
Season.1819Fall.detailed.extended$CT.Broadcast[55] <- 1 #KAR-HRA
Season.1819Fall.detailed.extended$CT.Broadcast[49] <- 1 #JIC-DUK
Season.1819Fall.detailed.extended$CT.Broadcast[31] <- 1 #HRA-LIT
Season.1819Fall.detailed.extended$CT.Broadcast[36] <- 1 #KAR-PLZ
Season.1819Fall.detailed.extended$CT.Broadcast[13] <- 1 #ZUB-LOV
Season.1819Fall.detailed.extended$CT.Broadcast[2] <- 1 #BRN-JIC


#### 1718 SEASON ####

# 0. Add local.match.ID info as the first column
Season.1718.detailed.extended <- data.frame("Local.Match.ID" = c(1:nrow(Season.1718.detailed.extended)),
                                            Season.1718.detailed.extended)

## 1. Add info about Season to the dataset
Season.1718.detailed.extended <- data.frame(Season.1718.detailed.extended,
                                                "Season" = "2017/2018")

## 3. Add info if the match was broadcasted by Ceska televize
# 0 = not broadcasted. 1 = broadcasted
# source: https://www.ceskatelevize.cz/porady/10132144283-hazena/dily/
Season.1718.detailed.extended <- data.frame(Season.1718.detailed.extended,
                                                "CT.Broadcast" = 0)
Season.1718.detailed.extended$CT.Broadcast[107] <- 1 #BRN-LOV
Season.1718.detailed.extended$CT.Broadcast[116] <- 1 #JIC-KOP
Season.1718.detailed.extended$CT.Broadcast[140] <- 1 #FRM-KAR
Season.1718.detailed.extended$CT.Broadcast[147] <- 1 #HRA-ZUB
Season.1718.detailed.extended$CT.Broadcast[181] <- 1 #LOV-DUK

#Create separate columns for home team and away team halftime and 7m scores
# 4.1. Transform the score-column from string format "XX:YY" into two columns for host and away team
Season.1718.detailed.extended <- cbind(Season.1718.detailed.extended, "Home.Team.Halftime.Goals" = rep(NA, length(Season.1718.detailed.extended$Halftime.Score)),
                              "Away.Team.Halftime.Goals" = rep(NA, length(Season.1718.detailed.extended$Halftime.Score)))
Results.parsed <- strsplit(Season.1718.detailed.extended$Halftime.Score,":")
for (i in 1:length(Season.1718.detailed.extended$Halftime.Score)) {
  Season.1718.detailed.extended$Home.Team.Halftime.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1718.detailed.extended$Away.Team.Halftime.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1718.detailed.extended <- Season.1718.detailed.extended %>% select(-Halftime.Score) #Remove unneccessary column "Result"
rm(Results.parsed)
#
Season.1718.detailed.extended <- cbind(Season.1718.detailed.extended, "Home.Team.7m.Goals" = rep(NA, length(Season.1718.detailed.extended$Seven.m.goals)),
                                       "Away.Team.7m.Goals" = rep(NA, length(Season.1718.detailed.extended$Seven.m.goals)))
Results.parsed <- strsplit(Season.1718.detailed.extended$Seven.m.goals,":")
for (i in 1:length(Season.1718.detailed.extended$Seven.m.goals)) {
  Season.1718.detailed.extended$Home.Team.7m.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1718.detailed.extended$Away.Team.7m.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1718.detailed.extended <- Season.1718.detailed.extended %>% select(-Seven.m.goals) #Remove unneccessary column "Result"
rm(Results.parsed)

#### 1617 SEASON ####

# 0. Add local.match.ID info as the first column
Season.1617.detailed.extended <- data.frame("Local.Match.ID" = c(1:nrow(Season.1617.detailed.extended)),
                                            Season.1617.detailed.extended)

## 1. Add info about Season to the dataset
Season.1617.detailed.extended <- data.frame(Season.1617.detailed.extended,
                                            "Season" = "2016/2017")

## 3. Add info if the match was broadcasted by Ceska televize
# 0 = not broadcasted. 1 = broadcasted
# source: https://www.ceskatelevize.cz/porady/10132144283-hazena/dily/
Season.1617.detailed.extended <- data.frame(Season.1617.detailed.extended,
                                            "CT.Broadcast" = 0)
Season.1617.detailed.extended$CT.Broadcast[3] <- 1 #DUK-PLZ
Season.1617.detailed.extended$CT.Broadcast[61] <- 1 #PLZ-KAR
Season.1617.detailed.extended$CT.Broadcast[67] <- 1 #JIC-DUK
Season.1617.detailed.extended$CT.Broadcast[73] <- 1 #ZUB-LOV
Season.1617.detailed.extended$CT.Broadcast[79] <- 1 #LOV-DUK
Season.1617.detailed.extended$CT.Broadcast[90] <- 1 #NOV-PLZ
Season.1617.detailed.extended$CT.Broadcast[91] <- 1 #HRA-DUK
Season.1617.detailed.extended$CT.Broadcast[101] <- 1 #BRN-LOV
Season.1617.detailed.extended$CT.Broadcast[108] <- 1 #ZUB-DUK
Season.1617.detailed.extended$CT.Broadcast[123] <- 1 #FRM-LOV
Season.1617.detailed.extended$CT.Broadcast[133] <- 1 #HRA-FRM
Season.1617.detailed.extended$CT.Broadcast[157] <- 1 #ZUB-PLZ
Season.1617.detailed.extended$CT.Broadcast[173] <- 1 #NOV-KAR

#Create separate columns for home team and away team halftime and 7m scores
# 4.1. Transform the score-column from string format "XX:YY" into two columns for host and away team
Season.1617.detailed.extended <- cbind(Season.1617.detailed.extended, "Home.Team.Halftime.Goals" = rep(NA, length(Season.1617.detailed.extended$Halftime.Score)),
                                       "Away.Team.Halftime.Goals" = rep(NA, length(Season.1617.detailed.extended$Halftime.Score)))
Results.parsed <- strsplit(Season.1617.detailed.extended$Halftime.Score,":")
for (i in 1:length(Season.1617.detailed.extended$Halftime.Score)) {
  Season.1617.detailed.extended$Home.Team.Halftime.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1617.detailed.extended$Away.Team.Halftime.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1617.detailed.extended <- Season.1617.detailed.extended %>% select(-Halftime.Score) #Remove unneccessary column "Result"
rm(Results.parsed)
#
Season.1617.detailed.extended <- cbind(Season.1617.detailed.extended, "Home.Team.7m.Goals" = rep(NA, length(Season.1617.detailed.extended$Seven.m.goals)),
                                       "Away.Team.7m.Goals" = rep(NA, length(Season.1617.detailed.extended$Seven.m.goals)))
Results.parsed <- strsplit(Season.1617.detailed.extended$Seven.m.goals,":")
for (i in 1:length(Season.1617.detailed.extended$Seven.m.goals)) {
  Season.1617.detailed.extended$Home.Team.7m.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1617.detailed.extended$Away.Team.7m.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1617.detailed.extended <- Season.1617.detailed.extended %>% select(-Seven.m.goals) #Remove unneccessary column "Result"
rm(Results.parsed)
#### 1516 SEASON ####

# 0. Add local.match.ID info as the first column
Season.1516.detailed.extended <- data.frame("Local.Match.ID" = c(1:nrow(Season.1516.detailed.extended)),
                                            Season.1516.detailed.extended)

## 1. Add info about Season to the dataset
Season.1516.detailed.extended <- data.frame(Season.1516.detailed.extended,
                                            "Season" = "2015/2016")

## 3. Add info if the match was broadcasted by Ceska televize
# 0 = not broadcasted. 1 = broadcasted
# source: https://www.ceskatelevize.cz/porady/10132144283-hazena/dily/
Season.1516.detailed.extended <- data.frame(Season.1516.detailed.extended,
                                            "CT.Broadcast" = 0)
Season.1516.detailed.extended$CT.Broadcast[4] <- 1 #PLZ-DUK
Season.1516.detailed.extended$CT.Broadcast[6] <- 1 #HRA-KAR
Season.1516.detailed.extended$CT.Broadcast[14] <- 1 #PLZ-HRA
Season.1516.detailed.extended$CT.Broadcast[24] <- 1 #ZUB-LOV
Season.1516.detailed.extended$CT.Broadcast[49] <- 1 #HRA-KAR
Season.1516.detailed.extended$CT.Broadcast[59] <- 1 #PLZ-DUK
Season.1516.detailed.extended$CT.Broadcast[75] <- 1 #LOV-PLZ
Season.1516.detailed.extended$CT.Broadcast[93] <- 1 #ZUB-DUK
Season.1516.detailed.extended$CT.Broadcast[97] <- 1 #LOV-FRM
Season.1516.detailed.extended$CT.Broadcast[111] <- 1 #KAR-DUK
Season.1516.detailed.extended$CT.Broadcast[117] <- 1 #KAR-HRA
Season.1516.detailed.extended$CT.Broadcast[121] <- 1 #HRA-LOV
Season.1516.detailed.extended$CT.Broadcast[133] <- 1 #KAR-PLZ
Season.1516.detailed.extended$CT.Broadcast[141] <- 1 #JIC-DUK
Season.1516.detailed.extended$CT.Broadcast[154] <- 1 #FRM-HRA
Season.1516.detailed.extended$CT.Broadcast[170] <- 1 #PLZ-FRM

#Create separate columns for home team and away team halftime and 7m scores
# 4.1. Transform the score-column from string format "XX:YY" into two columns for host and away team
Season.1516.detailed.extended <- cbind(Season.1516.detailed.extended, "Home.Team.Halftime.Goals" = rep(NA, length(Season.1516.detailed.extended$Halftime.Score)),
                                       "Away.Team.Halftime.Goals" = rep(NA, length(Season.1516.detailed.extended$Halftime.Score)))
Results.parsed <- strsplit(Season.1516.detailed.extended$Halftime.Score,":")
for (i in 1:length(Season.1516.detailed.extended$Halftime.Score)) {
  Season.1516.detailed.extended$Home.Team.Halftime.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1516.detailed.extended$Away.Team.Halftime.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1516.detailed.extended <- Season.1516.detailed.extended %>% select(-Halftime.Score) #Remove unneccessary column "Result"
rm(Results.parsed)
#
Season.1516.detailed.extended <- cbind(Season.1516.detailed.extended, "Home.Team.7m.Goals" = rep(NA, length(Season.1516.detailed.extended$Seven.m.goals)),
                                       "Away.Team.7m.Goals" = rep(NA, length(Season.1516.detailed.extended$Seven.m.goals)))
Results.parsed <- strsplit(Season.1516.detailed.extended$Seven.m.goals,":")
for (i in 1:length(Season.1516.detailed.extended$Seven.m.goals)) {
  Season.1516.detailed.extended$Home.Team.7m.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1516.detailed.extended$Away.Team.7m.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1516.detailed.extended <- Season.1516.detailed.extended %>% select(-Seven.m.goals) #Remove unneccessary column "Result"
rm(Results.parsed)

#### 1415 SEASON ####

# 0. Add local.match.ID info as the first column
Season.1415.detailed.extended <- data.frame("Local.Match.ID" = c(1:nrow(Season.1415.detailed.extended)),
                                            Season.1415.detailed.extended)

## 1. Add info about Season to the dataset
Season.1415.detailed.extended <- data.frame(Season.1415.detailed.extended,
                                            "Season" = "2014/2015")

## 3. Add info if the match was broadcasted by Ceska televize
# 0 = not broadcasted. 1 = broadcasted
# source: https://www.ceskatelevize.cz/porady/10132144283-hazena/dily/
Season.1415.detailed.extended <- data.frame(Season.1415.detailed.extended,
                                            "CT.Broadcast" = 0)
Season.1415.detailed.extended$CT.Broadcast[7] <- 1 #KAR-BRN
Season.1415.detailed.extended$CT.Broadcast[3] <- 1 #LOV-PLZ
Season.1415.detailed.extended$CT.Broadcast[2] <- 1 #PLZ-LOV
Season.1415.detailed.extended$CT.Broadcast[1] <- 1 #PLZ-LOV
Season.1415.detailed.extended$CT.Broadcast[16] <- 1 #LOV-BRN
Season.1415.detailed.extended$CT.Broadcast[14] <- 1 #PLZ-KAR
Season.1415.detailed.extended$CT.Broadcast[12] <- 1 #BRN-LOV
Season.1415.detailed.extended$CT.Broadcast[36] <- 1 #FRM-PLZ
Season.1415.detailed.extended$CT.Broadcast[32] <- 1 #LOV-DUK
Season.1415.detailed.extended$CT.Broadcast[29] <- 1 #ZUB-KAR
Season.1415.detailed.extended$CT.Broadcast[54] <- 1 #HRA-BRN
Season.1415.detailed.extended$CT.Broadcast[64] <- 1 #JIC-DUK
Season.1415.detailed.extended$CT.Broadcast[77] <- 1 #PLZ-HRA
Season.1415.detailed.extended$CT.Broadcast[118] <- 1 #BRN-HRA
Season.1415.detailed.extended$CT.Broadcast[137] <- 1 #PLZ-DUK
Season.1415.detailed.extended$CT.Broadcast[142] <- 1 #FRM-ZUB
Season.1415.detailed.extended$CT.Broadcast[153] <- 1 #LOV-KAR
Season.1415.detailed.extended$CT.Broadcast[167] <- 1 #HRA-JIC
Season.1415.detailed.extended$CT.Broadcast[180] <- 1 #LIT-DUK

#Create separate columns for home team and away team halftime and 7m scores
# 4.1. Transform the score-column from string format "XX:YY" into two columns for host and away team
Season.1415.detailed.extended <- cbind(Season.1415.detailed.extended, "Home.Team.Halftime.Goals" = rep(NA, length(Season.1415.detailed.extended$Halftime.Score)),
                                       "Away.Team.Halftime.Goals" = rep(NA, length(Season.1415.detailed.extended$Halftime.Score)))
Results.parsed <- strsplit(Season.1415.detailed.extended$Halftime.Score,":")
for (i in 1:length(Season.1415.detailed.extended$Halftime.Score)) {
  Season.1415.detailed.extended$Home.Team.Halftime.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1415.detailed.extended$Away.Team.Halftime.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1415.detailed.extended <- Season.1415.detailed.extended %>% select(-Halftime.Score) #Remove unneccessary column "Result"
rm(Results.parsed)
#
Season.1415.detailed.extended <- cbind(Season.1415.detailed.extended, "Home.Team.7m.Goals" = rep(NA, length(Season.1415.detailed.extended$Seven.m.goals)),
                                       "Away.Team.7m.Goals" = rep(NA, length(Season.1415.detailed.extended$Seven.m.goals)))
Results.parsed <- strsplit(Season.1415.detailed.extended$Seven.m.goals,":")
for (i in 1:length(Season.1415.detailed.extended$Seven.m.goals)) {
  Season.1415.detailed.extended$Home.Team.7m.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1415.detailed.extended$Away.Team.7m.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1415.detailed.extended <- Season.1415.detailed.extended %>% select(-Seven.m.goals) #Remove unneccessary column "Result"
rm(Results.parsed)
#### 1314 SEASON ####

# 0. Add local.match.ID info as the first column
Season.1314.detailed.extended <- data.frame("Local.Match.ID" = c(1:nrow(Season.1314.detailed.extended)),
                                            Season.1314.detailed.extended)

## 1. Add info about Season to the dataset
Season.1314.detailed.extended <- data.frame(Season.1314.detailed.extended,
                                            "Season" = "2013/2014")

## 3. Add info if the match was broadcasted by Ceska televize
# 0 = not broadcasted. 1 = broadcasted
# source: https://www.ceskatelevize.cz/porady/10132144283-hazena/dily/
Season.1314.detailed.extended <- data.frame(Season.1314.detailed.extended,
                                            "CT.Broadcast" = 0)
Season.1314.detailed.extended$CT.Broadcast[10] <- 1 #LOV-PLZ
Season.1314.detailed.extended$CT.Broadcast[13] <- 1 #HRA-JIC
Season.1314.detailed.extended$CT.Broadcast[26] <- 1 #PLZ-LOV
Season.1314.detailed.extended$CT.Broadcast[66] <- 1 #FRM-LOV
Season.1314.detailed.extended$CT.Broadcast[87] <- 1 #JIC-PLZ
Season.1314.detailed.extended$CT.Broadcast[93] <- 1 #BRN-DUK
Season.1314.detailed.extended$CT.Broadcast[114] <- 1 #LOV-PLZ
Season.1314.detailed.extended$CT.Broadcast[124] <- 1 #ZUB-DUK
Season.1314.detailed.extended$CT.Broadcast[130] <- 1 #HRA-KAR
Season.1314.detailed.extended$CT.Broadcast[141] <- 1 #LOV-DUK
Season.1314.detailed.extended$CT.Broadcast[148] <- 1 #ZUB-HRA
Season.1314.detailed.extended$CT.Broadcast[190] <- 1 #BRN-LOV

#Create separate columns for home team and away team halftime and 7m scores
# 4.1. Transform the score-column from string format "XX:YY" into two columns for host and away team
Season.1314.detailed.extended <- cbind(Season.1314.detailed.extended, "Home.Team.Halftime.Goals" = rep(NA, length(Season.1314.detailed.extended$Halftime.Score)),
                                       "Away.Team.Halftime.Goals" = rep(NA, length(Season.1314.detailed.extended$Halftime.Score)))
Results.parsed <- strsplit(Season.1314.detailed.extended$Halftime.Score,":")
for (i in 1:length(Season.1314.detailed.extended$Halftime.Score)) {
  Season.1314.detailed.extended$Home.Team.Halftime.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1314.detailed.extended$Away.Team.Halftime.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1314.detailed.extended <- Season.1314.detailed.extended %>% select(-Halftime.Score) #Remove unneccessary column "Result"
rm(Results.parsed)
#
Season.1314.detailed.extended <- cbind(Season.1314.detailed.extended, "Home.Team.7m.Goals" = rep(NA, length(Season.1314.detailed.extended$Seven.m.goals)),
                                       "Away.Team.7m.Goals" = rep(NA, length(Season.1314.detailed.extended$Seven.m.goals)))
Results.parsed <- strsplit(Season.1314.detailed.extended$Seven.m.goals,":")
for (i in 1:length(Season.1314.detailed.extended$Seven.m.goals)) {
  Season.1314.detailed.extended$Home.Team.7m.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1314.detailed.extended$Away.Team.7m.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1314.detailed.extended <- Season.1314.detailed.extended %>% select(-Seven.m.goals) #Remove unneccessary column "Result"
rm(Results.parsed)

#### 1213 SEASON ####

# 0. Add local.match.ID info as the first column
Season.1213.detailed.extended <- data.frame("Local.Match.ID" = c(1:nrow(Season.1213.detailed.extended)),
                                            Season.1213.detailed.extended)

## 1. Add info about Season to the dataset
Season.1213.detailed.extended <- data.frame(Season.1213.detailed.extended,
                                            "Season" = "2012/2013")

## 3. Add info if the match was broadcasted by Ceska televize
# 0 = not broadcasted. 1 = broadcasted
# source: https://www.ceskatelevize.cz/porady/10132144283-hazena/dily/
Season.1213.detailed.extended <- data.frame(Season.1213.detailed.extended,
                                            "CT.Broadcast" = 0)
Season.1213.detailed.extended$CT.Broadcast[1] <- 1 #LOV-DUK
Season.1213.detailed.extended$CT.Broadcast[6] <- 1 #HRA-LOV
Season.1213.detailed.extended$CT.Broadcast[11] <- 1 #LOV-JIC
Season.1213.detailed.extended$CT.Broadcast[14] <- 1 #ZUB-JIC
Season.1213.detailed.extended$CT.Broadcast[16] <- 1 #HRA-ZUB
Season.1213.detailed.extended$CT.Broadcast[21] <- 1 #JIC-DUK
Season.1213.detailed.extended$CT.Broadcast[22] <- 1 #LOV-ZUB
Season.1213.detailed.extended$CT.Broadcast[30] <- 1 #HRA-DUK
Season.1213.detailed.extended$CT.Broadcast[54] <- 1 #JIC-KAR
Season.1213.detailed.extended$CT.Broadcast[59] <- 1 #KAR-HRA
Season.1213.detailed.extended$CT.Broadcast[75] <- 1 #HRA-FRM
Season.1213.detailed.extended$CT.Broadcast[81] <- 1 #LOV-HRA
Season.1213.detailed.extended$CT.Broadcast[90] <- 1 #PLZ-DUK
Season.1213.detailed.extended$CT.Broadcast[95] <- 1 #LOV-ZUB
Season.1213.detailed.extended$CT.Broadcast[99] <- 1 #FRM-BRN
Season.1213.detailed.extended$CT.Broadcast[102] <- 1 #BRN-JIC
Season.1213.detailed.extended$CT.Broadcast[109] <- 1 #BRN-HRA
Season.1213.detailed.extended$CT.Broadcast[114] <- 1 #HRA-KAR
Season.1213.detailed.extended$CT.Broadcast[119] <- 1 #PLZ-JIC
Season.1213.detailed.extended$CT.Broadcast[124] <- 1 #LOV-DUK
Season.1213.detailed.extended$CT.Broadcast[129] <- 1 #PLZ-ZUB
Season.1213.detailed.extended$CT.Broadcast[133] <- 1 #DUK-KAR
Season.1213.detailed.extended$CT.Broadcast[139] <- 1 #JIC-ZUB
Season.1213.detailed.extended$CT.Broadcast[145] <- 1 #LOV-KAR
Season.1213.detailed.extended$CT.Broadcast[148] <- 1 #ZUB-LOV
Season.1213.detailed.extended$CT.Broadcast[154] <- 1 #DUK-JIC
Season.1213.detailed.extended$CT.Broadcast[158] <- 1 #HRA-DUK

#Create separate columns for home team and away team halftime and 7m scores
# 4.1. Transform the score-column from string format "XX:YY" into two columns for host and away team
Season.1213.detailed.extended <- cbind(Season.1213.detailed.extended, "Home.Team.Halftime.Goals" = rep(NA, length(Season.1213.detailed.extended$Halftime.Score)),
                                       "Away.Team.Halftime.Goals" = rep(NA, length(Season.1213.detailed.extended$Halftime.Score)))
Results.parsed <- strsplit(Season.1213.detailed.extended$Halftime.Score,":")
for (i in 1:length(Season.1213.detailed.extended$Halftime.Score)) {
  Season.1213.detailed.extended$Home.Team.Halftime.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1213.detailed.extended$Away.Team.Halftime.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1213.detailed.extended <- Season.1213.detailed.extended %>% select(-Halftime.Score) #Remove unneccessary column "Result"
rm(Results.parsed)
#
Season.1213.detailed.extended <- cbind(Season.1213.detailed.extended, "Home.Team.7m.Goals" = rep(NA, length(Season.1213.detailed.extended$Seven.m.goals)),
                                       "Away.Team.7m.Goals" = rep(NA, length(Season.1213.detailed.extended$Seven.m.goals)))
Results.parsed <- strsplit(Season.1213.detailed.extended$Seven.m.goals,":")
for (i in 1:length(Season.1213.detailed.extended$Seven.m.goals)) {
  Season.1213.detailed.extended$Home.Team.7m.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1213.detailed.extended$Away.Team.7m.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1213.detailed.extended <- Season.1213.detailed.extended %>% select(-Seven.m.goals) #Remove unneccessary column "Result"
rm(Results.parsed)
#### 1112 SEASON ####

# 0. Add local.match.ID info as the first column
Season.1112.detailed.extended <- data.frame("Local.Match.ID" = c(1:nrow(Season.1112.detailed.extended)),
                                            Season.1112.detailed.extended)

## 1. Add info about Season to the dataset
Season.1112.detailed.extended <- data.frame(Season.1112.detailed.extended,
                                            "Season" = "2011/2012")

## 3. Add info if the match was broadcasted by Ceska televize
# 0 = not broadcasted. 1 = broadcasted
# source: https://www.ceskatelevize.cz/porady/10132144283-hazena/dily/
Season.1112.detailed.extended <- data.frame(Season.1112.detailed.extended,
                                            "CT.Broadcast" = 0)
Season.1112.detailed.extended$CT.Broadcast[3] <- 1 #ZUB-JIC
Season.1112.detailed.extended$CT.Broadcast[6] <- 1 #JIC-DUK
Season.1112.detailed.extended$CT.Broadcast[11] <- 1 #ZUB-DUK
Season.1112.detailed.extended$CT.Broadcast[16] <- 1 #LOV-DUK
Season.1112.detailed.extended$CT.Broadcast[21] <- 1 #ZUB-KAR
Season.1112.detailed.extended$CT.Broadcast[23] <- 1 #KAR-DUK
Season.1112.detailed.extended$CT.Broadcast[30] <- 1 #KAR-JIC
Season.1112.detailed.extended$CT.Broadcast[65] <- 1 #LOV-KAR
Season.1112.detailed.extended$CT.Broadcast[77] <- 1 #JIC-KAR
Season.1112.detailed.extended$CT.Broadcast[81] <- 1 #LOV-FRM
Season.1112.detailed.extended$CT.Broadcast[87] <- 1 #JIC-LOV
Season.1112.detailed.extended$CT.Broadcast[93] <- 1 #FRM-JIC
Season.1112.detailed.extended$CT.Broadcast[107] <- 1 #KAR-DUK
Season.1112.detailed.extended$CT.Broadcast[111] <- 1 #HRA-LOV
Season.1112.detailed.extended$CT.Broadcast[116] <- 1 #LOV-DUK
Season.1112.detailed.extended$CT.Broadcast[125] <- 1 #HRA-JIC
Season.1112.detailed.extended$CT.Broadcast[131] <- 1 #PRE-FRM
Season.1112.detailed.extended$CT.Broadcast[136] <- 1 #LOV-PLZ
Season.1112.detailed.extended$CT.Broadcast[141] <- 1 #HRA-ZUB
Season.1112.detailed.extended$CT.Broadcast[149] <- 1 #ZUB-DUK
Season.1112.detailed.extended$CT.Broadcast[155] <- 1 #LOV-JIC
Season.1112.detailed.extended$CT.Broadcast[162] <- 1 #ZUB-KAR
Season.1112.detailed.extended$CT.Broadcast[159] <- 1 #JIC-FRM
Season.1112.detailed.extended$CT.Broadcast[168] <- 1 #KAR-HRA
Season.1112.detailed.extended$CT.Broadcast[173] <- 1 #ZUB-LOV
Season.1112.detailed.extended$CT.Broadcast[181] <- 1 #DUK-LOV
Season.1112.detailed.extended$CT.Broadcast[190] <- 1 #LOV-PRE

#Create separate columns for home team and away team halftime and 7m scores
# 4.1. Transform the score-column from string format "XX:YY" into two columns for host and away team
Season.1112.detailed.extended <- cbind(Season.1112.detailed.extended, "Home.Team.Halftime.Goals" = rep(NA, length(Season.1112.detailed.extended$Halftime.Score)),
                                       "Away.Team.Halftime.Goals" = rep(NA, length(Season.1112.detailed.extended$Halftime.Score)))
Results.parsed <- strsplit(Season.1112.detailed.extended$Halftime.Score,":")
for (i in 1:length(Season.1112.detailed.extended$Halftime.Score)) {
  Season.1112.detailed.extended$Home.Team.Halftime.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1112.detailed.extended$Away.Team.Halftime.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1112.detailed.extended <- Season.1112.detailed.extended %>% select(-Halftime.Score) #Remove unneccessary column "Result"
rm(Results.parsed)
#
Season.1112.detailed.extended <- cbind(Season.1112.detailed.extended, "Home.Team.7m.Goals" = rep(NA, length(Season.1112.detailed.extended$Seven.m.goals)),
                                       "Away.Team.7m.Goals" = rep(NA, length(Season.1112.detailed.extended$Seven.m.goals)))
Results.parsed <- strsplit(Season.1112.detailed.extended$Seven.m.goals,":")
for (i in 1:length(Season.1112.detailed.extended$Seven.m.goals)) {
  Season.1112.detailed.extended$Home.Team.7m.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1112.detailed.extended$Away.Team.7m.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1112.detailed.extended <- Season.1112.detailed.extended %>% select(-Seven.m.goals) #Remove unneccessary column "Result"
rm(Results.parsed)
#### 1011 SEASON ####

# 0. Add local.match.ID info as the first column
Season.1011.detailed.extended <- data.frame("Local.Match.ID" = c(1:nrow(Season.1011.detailed.extended)),
                                            Season.1011.detailed.extended)

## 1. Add info about Season to the dataset
Season.1011.detailed.extended <- data.frame(Season.1011.detailed.extended,
                                            "Season" = "2010/2011")

## 3. Add info if the match was broadcasted by Ceska televize
# 0 = not broadcasted. 1 = broadcasted
# source: https://www.ceskatelevize.cz/porady/10132144283-hazena/dily/
Season.1011.detailed.extended <- data.frame(Season.1011.detailed.extended,
                                            "CT.Broadcast" = 0)
Season.1011.detailed.extended$CT.Broadcast[3] <- 1 #LOV-DUK
Season.1011.detailed.extended$CT.Broadcast[6] <- 1 #PLZ-HRA
Season.1011.detailed.extended$CT.Broadcast[9] <- 1 #HRA-LOV
Season.1011.detailed.extended$CT.Broadcast[11] <- 1 #LOV-ZUB
Season.1011.detailed.extended$CT.Broadcast[17] <- 1 #PLZ-ZUB
Season.1011.detailed.extended$CT.Broadcast[21] <- 1 #LOV-KAR
Season.1011.detailed.extended$CT.Broadcast[24] <- 1 #KAR-ZUB
Season.1011.detailed.extended$CT.Broadcast[26] <- 1 #ZUB-LOV
Season.1011.detailed.extended$CT.Broadcast[30] <- 1 #KAR-DUK
Season.1011.detailed.extended$CT.Broadcast[65] <- 1 #PLZ-KAR
Season.1011.detailed.extended$CT.Broadcast[71] <- 1 #ZUB-PLZ
Season.1011.detailed.extended$CT.Broadcast[75] <- 1 #HRA-KAR
Season.1011.detailed.extended$CT.Broadcast[82] <- 1 #LOV-JIC
Season.1011.detailed.extended$CT.Broadcast[89] <- 1 #HRA-DUK
Season.1011.detailed.extended$CT.Broadcast[94] <- 1 #ZUB-JIC
Season.1011.detailed.extended$CT.Broadcast[99] <- 1 #LOV-KAR
Season.1011.detailed.extended$CT.Broadcast[105] <- 1 #ZUB-LOV
Season.1011.detailed.extended$CT.Broadcast[112] <- 1 #KAR-ZUB
Season.1011.detailed.extended$CT.Broadcast[124] <- 1 #ZUB-DUK
Season.1011.detailed.extended$CT.Broadcast[128] <- 1 #KAR-PLZ
Season.1011.detailed.extended$CT.Broadcast[143] <- 1 #DUK-PLZ
Season.1011.detailed.extended$CT.Broadcast[139] <- 1 #BRN-PRE
Season.1011.detailed.extended$CT.Broadcast[148] <- 1 #JIC-LOV
Season.1011.detailed.extended$CT.Broadcast[154] <- 1 #DUK-HRA
Season.1011.detailed.extended$CT.Broadcast[165] <- 1 #KAR-LOV
Season.1011.detailed.extended$CT.Broadcast[171] <- 1 #PRE-KAR
Season.1011.detailed.extended$CT.Broadcast[179] <- 1 #ZUB-KAR
Season.1011.detailed.extended$CT.Broadcast[181] <- 1 #JIC-TRE

#Create separate columns for home team and away team halftime and 7m scores
# 4.1. Transform the score-column from string format "XX:YY" into two columns for host and away team
Season.1011.detailed.extended <- cbind(Season.1011.detailed.extended, "Home.Team.Halftime.Goals" = rep(NA, length(Season.1011.detailed.extended$Halftime.Score)),
                                       "Away.Team.Halftime.Goals" = rep(NA, length(Season.1011.detailed.extended$Halftime.Score)))
Results.parsed <- strsplit(Season.1011.detailed.extended$Halftime.Score,":")
for (i in 1:length(Season.1011.detailed.extended$Halftime.Score)) {
  Season.1011.detailed.extended$Home.Team.Halftime.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1011.detailed.extended$Away.Team.Halftime.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1011.detailed.extended <- Season.1011.detailed.extended %>% select(-Halftime.Score) #Remove unneccessary column "Result"
rm(Results.parsed)
#
Season.1011.detailed.extended <- cbind(Season.1011.detailed.extended, "Home.Team.7m.Goals" = rep(NA, length(Season.1011.detailed.extended$Seven.m.goals)),
                                       "Away.Team.7m.Goals" = rep(NA, length(Season.1011.detailed.extended$Seven.m.goals)))
Results.parsed <- strsplit(Season.1011.detailed.extended$Seven.m.goals,":")
for (i in 1:length(Season.1011.detailed.extended$Seven.m.goals)) {
  Season.1011.detailed.extended$Home.Team.7m.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.1011.detailed.extended$Away.Team.7m.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.1011.detailed.extended <- Season.1011.detailed.extended %>% select(-Seven.m.goals) #Remove unneccessary column "Result"
rm(Results.parsed)
#### 0910 SEASON ####

# 0. Add local.match.ID info as the first column
Season.0910.detailed.extended <- data.frame("Local.Match.ID" = c(1:nrow(Season.0910.detailed.extended)),
                                            Season.0910.detailed.extended)

## 1. Add info about Season to the dataset
Season.0910.detailed.extended <- data.frame(Season.0910.detailed.extended,
                                            "Season" = "2009/2010")

## 3. Add info if the match was broadcasted by Ceska televize
# 0 = not broadcasted. 1 = broadcasted
# source: https://www.ceskatelevize.cz/porady/10132144283-hazena/dily/
Season.0910.detailed.extended <- data.frame(Season.0910.detailed.extended,
                                            "CT.Broadcast" = 0)
Season.0910.detailed.extended$CT.Broadcast[4] <- 1 #KAR-ZUB
Season.0910.detailed.extended$CT.Broadcast[3] <- 1 #ZUB-KAR
Season.0910.detailed.extended$CT.Broadcast[1] <- 1 #ZUB-KAR
Season.0910.detailed.extended$CT.Broadcast[2] <- 1 #KAR-ZUB ZAPAS 18.04. DLE CT
Season.0910.detailed.extended$CT.Broadcast[15] <- 1 #DUK-KAR
Season.0910.detailed.extended$CT.Broadcast[11] <- 1 #ZUB-HRA
Season.0910.detailed.extended$CT.Broadcast[28] <- 1 #KAR-PLZ
Season.0910.detailed.extended$CT.Broadcast[49] <- 1 #LOV-KOP
Season.0910.detailed.extended$CT.Broadcast[69] <- 1 #PRE-LOV
Season.0910.detailed.extended$CT.Broadcast[84] <- 1 #KAR-HRA
Season.0910.detailed.extended$CT.Broadcast[88] <- 1 #LOV-KAR
Season.0910.detailed.extended$CT.Broadcast[95] <- 1 #KAR-DUK
Season.0910.detailed.extended$CT.Broadcast[103] <- 1 #ZUB-KAR
Season.0910.detailed.extended$CT.Broadcast[115] <- 1 #KOP-LOV
Season.0910.detailed.extended$CT.Broadcast[121] <- 1 #HRA-JIC
Season.0910.detailed.extended$CT.Broadcast[128] <- 1 #PLZ-DUK
Season.0910.detailed.extended$CT.Broadcast[132] <- 1 #LOV-HRA
Season.0910.detailed.extended$CT.Broadcast[144] <- 1 #FRM-PLZ
Season.0910.detailed.extended$CT.Broadcast[151] <- 1 #LOV-ZUB
Season.0910.detailed.extended$CT.Broadcast[158] <- 1 #ZUB-DUK
Season.0910.detailed.extended$CT.Broadcast[163] <- 1 #DUK-KAR
Season.0910.detailed.extended$CT.Broadcast[167] <- 1 #DVK-HRA
Season.0910.detailed.extended$CT.Broadcast[174] <- 1 #JIC-

#Create separate columns for home team and away team halftime and 7m scores
# 4.1. Transform the score-column from string format "XX:YY" into two columns for host and away team
Season.0910.detailed.extended <- cbind(Season.0910.detailed.extended, "Home.Team.Halftime.Goals" = rep(NA, length(Season.0910.detailed.extended$Halftime.Score)),
                                       "Away.Team.Halftime.Goals" = rep(NA, length(Season.0910.detailed.extended$Halftime.Score)))
Results.parsed <- strsplit(Season.0910.detailed.extended$Halftime.Score,":")
for (i in 1:length(Season.0910.detailed.extended$Halftime.Score)) {
  Season.0910.detailed.extended$Home.Team.Halftime.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.0910.detailed.extended$Away.Team.Halftime.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.0910.detailed.extended <- Season.0910.detailed.extended %>% select(-Halftime.Score) #Remove unneccessary column "Result"
rm(Results.parsed)
#
Season.0910.detailed.extended <- cbind(Season.0910.detailed.extended, "Home.Team.7m.Goals" = rep(NA, length(Season.0910.detailed.extended$Seven.m.goals)),
                                       "Away.Team.7m.Goals" = rep(NA, length(Season.0910.detailed.extended$Seven.m.goals)))
Results.parsed <- strsplit(Season.0910.detailed.extended$Seven.m.goals,":")
for (i in 1:length(Season.0910.detailed.extended$Seven.m.goals)) {
  Season.0910.detailed.extended$Home.Team.7m.Goals[i] <- as.numeric(Results.parsed[[i]][1])
  Season.0910.detailed.extended$Away.Team.7m.Goals[i] <- as.numeric(Results.parsed[[i]][2])
}
Season.0910.detailed.extended <- Season.0910.detailed.extended %>% select(-Seven.m.goals) #Remove unneccessary column "Result"
rm(Results.parsed)
##

#### SAVE EXTENDED DATASETS ####
write.csv(Season.1819Fall.detailed.extended, file = "csv_detailed_extended_match_info/Season1819Fall_detailed_extended_info.csv",row.names=FALSE, na="")
write.csv(Season.1718.detailed.extended, file = "csv_detailed_extended_match_info/Season1718_detailed_extended_info.csv",row.names=FALSE, na="")
write.csv(Season.1617.detailed.extended, file = "csv_detailed_extended_match_info/Season1617_detailed_extended_info.csv",row.names=FALSE, na="")
write.csv(Season.1516.detailed.extended, file = "csv_detailed_extended_match_info/Season1516_detailed_extended_info.csv",row.names=FALSE, na="")
write.csv(Season.1415.detailed.extended, file = "csv_detailed_extended_match_info/Season1415_detailed_extended_info.csv",row.names=FALSE, na="")
write.csv(Season.1314.detailed.extended, file = "csv_detailed_extended_match_info/Season1314_detailed_extended_info.csv",row.names=FALSE, na="")
write.csv(Season.1213.detailed.extended, file = "csv_detailed_extended_match_info/Season1213_detailed_extended_info.csv",row.names=FALSE, na="")
write.csv(Season.1112.detailed.extended, file = "csv_detailed_extended_match_info/Season1112_detailed_extended_info.csv",row.names=FALSE, na="")
write.csv(Season.1011.detailed.extended, file = "csv_detailed_extended_match_info/Season1011_detailed_extended_info.csv",row.names=FALSE, na="")
write.csv(Season.0910.detailed.extended, file = "csv_detailed_extended_match_info/Season0910_detailed_extended_info.csv",row.names=FALSE, na="")
