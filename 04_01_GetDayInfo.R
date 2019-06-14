## GET DAY INFO FOR ALL DAYS BETWEEN 2011 AND 2020
library(dplyr)

#Get dates
days <- data.frame("Date" = seq.Date(as.Date("2009/1/1"), as.Date("2020/12/31"), by = "day"))

#Get Day names
week <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
days <- data.frame(days,"Day.Name" = c("Thu", "Fri", "Sat", "Sun",
                                       rep(week,625), "Mon", "Tue", "Wed", "Thu"))

#Get Day status: WorkingDay vs. Weekend
days <- days %>% mutate("Day.Status" = ifelse(Day.Name %in% c("Sat", "Sun"),
                                              "Weekend",
                                              "WorkingDay"))

#Get public holidays (without Easter Friday, Easter Monday)
days <- days %>% mutate("Public.Holiday" = ifelse((grepl("01-01", as.character(Date))) | 
                                                    (grepl("05-01", as.character(Date))) |
                                                    (grepl("05-08", as.character(Date))) |
                                                    (grepl("07-05", as.character(Date))) |
                                                    (grepl("07-06", as.character(Date))) |
                                                    (grepl("09-28", as.character(Date))) |
                                                    (grepl("10-28", as.character(Date))) |
                                                    (grepl("11-17", as.character(Date))) |
                                                    (grepl("12-24", as.character(Date))) |
                                                    (grepl("12-25", as.character(Date))) |
                                                    (grepl("12-26", as.character(Date))), 1, 0))

#Remove unneccessary structures from workspace
rm(week)
