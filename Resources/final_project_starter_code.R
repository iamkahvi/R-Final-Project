###############################################################################
##########               FINAL PROJECT STARTER CODE                 ##########
###############################################################################
# clear workspace
# rm(list=ls())

# import required libraries
library(tidyverse)
library(readxl)
library(lubridate)

# helper function to fix time formatting
fix.time = function(x) {
  return(str_split(x, " ", simplify=T)[1,2])
}


#' clean.my.data
#'
#' @param data the full 919 report data 
#'
#' @return cleaned version of the full data (some observations removed for too many NA's)
#' @export
#'
#' @examples df = clean.my.data(data.all)
clean.my.data = function(data) {
  df = data %>%
    select(-contains("Attended")) %>%
    select(-contains("Incident")) %>%
    select(-c(`Gender`, Height, `Weight (lbs)`, `Hair Colour`, `Call initiated to: Police (911)`, 
              `Call initiated to: Fire (911)`,`Vehicle ID`, `Other: (Enter)`)) %>%
    mutate(`Call initiated to: Ambulance  (911)` = as.numeric(`Call initiated to: Ambulance  (911)`)) %>%
    mutate(`Call initiated to: Ambulance  (911)` = if_else(is.na(`Call initiated to: Ambulance  (911)`), 
                                                           0, `Call initiated to: Ambulance  (911)`)) %>%
    mutate(`Emergency Services Required` = as.numeric(`Emergency Services Required`)) %>%
    mutate(`Emergency Services Required` = if_else(is.na(`Emergency Services Required`), 
                                                   0, `Emergency Services Required`)) %>%
    mutate(`Event Location: Drop In` = as.numeric(`Event Location: Drop In`)) %>%
    mutate(`Event Location: Drop In` = if_else(is.na(`Event Location: Drop In`), 
                                               0, `Event Location: Drop In`)) %>%
    mutate(`Event Location: Lobby` = as.numeric(`Event Location: Lobby`)) %>%
    mutate(`Event Location: Lobby` = if_else(is.na(`Event Location: Lobby`), 
                                             0, `Event Location: Lobby`)) %>%
    mutate(`Event Location: Courtyard` = as.numeric(`Event Location: Courtyard`)) %>%
    mutate(`Event Location: Courtyard` = if_else(is.na(`Event Location: Courtyard`), 
                                                 0, `Event Location: Courtyard`)) %>%
    mutate(`Event Location: Kitchen` = as.numeric(`Event Location: Kitchen`)) %>%
    mutate(`Event Location: Kitchen` = if_else(is.na(`Event Location: Kitchen`), 
                                               0, `Event Location: Kitchen`)) %>%
    mutate(`Event Location: Dinning Area` = as.numeric(`Event Location: Dinning Area`)) %>%
    mutate(`Event Location: Dinning Area` = if_else(is.na(`Event Location: Dinning Area`), 
                                                    0, `Event Location: Dinning Area`)) %>%
    mutate(`Event Location: Chapel` = as.numeric(`Event Location: Chapel`)) %>%
    mutate(`Event Location: Chapel` = if_else(is.na(`Event Location: Chapel`), 
                                              0, `Event Location: Chapel`)) %>%
    mutate(`Event Location: Hygiene` = as.numeric(`Event Location: Hygiene`)) %>%
    mutate(`Event Location: Hygiene` = if_else(is.na(`Event Location: Hygiene`), 
                                               0, `Event Location: Hygiene`)) %>%
    mutate(`Event Location: Games room` = as.numeric(`Event Location: Games room`)) %>%
    mutate(`Event Location: Games room` = if_else(is.na(`Event Location: Games room`), 
                                                  0, `Event Location: Games room`)) %>%
    mutate(`Event Location: Shipping&Reveiving Area` = as.numeric(`Event Location: Shipping&Reveiving Area`)) %>%
    mutate(`Event Location: Shipping&Reveiving Area` = if_else(is.na(`Event Location: Shipping&Reveiving Area`), 
                                                               0, `Event Location: Shipping&Reveiving Area`)) %>%
    mutate(`Event Location: Common Housing Area` = as.numeric(`Event Location: Common Housing Area`)) %>%
    mutate(`Event Location: Common Housing Area` = if_else(is.na(`Event Location: Common Housing Area`), 
                                                           0, `Event Location: Common Housing Area`)) %>%
    mutate(`Event Location: In Room` = as.numeric(`Event Location: In Room`)) %>%
    mutate(`Event Location: In Room` = if_else(is.na(`Event Location: In Room`), 
                                               0, `Event Location: In Room`)) %>%
    mutate(`Event Location: Perimeter` = as.numeric(`Event Location: Perimeter`)) %>%
    mutate(`Event Location: Perimeter` = if_else(is.na(`Event Location: Perimeter`), 
                                                 0, `Event Location: Perimeter`)) %>%
    mutate(`Event Location:Snack Bar` = as.numeric(`Event Location:Snack Bar`)) %>%
    mutate(`Event Location:Snack Bar` = if_else(is.na(`Event Location:Snack Bar`), 
                                                0, `Event Location:Snack Bar`)) %>%
    mutate(`Naloxone` = as.numeric(`Naloxone`)) %>%
    mutate(`Naloxone` = if_else(is.na(`Naloxone`), 
                                0, `Naloxone`)) %>%
    mutate(`Overdose` = as.numeric(`Overdose`)) %>%
    mutate(`Overdose` = if_else(is.na(`Overdose`), 
                                0, `Overdose`)) %>%
    drop_na()
  
  df$Time = sapply(df$Time, FUN=fix.time)
  
  df = df %>%
    mutate(Time=hms::as.hms(Time))
}

###############################################################################
##########                      VERY IMPORTANT                       ##########
###############################################################################
# This must be an "absolute" file path. This means you can't just place the 
# 919report_data.xlsx file in the same folder and expect it to run properly. 
# The "read_excel" function requires that you use an absolute file path. Below
# is what the correct file path looks like on my mac laptop. If you are using
# a pc, the syntax is slightly different. You need to replace the line below
# with the ABSOLUTE FILE PATH ON YOUR COMPUTER.
setwd("~/Desktop/Spring 2019/STAT 123/Final Project")


# if I was using a windows based computer, my file path might look something like this
# file_path = "C:/Users/stevehof/OneDrive/Documents/tutoring/Stats_123/919report_data.xlsx"

# this line loads the original data into R
data.all = read_excel("919report_data.xlsx", col_types=c("numeric", "text", "date", rep("text",39)))

# this line will "clean" data.all into a usable format and store it in a variable
# called df
df = clean.my.data(data.all)


###############################################################################
##########       Put the code for your assignment below here         ##########
###############################################################################


