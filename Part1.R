library(readr)
library(lubridate)
library(tidyverse)
library(tidyr)
library(viridis)
library(scales)

setwd("~/Desktop/Spring 2019/STAT 123/Final Project")

ds <- read_csv("919report_data.csv")

# Cleaning Data

# Functions
fix.time = function(x) {
  return(str_split(x, " ", simplify=T)[1,2])
}

clean.up = function(x) {
  if (is.na(x)) {
    x <- FALSE
  } else if (x == 0) {
    x <- FALSE
  } else {
    x <- TRUE
  }
}

# Fix and seperate Time
ds$Time = sapply(ds$Time, FUN = fix.time)
ds = ds %>%
  mutate(Time=hms::as.hms(Time))

ds$Time <- as.POSIXct(strptime(ds$Time, format="%H:%M:%S"))

# Time of Day Column
ds = ds %>%
  mutate(Time_Of_Day = ifelse(Time < 12*60*60, "Morning", ifelse(Time < 17*60*60, "Afternoon", "Evening")))

# Reorder, rename Columns
ds <- ds[c(1,2,3,43,4:42)]
colnames(ds)[17] <- "Attended By: Fire"
colnames(ds)[42] <- "Event Location: Snack Bar"

# Tranforms with semicolon
for (i in c(12:17, 19:26, 30:42)) {
  ds[i] <- apply(ds[i], MARGIN = 1, FUN = clean.up)
  ds[i] <- transform(ifelse(ds[i] == TRUE, paste(str_split(colnames(ds)[i], ": ")[[1]][2]), NA))
}

# Transforms with no semicolon
for (i in c(11, 27:29)) {
  ds[i] <- apply(ds[i], MARGIN = 1, FUN = clean.up)
  ds[i] <- transform(ifelse(ds[i] == TRUE, paste(colnames(ds)[i]), NA))
}

# Unite transformed Columns
ds <- unite(ds, col = "Incident_Type", matches("Incident"), sep=",")
ds$Incident_Type <- gsub("NA[,]?","", ds$Incident_Type)
ds$Incident_Type <- gsub("^$", NA, ds$Incident_Type)

ds <- unite(ds, col = "Attended_By", matches("Attended"), sep=",")
ds$Attended_By <- gsub("NA[,]?","", ds$Attended_By)
ds$Attended_By <- gsub("^$", NA, ds$Attended_By)

ds <- unite(ds, col = "Init_Call", matches("Call"), sep=",")
ds$Init_Call <- gsub("NA[,]?","", ds$Init_Call)
ds$Init_Call <- gsub("^$", NA, ds$Init_Call)

ds <- unite(ds, col = "Location", matches("Event"), sep=",")
ds$Location <- gsub("NA[,]?","", ds$Location)
ds$Location <- gsub("^$", NA, ds$Location)

ds = ds %>%
  separate(Incident_Type, "Incident Type", sep = ",", remove = TRUE) %>%
  separate(Attended_By, "Attended By", sep = ",", remove = TRUE) %>%
  separate(Init_Call, "Initial Call", sep = ",", remove = TRUE) %>%
  separate(Location, "Location", sep = ",", remove = TRUE)

ds$Incident_Type <- factor(ds$Incident_Type)
ds$Attended_By <- factor(ds$Attended_By)
ds$Init_Call <- factor(ds$Init_Call)
ds$Location <- factor(ds$Location)
ds$Location <- fct_infreq(ds$Location)

# summary stastics i think
summary(ds)

# number of overdose calls
overdose_calls <- sum(ds$Overdose == 1, na.rm = TRUE)
overdose_calls

# number of call to first responders
calls <- sum(ds$`Emergency Services Required` == 1, na.rm = TRUE)
calls

# Q: Summary incidents by time of day.  Are there more incidents at the end of the day?

ggplot(data.frame(ds$Time_Of_Day), aes(x=ds$Time_Of_Day)) +
  geom_bar()

ggplot(data.frame(ds$Time), aes(x=ds$Time)) +
  geom_histogram()

# A: There are the most incidents in the morning (before noon)

# Is there a correlation between the time of incident and the site of incident?

ggplot(ds, aes(x=Time, y=Location)) + 
  geom_point()

ggplot(ds, aes(x = Time,
               fill = Location,
               color = factor(Location))) + 
  geom_freqpoly() +
  scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%I:%M %p")) +
  scale_color_viridis(discrete = TRUE, "", direction = -1)


                     