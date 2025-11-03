## ---------------------------------------------------------------------------##
## PREPARE recordData and camOP for myfunc_detectionHistory_minute

## dependencies
library(camtrapR)    # updated February 24, 2024
library(tidyverse)   # data wrangling
library(lubridate)   # data wrangling
library(dplyr)       # data wrangling
library(lorelogram)  # 
library (ggplot2)    # plotting
library(gridExtra)   # plotting
## ---------------------------------------------------------------------------##

rm(list = ls())

##----------------------------------------------------------------------------##
load("recordData.Rdata")
load("camOP.Rdata")
##----------------------------------------------------------------------------##


##----------------------------------------------------------------------------##
## occasion days when camera was turned off need to be filtered out
## 2024-03-09 20:20:00 - 2024-03-18 17:00:00
## 2024-04-02 - 2024-04-09 all day time records (05:00:00 - 19:00:00)
## 2024-04-09 (19:00:00) - 2024-04-19 (10:00:00)
## 2024-05-03 (00:00:00) - 2024-05-09 (15:00:00)

## checks
## 08/03/2024 DEPLOYMENT
## 08/03/2024 00:00:00 - 18/03/2024 16:00:00 - Round 01
## 18/03/2024 16:00:01 - 31/03/2024 12:00:00 - Round 02
## 31/03/2024 12:00:01 - 19/04/2024 12:00:00 - Round 03
## 19/04/2024 12:00:01 - 30/04/2024 11:00:00 - Round 04
## 30/04/2024 11:00:01 - 15/05/2024 12:00:00 - Round 05
## 15/05/2024 12:00:01 - 28/05/2024 14:00:00 - Round 06 
## 28/05/2024 RETRIEVAL 
##----------------------------------------------------------------------------##


##----------------------------------------------------------------------------##
## DIVIDING camOP according to checks
head(camop_problem2)
class(camop_problem2)

# First, get the column names as dates
dates <- as.Date(colnames(camop_problem2))

# Define the date ranges for each round
round1 <- dates >= as.Date("2024-03-08") & dates <= as.Date("2024-03-18")
round2 <- dates >= as.Date("2024-03-19") & dates <= as.Date("2024-03-31")
round3 <- dates >= as.Date("2024-04-01") & dates <= as.Date("2024-04-19")
round4 <- dates >= as.Date("2024-04-20") & dates <= as.Date("2024-04-30")
round5 <- dates >= as.Date("2024-05-01") & dates <= as.Date("2024-05-15")
round6 <- dates >= as.Date("2024-05-16") & dates <= as.Date("2024-05-28")

# Now split the matrix
camop_round1 <- camop_problem2[, round1, drop = F]; class(camop_round1)
camop_round2 <- camop_problem2[, round2]; class(camop_round2)
camop_round3 <- camop_problem2[, round3]
camop_round4 <- camop_problem2[, round4]
camop_round5 <- camop_problem2[, round5]
camop_round6 <- camop_problem2[, round6]

# Check the dimensions
dim(camop_round1)
dim(camop_round2)
dim(camop_round3)
dim(camop_round4)
dim(camop_round5)
dim(camop_round6)
##----------------------------------------------------------------------------##


##----------------------------------------------------------------------------##
## DIVIDING recordData according to checks
head(recordData)
# Make sure DateTimeOriginal is POSIXct
#recordData$DateTimeOriginal <- as.POSIXct(recordData$DateTimeOriginal)

# Define round boundaries
boundaries <- as.POSIXct(c(
        "2024-03-08 00:00:00",  # Start of Round 1
        "2024-03-18 23:59:59",  # End of Round 1
        "2024-03-18 00:00:00",  # Start of Round 2
        "2024-03-31 23:59:59",  # End of Round 2
        "2024-03-31 00:00:00",  # Start of Round 3
        "2024-04-19 23:59:59",  # End of Round 3
        "2024-04-19 00:00:00",  # Start of Round 4
        "2024-04-30 23:59:59",  # End of Round 4
        "2024-04-30 00:00:00",  # Start of Round 5
        "2024-05-15 23:59:59",  # End of Round 5
        "2024-05-15 00:00:00",  # Start of Round 6
        "2024-05-28 23:59:59"   # End of Round 6
))

# Round DateTimeOriginal to the nearest second to ensure consistency
recordData$DateTimeOriginal <- as.POSIXct(format(recordData$DateTimeOriginal, "%Y-%m-%d %H:%M:%S",
                                                 tz = "UTC"))


# Add Round information
recordData <- recordData %>%
        mutate(Round = case_when(
                DateTimeOriginal >= boundaries[1]  & DateTimeOriginal <= boundaries[2]  ~ "Round 1",
                DateTimeOriginal >= boundaries[3]  & DateTimeOriginal <= boundaries[4]  ~ "Round 2",
                DateTimeOriginal >= boundaries[5]  & DateTimeOriginal <= boundaries[6]  ~ "Round 3",
                DateTimeOriginal >= boundaries[7]  & DateTimeOriginal <= boundaries[8]  ~ "Round 4",
                DateTimeOriginal >= boundaries[9]  & DateTimeOriginal <= boundaries[10] ~ "Round 5",
                DateTimeOriginal >= boundaries[11] & DateTimeOriginal <= boundaries[12] ~ "Round 6",
                TRUE ~ NA_character_
        ))

# Split into 6 separate datasets
R1 <- recordData %>% filter(Round == "Round 1")
R2 <- recordData %>% filter(Round == "Round 2")
R3 <- recordData %>% filter(Round == "Round 3")
R4 <- recordData %>% filter(Round == "Round 4")
R5 <- recordData %>% filter(Round == "Round 5")
R6 <- recordData %>% filter(Round == "Round 6")

# (Optional) see how many records in each
sapply(list(R1, R2, R3, R4, R5, R6), nrow)


##----------------------------------------------------------------------------##
# Save recordData rounds into one RData file
save(R1, R2, R3, R4, R5, R6, file = "recordData_rounds.RData")
# Save camOP rounds into another RData file
save(camop_round1, camop_round2, camop_round3, camop_round4, camop_round5, 
     camop_round6,file = "camOP_rounds.RData")
##----------------------------------------------------------------------------##


