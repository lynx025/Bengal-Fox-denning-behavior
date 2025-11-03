## ---------------------------------------------------------------------------##
## APPLY recordData and camOP to myfunc_detectionHistory_minute
## PRODUCE the minute level detection history

## dependencies
library(camtrapR)    # updated February 24, 2024
library(tidyverse)   # data wrangling
library(lubridate)   # data wrangling
library(dplyr)       # data wrangling
library(lorelogram)  # lorelogram production
library (ggplot2)    # plotting
library(gridExtra)   # plotting
## ---------------------------------------------------------------------------##


rm(list = ls())


## load data
load("recordData.Rdata")
load("camop.Rdata")

# camop_problem2 <- camop_problem2[-2, , drop = FALSE]
str(recordData$DateTimeOriginal)

## check properties
class(recordData)    # should be a dataframe
# is.data.frame(recordData)
recordData = as.data.frame(recordData)
class(recordData)

class(camop_problem2) # should be a matrix # 59 days
# camop_problem2 = as.matrix(camop_problem2)
# is.matrix(camop_problem2)

## Note: for CSV pathway, follow the codes below
## check whether row names are in bold. if so, okay
# camop_problem2 <- read.csv("camOP.csv", header=TRUE, check.names = FALSE)
# row.names(camop_problem2) # if not specified, the return will be [1] "1" "2"
# row.names(camop_problem2) <- camop_problem2[,1]
# camop_problem2 <- camop_problem2[,-1] 
row.names(camop_problem2) # now, the return will be [1] "StationA" "StationA_dummy"

# camop_problem2[1,] = 1 # remove 0.5
# camop_problem2[2,] = 1 # remove 0.5

unique(recordData$Station)
unique(recordData$Species)
str(recordData$Date)

## because, there is a dummy station in camop_problem2
dummy_record <- recordData[1, ]
dummy_record$Station <- "StationA_dummy"
dummy_record$Species <- "Bengal Fox"
dummy_record$DateTimeOriginal <- "2024-04-30 13:47:43"  # arbitrary old date
recordData <- rbind(recordData, dummy_record)

## final_check
class(recordData) 
class(camop_problem2)
unique(recordData$Species)
unique(recordData$Station)
rownames(camop_problem2)
head(camop_problem2)
## ---------------------------------------------------------------------------##


## ---------------------------------------------------------------------------##
## define the function
myfunc_detectionHistory_minute <- function (recordTable, 
                                            camOp, 
                                            species, 
                                            stationCol = "Station",
                                            speciesCol = "Species", 
                                            recordDateTimeCol = "DateTimeOriginal",
                                            occasionLength = 1,   # minute-level
                                            day1 = "survey",      # see ?detectionHistory
                                            includeEffort = TRUE, # see ?detectionHistory
                                            scaleEffort = FALSE,  # see ?detectionHistory
                                            includeNonDetection = F, 
                                            timeZone = "UTC")     # specify time zone
{
        
        # Check for species
        if (!species %in% recordTable[, speciesCol]) {
                stop(paste(species, "is not found in species column."))
        }
        
        # Convert time zone
        recordTable[, recordDateTimeCol] <- as.POSIXct(recordTable[, recordDateTimeCol], 
                                                       tz = timeZone)
        
        # Extract the dates from camOp (column names of camOp matrix)
        camOpDates <- colnames(camOp)
        
        # Filter recordTable to include only rows with DateTime in camOpDates
        recordTable$recordDate <- as.Date(recordTable[, recordDateTimeCol])  # Create date column in recordTable
        thisSpecies <- recordTable[recordTable[, speciesCol] == species & 
                                           recordTable$recordDate %in% camOpDates, ]
        
        # Create minute-level occasions (for each minute of the day)
        thisSpecies$occasion <- as.numeric(difftime(thisSpecies[, recordDateTimeCol], 
                                                    as.POSIXct(as.Date(thisSpecies[, recordDateTimeCol])), 
                                                    units = "mins")) + 1
        
        # Create matrix for detections (minute-level)
        stations <- rownames(camOp)
        # Total number of minutes (occasions) based on the range of dates in camOp
        nOccasions <- length(camOpDates) * 1440  # 1440 minutes per day
        detectionMatrix <- matrix(0, nrow = length(stations), ncol = nOccasions,
                                  dimnames = list(stations, paste0("o", 1:nOccasions)))
        
        # Loop through records to fill the detection matrix
        for (i in 1:nrow(thisSpecies)) {
                row <- thisSpecies[i, ]
                station <- row[[stationCol]]
                # Find the minute of the day
                date_index <- which(camOpDates == as.character(row$recordDate))
                minute_of_day <- thisSpecies$occasion[i] + (date_index - 1) * 1440
                detectionMatrix[station, minute_of_day] <- 1
        }
        
        return(detectionMatrix)
}
## ---------------------------------------------------------------------------##


## ---------------------------------------------------------------------------##
## run the function
DetHist_Minute <- myfunc_detectionHistory_minute(recordTable =  recordData,
                                                 species = "Bengal Fox", #change species name
                                                 camOp = camop_problem2,
                                                 stationCol =  "Station",
                                                 speciesCol = "Species",
                                                 recordDateTimeCol = "DateTimeOriginal", 
                                                 # recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                                 occasionLength = 15, 
                                                 includeEffort = FALSE,
                                                 scaleEffort = FALSE, 
                                                 day1 = "survey", 
                                                 timeZone="Asia/Dhaka"
                                                 )
?camtrapR::detectionHistory
## ---------------------------------------------------------------------------##


## ---------------------------------------------------------------------------##
## saving the output
class(DetHist_Minute)
row.names(DetHist_Minute)
DetHist_Minute <- DetHist_Minute[-2, , drop = FALSE]
DetHist_Minute[, 950:1080]
sum(DetHist_Minute == 1)
## ---------------------------------------------------------------------------##


## ---------------------------------------------------------------------------##
## CLASSIFYING detectionHistory because there are more False Negatives
## 0s where the camera was functional for half a day
## due to camera malfunction
## create the date range and minutes' serials
## full date range
dates <- seq(as.Date("2024-03-08"), as.Date("2024-05-28"), by = "day")

## define dates to be excluded
excluded_dates <- c(
        seq(as.Date("2024-03-10"), as.Date("2024-03-17"), by = "day"),
        seq(as.Date("2024-04-10"), as.Date("2024-04-18"), by = "day"),
        seq(as.Date("2024-05-03"), as.Date("2024-05-08"), by = "day"))

## now correctly identify valid dates
valid_dates <- dates[!(dates %in% excluded_dates)]

## total minutes per day
minutes_per_day <- 1440

## build minute serials based on only valid dates
minute_start <- (seq_along(valid_dates) - 1) * minutes_per_day + 1
minute_end <- seq_along(valid_dates) * minutes_per_day

## create serials_valid
serials_valid <- data.frame(
        Date = valid_dates,
        Minute_Start = minute_start,
        Minute_End = minute_end)

## now build full table including excluded dates
adjusted_serials <- data.frame(
        Date = dates,
        Minute_Start = NA,
        Minute_End = NA)

## fill serials for valid dates
adjusted_serials$Minute_Start[dates %in% valid_dates] <- serials_valid$Minute_Start
adjusted_serials$Minute_End[dates %in% valid_dates] <- serials_valid$Minute_End

## remove NAs
head(adjusted_serials)
adjusted_serials <- adjusted_serials[!is.na(adjusted_serials$Minute_Start), ]
## the total should 84960

## now, these days are operational for day time
## camera malfunction
operational4halfday = c("2024-04-01", "2024-04-02", "2024-04-03", "2024-04-04", 
                        "2024-04-05", "2024-04-06", "2024-04-07", 
                        "2024-04-08", "2024-04-09")

## ---------- ##
## April 01
DetHist_Minute[, 23041:24480]
all(DetHist_Minute[, (23041+1110):24480] == 0, na.rm = TRUE)
## o23041 - o24480 should be removed


## ---------- ##
## April 02
DetHist_Minute[, 24481:25920]
all(DetHist_Minute[, 24481:(24481+365)] == 0, na.rm = TRUE)
## o24481 - o24846 should be removed

all(DetHist_Minute[, 25589:25920] == 0, na.rm = TRUE)
sum(DetHist_Minute[, 25589:25920] == 1, na.rm = TRUE)
DetHist_Minute[, 25589]
## o25589 - o25920 should be removed

## ---------- ##
## April 03
DetHist_Minute[, 25921:27360]
all(DetHist_Minute[, 25921:(25921+300)] == 0, na.rm = TRUE)
sum(DetHist_Minute[, 25921:(25921+300)] == 1, na.rm = TRUE)
sum(DetHist_Minute[, 25921:26264] == 1, na.rm = TRUE)
## o25921 - o26264 should be removed

DetHist_Minute[, (25921+1100):27360]
all(DetHist_Minute[, (25921+1108):27360] == 0, na.rm = TRUE)
## o27029 - o27360 should be removed
## ---------- ##

## ---------- ##
## April 04
DetHist_Minute[, 27361:28800]
all(DetHist_Minute[, 27361:(27361+359)] == 0, na.rm = TRUE)
## o27361 - o27720 should be removed

all(DetHist_Minute[, (27361+1107):28800] == 0, na.rm = TRUE)
## o28468 - o28800 should be removed
## ---------- ##

## ---------- ##
## April 05
DetHist_Minute[, 28801:30240]
all(DetHist_Minute[, 28801:(28801+346)] == 0, na.rm = TRUE)
## o28801 - o29147 should be removed

all(DetHist_Minute[, (28801+1108):30240] == 0, na.rm = TRUE)
## o29909 - o30240 should be removed
## ---------- ##

## ---------- ##
## April 06
DetHist_Minute[, 30241:31680]
all(DetHist_Minute[, 30241:(30241+347)] == 0, na.rm = TRUE)
## o30241 - o30588 should be removed

all(DetHist_Minute[, (30241+1110):31680] == 0, na.rm = TRUE)
## o31351 - o31680 should be removed

## ---------- ##
## April 07
DetHist_Minute[, 31681:33120]
all(DetHist_Minute[, 31681:(31681+340)] == 0, na.rm = TRUE)
## o31681 - o32021 should be removed

all(DetHist_Minute[, (31681+1044):33120] == 0, na.rm = TRUE)
## o32725 - o33120 should be removed
## ---------- ##

## ---------- ##
## April 08
DetHist_Minute[, 33121:34560]
all(DetHist_Minute[, 33121:(33121+348)] == 0, na.rm = TRUE)
## o33121 - o33469 should be removed

all(DetHist_Minute[, (33121+1113):34560] == 0, na.rm = TRUE)
## o34234 - o34560 should be removed
## ---------- ##

## ---------- ##
## April 09
DetHist_Minute[, 34561:36000]
all(DetHist_Minute[, 34561:(34561+350)] == 0, na.rm = TRUE)
## o34561 - o34911 should be removed

all(DetHist_Minute[, (34561+1110):36000] == 0, na.rm = TRUE)
## o35671 - o36000 should be removed
## ---------- ##


## ---------- ##
## start day
all(DetHist_Minute[, 1:1084] == 0, na.rm = TRUE)
## o1 - o1084 should be removed

## end day
all(DetHist_Minute[, (83521+690):84960] == 0, na.rm = TRUE)
## o84211 - o84960 should be removed
## ---------- ##


## ------------------------------------------------------------------------- ##
## FINAL NOTE
## o1 - o1084 should be removed
## o23041 - o24480 should be removed
## o24481 - o24846 should be removed
## o25589 - 025920 should be removed
## o25921 - o26264 should be removed
## o27029 - o27360 should be removed
## o27361 - o27720 should be removed
## o28468 - o28800 should be removed
## o28801 - o29147 should be removed
## o29909 - o30240 should be removed
## o30241 - o30588 should be removed
## o31351 - o31680 should be removed
## o31681 - o32021 should be removed
## o32725 - o33120 should be removed
## o33121 - o33469 should be removed
## o34234 - o34560 should be removed
## o34561 - o34911 should be removed
## o35671 - o36000 should be removed
## o84211 - o84960 should be removed
## ------------------------------------------------------------------------- ##


## ------------------------------------------------------------------------- ##
dim(DetHist_Minute)
class(DetHist_Minute)


## ------------------------------------------------------------------------- ##
## define columns to remove
cols_to_remove <- c(
        1:1084,
        23041:24480,
        24481:24846,
        25589:25920,
        25921:26264,
        27029:27360,
        27361:27720,
        28468:28800,
        28801:29147,
        29909:30240,
        30241:30588,
        31351:31680,
        31681:32021,
        32725:33120,
        33121:33469,
        34234:34560,
        34561:34911,
        35671:36000,
        84211:84960)


## remove the columns
DetHist_Minute2 <- DetHist_Minute[, 
                                  -cols_to_remove, 
                                  drop = FALSE]
## check dimensions
dim(DetHist_Minute2)
## ------------------------------------------------------------------------- ##


## ------------------------------------------------------------------------- ##
write.csv(DetHist_Minute2, "DetHist_BFox_minute.csv", row.names = T)
save(DetHist_Minute2, file = "DetHist_BFox_minute.RData")
## ------------------------------------------------------------------------- ##

