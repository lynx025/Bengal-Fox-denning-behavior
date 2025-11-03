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
load("recordData_rounds.RData")
load("camOP_rounds.RData")

## Loop over R1 to R6
for (i in 1:6) {
  # Access the specific record table (R1 to R6)
  current_record_table <- get(paste0("R", i))   # e.g., R1, R1, ..., R6

  # Create a dummy record
  dummy_record <- current_record_table[1, ]  # Take the first row as a template
  dummy_record$Station <- "StationA_dummy"    # Assign dummy station
  dummy_record$Species <- "Bengal Fox"        # Assign species
  dummy_record$DateTimeOriginal <- "2024-03-09 13:47:43"  # Assign arbitrary old date

  # Append the dummy record to the current record table
  current_record_table <- rbind(current_record_table, dummy_record)

  # Assign the updated table back to the environment
  assign(paste0("R", i), current_record_table)
}


## final_check
# class(R1) 
# class(camop_round1)
# unique(R1$Species)
# unique(R1$Station)
# rownames(camop_round1)
## ---------------------------------------------------------------------------##


## ---------------------------------------------------------------------------##
## define the function
myfunc_detectionHistory_minute <- function (recordTable, 
                                            camOp, 
                                            species, 
                                            stationCol = "Station",
                                            speciesCol = "Species", 
                                            recordDateTimeCol = "DateTimeOriginal",
                                            occasionLength = 1,   
                                            day1 = "survey",      
                                            includeEffort = TRUE, 
                                            scaleEffort = FALSE,  
                                            includeNonDetection = FALSE, 
                                            timeZone = "UTC")     
{
        # Check for species
        if (!species %in% recordTable[[speciesCol]]) {
                stop(paste(species, "is not found in species column."))
        }
        
        # Convert time zone
        recordTable[[recordDateTimeCol]] <- as.POSIXct(recordTable[[recordDateTimeCol]], 
                                                       tz = timeZone)
        
        # Extract the dates from camOp (column names of camOp matrix)
        camOpDates <- colnames(camOp)
        
        # Filter recordTable to include only rows with DateTime in camOpDates
        recordTable$recordDate <- as.Date(recordTable[[recordDateTimeCol]])  
        
        thisSpecies <- recordTable[recordTable[[speciesCol]] == species & 
                                           recordTable$recordDate %in% camOpDates, ]
        
        # Create minute-level occasions
        thisSpecies$occasion <- as.numeric(difftime(thisSpecies[[recordDateTimeCol]], 
                                                    as.POSIXct(as.Date(thisSpecies[[recordDateTimeCol]])), 
                                                    units = "mins")) + 1
        
        # Create matrix for detections (minute-level)
        stations <- rownames(camOp)
        nOccasions <- length(camOpDates) * 1440  # 1440 minutes per day
        detectionMatrix <- matrix(0, nrow = length(stations), ncol = nOccasions,
                                  dimnames = list(stations, paste0("o", 1:nOccasions)))
        
        # Loop through records to fill the detection matrix
        for (i in 1:nrow(thisSpecies)) {
                row <- thisSpecies[i, ]
                station <- row[[stationCol]]
                date_index <- which(camOpDates == as.character(row$recordDate))
                minute_of_day <- thisSpecies$occasion[i] + (date_index - 1) * 1440
                detectionMatrix[station, minute_of_day] <- 1
        }
        
        return(detectionMatrix)
}
## ---------------------------------------------------------------------------##


## ---------------------------------------------------------------------------##
## list of record tables and corresponding camera operation matrices
recordTables <- list(R1, R2, R3, R4, R5, R6)
camOps <- list(camop_round1, camop_round2, camop_round3, camop_round4, camop_round5, camop_round6)

# Create an empty list to store detection histories
DetHist_Minute_List <- list()

# Vector of names for saving separately
round_names <- paste0("DetHist_MinuteR", 1:6)
unique(recordTables[[i]]$Species)
# Loop over the six datasets
for (i in 1:6) {
        
        recordTables[[i]]$Species <- as.character(recordTables[[i]]$Species)  # ensure character
        
        if ("Bengal Fox" %in% recordTables[[i]]$Species) {
                DetHist_Minute_List[[i]] <- myfunc_detectionHistory_minute(
                        recordTable = recordTables[[i]],
                        camOp = camOps[[i]],
                        species = "Bengal Fox",   
                        stationCol = "Station",
                        speciesCol = "Species",
                        recordDateTimeCol = "DateTimeOriginal",
                        includeEffort = FALSE,
                        scaleEffort = FALSE
                )
                
                assign(round_names[i], DetHist_Minute_List[[i]])
        } else {
                cat("Bengal Fox not found in recordTables[[", i, "]]\n")
        }
}
## ---------------------------------------------------------------------------##


## ---------------------------------------------------------------------------##
## saving the output
# Loop through each detection history matrix, drop the second row, and save them
for (i in 1:6) {
        
        # Drop the second row for each DetHist_Minute matrix in the list
        DetHist_Minute_List[[i]] <- DetHist_Minute_List[[i]][-2, , drop = FALSE]
        
        # Assign the modified matrix to a variable named DetHist_Minute_R1 to DetHist_Minute_R6
        assign(paste0("DetHist_Minute_R", i), DetHist_Minute_List[[i]])
}

# Save all the modified matrices into an RData file
save(list = paste0("DetHist_Minute_R", 1:6), file = "DetHist_Minute_R1_to_R6_Updated.RData")
## ---------------------------------------------------------------------------##


# ## ---------------------------------------------------------------------------##
# ## CLASSIFYING detectionHistory because there are more False Negatives
# ## 0s where the camera was functional for half a day
# ## due to camera malfunction
# ## create the date range and minutes' serials
# ## full date range
# dates <- seq(as.Date("2024-03-08"), as.Date("2024-05-28"), by = "day")
# 
# ## define dates to be excluded
# excluded_dates <- c(
#         seq(as.Date("2024-03-10"), as.Date("2024-03-17"), by = "day"),
#         seq(as.Date("2024-04-10"), as.Date("2024-04-18"), by = "day"),
#         seq(as.Date("2024-05-03"), as.Date("2024-05-08"), by = "day"))
# 
# ## now correctly identify valid dates
# valid_dates <- dates[!(dates %in% excluded_dates)]
# 
# ## total minutes per day
# minutes_per_day <- 1440
# 
# ## build minute serials based on only valid dates
# minute_start <- (seq_along(valid_dates) - 1) * minutes_per_day + 1
# minute_end <- seq_along(valid_dates) * minutes_per_day
# 
# ## create serials_valid
# serials_valid <- data.frame(
#         Date = valid_dates,
#         Minute_Start = minute_start,
#         Minute_End = minute_end)
# 
# ## now build full table including excluded dates
# adjusted_serials <- data.frame(
#         Date = dates,
#         Minute_Start = NA,
#         Minute_End = NA)
# 
# ## fill serials for valid dates
# adjusted_serials$Minute_Start[dates %in% valid_dates] <- serials_valid$Minute_Start
# adjusted_serials$Minute_End[dates %in% valid_dates] <- serials_valid$Minute_End
# 
# ## remove NAs
# head(adjusted_serials)
# adjusted_serials <- adjusted_serials[!is.na(adjusted_serials$Minute_Start), ]
# ## the total should 84960
# 
# ## now, these days are operational for day time
# ## camera malfunction
# operational4halfday = c("2024-04-01", "2024-04-02", "2024-04-03", "2024-04-04", 
#                         "2024-04-05", "2024-04-06", "2024-04-07", 
#                         "2024-04-08", "2024-04-09")
# 
# # "2024-03-08 00:00:00",  # Start of Round 1
# # "2024-03-18 23:59:59",  # End of Round 1
# # "2024-03-19 00:00:00",  # Start of Round 2
# # "2024-03-31 23:59:59",  # End of Round 2
# # "2024-04-01 00:00:00",  # Start of Round 3
# # "2024-04-19 23:59:59",  # End of Round 3
# # "2024-04-20 00:00:00",  # Start of Round 4
# # "2024-04-30 23:59:59",  # End of Round 4
# # "2024-05-01 00:00:00",  # Start of Round 5
# # "2024-05-15 23:59:59",  # End of Round 5
# # "2024-05-16 00:00:00",  # Start of Round 6
# # "2024-05-28 23:59:59"   # End of Round 6
# 
# ## ---------- ##
# ## start date
# all(DetHist_Minute[, 725] == 0, na.rm = TRUE)
# sum(DetHist_Minute[, 1:1085] == 1, na.rm = TRUE)
# which(DetHist_Minute[, 1:1083] == 1, arr.ind = TRUE)
# ## o1 - o1084 should be removed
# 
# 
# ## end date
# ## ---------- ##
# 
# 
# ## ------------------------------------------------------------------------- ##
# ## FINAL NOTE
# ## o1 - o1084 should be removed
# ## o23041 - o24480 should be removed
# ## o24481 - o24846 should be removed
# ## o25589 - 025920 should be removed
# ## o25921 - o26264 should be removed
# ## o27029 - o27360 should be removed
# ## o27361 - o27720 should be removed
# ## o28468 - o28800 should be removed
# ## o28801 - o29147 should be removed
# ## o29909 - o30240 should be removed
# ## o30241 - o30588 should be removed
# ## o31351 - o31680 should be removed
# ## o31681 - o32021 should be removed
# ## o32725 - o33120 should be removed
# ## o33121 - o33469 should be removed
# ## o34234 - o34560 should be removed
# ## o34561 - o34911 should be removed
# ## o35671 - o36000 should be removed
# ## o84211 - o84960 should be removed
# ## ------------------------------------------------------------------------- ##
# 
# 
# ## ------------------------------------------------------------------------- ##
# dim(DetHist_Minute)
# class(DetHist_Minute)
# 
# 
# ## ------------------------------------------------------------------------- ##
# ## define columns to remove
# cols_to_remove <- c(
#         1:1084,
#         23041:24480,
#         24481:24846,
#         25589:25920,
#         25921:26264,
#         27029:27360,
#         27361:27720,
#         28468:28800,
#         28801:29147,
#         29909:30240,
#         30241:30588,
#         31351:31680,
#         31681:32021,
#         32725:33120,
#         33121:33469,
#         34234:34560,
#         34561:34911,
#         35671:36000,
#         84211:84960)
# 
# 
# ## remove the columns
# DetHist_Minute2 <- DetHist_Minute[, 
#                                   -cols_to_remove, 
#                                   drop = FALSE]
# ## check dimensions
# dim(DetHist_Minute2)
# ## ------------------------------------------------------------------------- ##


## ------------------------------------------------------------------------- ##
write.csv(DetHist_Minute, "DetHist_BFox_minuteR1.csv", row.names = T)
save(DetHist_Minute2, file = "DetHist_BFox_minute.RData")
## ------------------------------------------------------------------------- ##
