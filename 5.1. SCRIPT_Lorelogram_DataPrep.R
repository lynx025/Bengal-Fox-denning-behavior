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

## read data: 
## read recordTable file as created using function 
## recordTable in the camtrapR package 
## see https://cran.r-project.org/web/packages/camtrapR/vignettes/DataExtraction.html#camera-operation
recordData <- read.csv("/Users/lynx025/Desktop/BengalFox_analyses/5.1 Activity_Lorelogram_Minute Level/BFox_cleaned_Apr20.csv") 
# Fox = Fox %>% filter(Species == "Bengal Fox") 
# Fox = Fox %>% filter(Species %in% c("Bengal Fox", 
#                                     "Bengal Fox and Bengal Monitor", 
#                                     "Bengal Fox and Jungle Babbler")) 
head(recordData)
str(recordData)

recordData = recordData %>% 
        droplevels %>% 
        select(Station, Time, Date, DateTimeOriginal, Species) %>% 
        mutate(DateTimeOriginal = ymd_hms(DateTimeOriginal))

head(recordData)
str(recordData)

unique(recordData$Species)
sum(is.na(recordData$Species))
# Fox = Fox[!is.na(Fox$Species), ]

## reconsidering Double Entries
recordData = recordData %>%
        filter(!is.na(Species)) %>%
        mutate(Species_split = strsplit(Species, " and ")) %>%
        unnest(Species_split) %>%
        mutate(Species_split = str_trim(Species_split))

head(recordData)
str(recordData)

unique(recordData$Species_split)
table(recordData$Species_split)

## make the data.frame shorter
recordData = recordData %>%
        dplyr::select(Station, Species = Species_split, DateTimeOriginal)

## check with minimum and maximum date
str(recordData)
min(recordData$DateTimeOriginal, na.rm = TRUE)
max(recordData$DateTimeOriginal, na.rm = TRUE)

## fix the error
recordData[recordData$Species == "Oriental Magpie-Robin", ]
# recordData$DateTimeOriginal[5064] <- as.POSIXct("2024-04-02 08:29:06", 
#                                                 format = "%Y-%m-%d %H:%M:%S",
#                                                 tz = "UTC")

## save as Rdata and csv file
write.csv(recordData, "recordData.csv")
save(recordData, file = "recordData.Rdata")
## ---------------------------------------------------------------------------##


## ---------------------------------------------------------------------------##
## read camera operation matrix as created using function 
## ?camtrapR::cameraOperation
cameratraptable = 
        read.csv("/Users/lynx025/Desktop/BengalFox_analyses/5.1 Activity_Lorelogram_Minute Level/CamOp_BFox.csv") 

## making station name uniform
# unique(recordData$Station)
# unique(cameratraptable$Station)
# cameratraptable$Station <- gsub(" ", "", cameratraptable$Station)

## making date and time in correct format
cameratraptable <- cameratraptable %>%
        mutate(Setup_Date = format(as.Date(Setup_Date, format="%d-%m-%Y"), "%d/%m/%Y"))%>%
        mutate(Retrieval_Date = format(as.Date(Retrieval_Date, format="%d-%m-%Y"), "%d/%m/%Y"))%>%
        mutate(Problem1_from = format(as.Date(Problem1_from, format="%d-%m-%Y"), "%d/%m/%Y"))%>%
        mutate(Problem1_to = format(as.Date(Problem1_to, format="%d-%m-%Y"), "%d/%m/%Y"))%>%
        mutate(Problem2_from = format(as.Date(Problem2_from, format="%d-%m-%Y"), "%d/%m/%Y"))%>%
        mutate(Problem2_to = format(as.Date(Problem2_to, format="%d-%m-%Y"), "%d/%m/%Y"))%>%
        mutate(Problem3_from = format(as.Date(Problem3_from, format="%d-%m-%Y"), "%d/%m/%Y"))%>%
        mutate(Problem3_to = format(as.Date(Problem3_to, format="%d-%m-%Y"), "%d/%m/%Y"))

## add a dummy station row
## reason: single camera trap station
## the matrix can not be rendered in that scenario
## not necessary, if there are multiple stations
cameratraptable_test <- rbind(cameratraptable,
                              cameratraptable[1, ])
head(cameratraptable_test)

## rename the duplicated row
cameratraptable_test$Station[2] = paste0(cameratraptable_test$Station[1], "_dummy")
head(cameratraptable_test)

## recording camop_problem using cameraOperation
camop_problem <- camtrapR::cameraOperation(CTtable = cameratraptable_test,
                                           stationCol   = "Station",
                                           setupCol     = "Setup_Date",
                                           retrievalCol = "Retrieval_Date",
                                           writecsv     = FALSE,
                                           hasProblems  = TRUE,
                                           dateFormat   = "%d/%m/%Y"
)
# data(camtraps)
# camop_problem <- cameraOperation(CTtable      = camtraps,
#                                  stationCol   = "Station",
#                                  setupCol     = "Setup_date",
#                                  retrievalCol = "Retrieval_date",
#                                  writecsv     = FALSE,
#                                  hasProblems  = TRUE,
#                                  dateFormat   = "dmy"
# )
# # with problems/malfunction / dateFormat in strptime format
# camop_problem_lubridate <- cameraOperation(CTtable      = camtraps,
#                                            stationCol   = "Station",
#                                            setupCol     = "Setup_date",
#                                            retrievalCol = "Retrieval_date",
#                                            writecsv     = FALSE,
#                                            hasProblems  = TRUE,
#                                            dateFormat   = "%d/%m/%Y"
# )
?cameraOperation

## replacing 1 with 0.5 for days that were operational for 
## only daytime
head(camop_problem)
date_range <- which(colnames(camop_problem) %in% c("2024-04-02", 
                                                   "2024-04-03", 
                                                   "2024-04-04", 
                                                   "2024-04-05", 
                                                   "2024-04-06", 
                                                   "2024-04-07", 
                                                   "2024-04-08", 
                                                   "2024-04-09"))
camop_problem["StationA", date_range] <- 0.5

## extracting only active camera trap days
columns_with_1 <- which(camop_problem["StationA", ] != 0)
camop_problem2 <- camop_problem[, columns_with_1]


## save as Rdata and csv file
save(camop_problem, camop_problem2, file = "camOP.Rdata")
write.csv(camop_problem, "camOP.csv")
write.csv(camop_problem2, "camOP2.csv")
## ---------------------------------------------------------------------------##

