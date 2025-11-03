####################  Bengal Fox Denning Behaviour  ############################
################               Bangladesh             ##########################
################               script by              ##########################
################             Muntasir Akash           ##########################


## ---------------------------------------------------------------------------##
## libraries
library("tidyverse")
library("lubridate")
library("hms")
library("dplyr")
library("stringr")
library("ggplot2")
## ---------------------------------------------------------------------------##


rm(list = ls())

## ---------------------------------------------------------------------------##
## load dataset 
Data = read.csv(file = "Fox_MA_edits.csv", header = T, row.names = NULL)
BFox = Data
# ?read.csv
head(BFox)
str(BFox)
colnames(BFox)       
## ---------------------------------------------------------------------------##


## ---------------------------------------------------------------------------##
## REMOVE the following columns for a neat data wrangling and 
## avoid misinterpretation
colnames(BFox)  
## "DateTimeOriginal", "delta.time.secs", "delta.time.mins",
## "delta.time.hours", "delta.time.days", "n_images", "FileName.1", "X",
## "X.1", "X.2"                         
BFox = BFox %>%
        select(-DateTimeOriginal, -delta.time.secs, -delta.time.mins, 
               -delta.time.hours, -delta.time.days, 
               -n_images, -FileName.1, 
               -X, -X.1, -X.2)                 
## ---------------------------------------------------------------------------##  


## ---------------------------------------------------------------------------##
## FIXING Date and Time column 
## CREATE DateTimeOriginal column
str(BFox)
BFox = BFox %>%
        filter(!(Time %in% c("N/A", "NA", ""))) %>% 
        mutate(Date = dmy(Date), 
               Date = format(Date, "%d-%m-%Y"),
               Time = as_hms(Time),
               DateTimeOriginal = as.POSIXct(
                       paste(Date, Time), 
                       format="%d-%m-%Y %H:%M:%S")) 
## check the string
str(BFox)
## ---------------------------------------------------------------------------##


## ---------------------------------------------------------------------------##
## FIXING Species and metadata_Species column 
length(unique(BFox$Species))           # camtrapR extracted species name
length(unique(BFox$metadata_Species))  # name assigned during metadata tagging
unique(BFox$Species)
unique(BFox$metadata_Species)

BFox = BFox %>%
        select(-Species) %>%
        mutate(metadata_Species = case_when(
                metadata_Species %in% 
                                c("Bengal Fox_&_Bengal Monitor", 
                                  "Bengal Monitor_&_Bengal Fox") ~ 
                                  "Bengal Fox and Bengal Monitor",
                metadata_Species %in% 
                                c("Bengal Fox_&_Babbler") ~ 
                                "Bengal Fox and Jungle Babbler",
                        TRUE ~ metadata_Species  # keep other themes unchanged
                ))
unique(BFox$metadata_Species)

## NOTE:
## NA is present in 'metadata_Species'; should not be the case
## let's check how many are there
sum(is.na(BFox$metadata_Species))

## compare these with the adjacent column 'metadata_NumberofFoxes'
## so, there are errors. Let's fix them

BFox$metadata_Species[is.na(BFox$metadata_Species) & 
                      grepl("Fox", BFox$metadata_NumberofFoxes)] = "Bengal Fox"
sum(is.na(BFox$metadata_Species))                      
## ---------------------------------------------------------------------------##


## ---------------------------------------------------------------------------##
## FIXING metadata_NumberofFoxes, metadata_LengthofVisit, metadata_FoxBehavior
## and metadata_NumberofMonitors column 
## NOTE: These columns have words e.g., 1 fox, 2 foxes; 1 second, 2 seconds
## We need to remove these words and keep only the numercial values
## Also, lets shorten the column names
Data %>% 
        select(metadata_NumberofFoxes, metadata_LengthofVisit, 
               metadata_NumberofMonitors, metadata_FoxBehavior) %>%
        head()
unique(Data$metadata_LengthofVisit)

## identify rows with special cases in 'metadata_LengthofVisit'
case1 = Data %>% filter(metadata_LengthofVisit == ">60 seconds")
case2 = Data %>% filter(metadata_LengthofVisit == "16 seconds_&_20 seconds")
case3 = Data %>% filter(metadata_LengthofVisit == "0 second_&_1 second")
case4 = Data %>% filter(metadata_LengthofVisit == "10 seconds_&_1 second")

BFox = BFox %>%
        # filter(!metadata_LengthofVisit %in% c("16 seconds_&_20 seconds", 
        #                                        "0 second_&_1 second", 
        #                                        "10 seconds_&_1 second")) %>%
        # Note: fixed by MSI
        mutate(
                # convert ">60 seconds" to "60"
                metadata_LengthofVisit = ifelse(
                        metadata_LengthofVisit == ">60 seconds", "60 seconds", 
                        metadata_LengthofVisit))%>%
        
        # mutate(metadata_LengthofVisit = 
        #                gsub(">60 seconds", "60 seconds", 
        #                     metadata_LengthofVisit),)%>%
        
        mutate( # remove word and make the variable numeric
                metadata_LengthofVisit = gsub("second(s)?", "", 
                                              metadata_LengthofVisit),
                metadata_LengthofVisit = as.numeric(
                        ifelse(metadata_LengthofVisit %in% c("N/A", "NA"), 
                               NA, metadata_LengthofVisit)),  
                
                # remove word and make the variable numeric
                metadata_NumberofMonitors = gsub("Monitor(s)?", "", 
                                                 metadata_NumberofMonitors),
                metadata_NumberofMonitors = as.numeric(
                        ifelse(metadata_NumberofMonitors %in% c("N/A", "NA"), 
                               NA, metadata_NumberofMonitors)), 
                
                # remove word and make the variable numeric
                metadata_NumberofFoxes = gsub("Fox(es)?", "", 
                                              metadata_NumberofFoxes),
                metadata_NumberofFoxes = 
                        as.numeric(ifelse(metadata_NumberofFoxes %in% c("N/A", "NA"), 
                                          NA, metadata_NumberofFoxes))) %>%
        
               # rename the columns
        rename(FoxN = metadata_NumberofFoxes,
               VisitLength = metadata_LengthofVisit,
               MonitorN = metadata_NumberofMonitors,
               FoxBehavior = metadata_FoxBehavior)

unique(BFox$VisitLength)
unique(BFox$FoxN)

sum(is.na(BFox$FoxN))
BFox$FoxN[is.na(BFox$FoxN)] = 0
sum(is.na(BFox$metadata_Species))
## Note: BFox$metadata_Species NAs are blank

## identify rows with special cases in 'FoxN' and 'VisitLength'
## there are NAs ## why?
case5 = BFox %>% filter(is.na(FoxN))
case6 = BFox %>% filter(is.na(VisitLength))
## ---------------------------------------------------------------------------##


## ---------------------------------------------------------------------------##
## FIXING HierarchicalSubject ## VVIP
## First, separate EndofVisit
unique(BFox$HierarchicalSubject)
BFox = BFox %>%
        mutate(EndofVisit = ifelse(grepl("EndofVisit", HierarchicalSubject), 
                                   "Yes", "No"))
## ---------------------------------------------------------------------------##

## ---------------------------------------------------------------------------##
## FIXING HierarchicalSubject ## VVIP
## extracting first order behavior
## challenge there are double cases in multiple entries
## such as
## FoxBehavior|Feeding, FoxBehavior|Vigilance, LengthofVisit|10 seconds, 
## NumberofFoxes|2 Foxes, Species|Bengal Fox, EndofVisit, 
## FoxBehavior|Feeding|Unidentified
## see two primary behaviors were detected 

## extracting First-Order behaviour
BFox = BFox %>%
        mutate(FOBehav = str_extract_all(HierarchicalSubject, "FoxBehavior\\|[A-Za-z]+(?!\\|)")) %>%
        mutate(FOBehav = sapply(FOBehav, function(x) {
                # Remove "FoxBehavior|" and keep unique values
                unique_behaviors <- unique(str_replace(x, "FoxBehavior\\|", ""))  
                paste(unique_behaviors, collapse = ",") 
        }))
unique(BFox$FOBehav)

## extracting First-Order behaviour ## removing repeated entry
BFox = BFox %>%
        mutate(FOBehav = str_replace_all(FOBehav, "\\bPassin\\b", 
                                         "Passing")) %>%
        mutate(FOBehav = str_replace_all(FOBehav, "\\bFeedin\\b", 
                                         "Feeding")) %>%
        mutate(FOBehav = str_replace_all(FOBehav, "\\bUnidentifiabl\\b", 
                                         "Unidentifiable")) %>%
        mutate(FOBehav = str_replace_all(FOBehav, "\\bUndefinabl\\b", 
                                         "Undefinable")) 

## extracting First-Order behaviour ## removing repeated entry
BFox = BFox %>%
        mutate(FOBehav = sapply(FOBehav, function(x) {
                # split the string by commas
                behaviors <- str_split(x, ",")[[1]]
                
                # remove duplicates
                unique_behaviors <- unique(behaviors)
                
                # recombine the unique behaviors into a single string, 
                # separated by commas
                str_c(unique_behaviors, collapse = ",")
        }))
unique(BFox$FOBehav)

## identify rows with special cases in 'FOBehav' column
## there are empty cells ## why?
case7 = BFox %>% filter(FOBehav == "") ## All Bengal Monitors

## extracting Inter-species, Intra-species, Feeding column ## Logical
BFox = BFox %>%
        mutate(FeedingLogical = ifelse(grepl("Feeding", FOBehav), 
                                   "Yes", "No"))%>%
        mutate(Intra_speciesLogical = ifelse(str_detect(FOBehav, 
                                                        "\\bIntra\\b"), 
                                             "Yes", "No"))%>%
        mutate(Inter_speciesLogical = ifelse(str_detect(FOBehav, 
                                                        "\\bInter\\b"), 
                                             "Yes", "No"))

## remove the word "Inter" and "Intra" ## from FOBehav column
## NOTE: must be done after the above code is run
BFox = BFox %>%
        mutate(FOBehav = gsub("\\s*,?\\b(Intra|Inter)\\b,?\\s*", ",", FOBehav)) %>%
        mutate(FOBehav = gsub("^,\\s*|\\s*,$", "", FOBehav))  
unique(BFox$FOBehav)
sum(is.na(BFox$FOBehav))

# ## extracting species interactions ## exact values
# BFox = BFox %>%
#         mutate(
#                 Intra_species = ifelse(str_detect
#                                       (FOBehav,  "\\bIntra\\b"), 
#                                       FOBehav, NA))%>%
#         mutate(
#                 Inter_species = ifelse(str_detect
#                                       (FOBehav,  "\\bInter\\b"), 
#                                        FOBehav, NA))

## extracting Intra-species interactions ## exact values from HierarchicalSub
BFox = BFox %>%
        mutate(
        Intra_Species = 
                str_extract_all(HierarchicalSubject, 
                "(?<=FoxBehavior\\|Intra-species Interaction\\|)[^,]+")) %>%
        mutate(
        Intra_Species = sapply(Intra_Species, 
                               function(x) ifelse(length(x) > 0, 
                                                  paste(unique(x), 
                                                        collapse = ", "), 
                                                  NA)))
unique(BFox$Intra_Species)
sum(is.na(BFox$Intra_Species))

## extracting Inter-species interactions ## exact values from HierarchicalSub
BFox = BFox %>%
        mutate(Inter_Species = 
                str_extract_all(HierarchicalSubject, 
                "(?<=FoxBehavior\\|Inter-species Interaction\\|)[^,]+")) %>%
        mutate(Inter_Species = sapply(Inter_Species, 
                                      function(x) ifelse(length(x) > 0, 
                                                         paste(unique(x), 
                                                               collapse = ", "),
                                                         NA)))
unique(BFox$Inter_Species)
sum(is.na(BFox$Inter_Species))

## extracting Feeding ## exact values from HierarchicalSub
BFox = BFox %>%
        mutate(Feeding = 
                str_extract_all(HierarchicalSubject, 
                "(?<=FoxBehavior\\|Feeding\\|)[^,]+")) %>%
        mutate(Feeding = sapply(Feeding, 
                                function(x) ifelse(length(x) > 0, 
                                                   paste(unique(x), 
                                                         collapse = ", "), 
                                                   NA)))
unique(BFox$Feeding)
sum(is.na(BFox$Feeding))

## ---------------------------------------------------------------------------##


## ---------------------------------------------------------------------------##
## FINALIZING First-Order Behaviour 
unique(BFox$FOBehav)

## clean up FOBehav by sorting the comma-separated behaviors alphabetically
## for example, "foraging, passing" and "passing, foraging" are taken as two 
## entries
BFox$FOBehav = sapply(strsplit(BFox$FOBehav, ","), function(x) {
        x <- trimws(x)  # remove any leading/trailing whitespace
        if (all(x == "")) return("")  # keep empty strings as is
        paste(sort(x), collapse = ",")
})

length(unique(BFox$FOBehav))
case7 = BFox %>% filter(FOBehav == "")
case8 = BFox %>% filter(FOBehav == "Blank")
case9 = BFox %>% filter(FOBehav == "Detection")
case10 = BFox %>% filter(FOBehav == "Undefinable")

## almost there, let's keep the FOBehav combining with intra and interspecies 
## interaction before we do our final clean-up
## We will see merit in that very soon!
BFox$FOBehav_combined = BFox$FOBehav
unique(BFox$FOBehav)
unique(BFox$Inter_speciesLogical)
unique(BFox$Intra_speciesLogical)

BFox$FOBehav_combined = apply(BFox, 1, function(row) {
        behav = row["FOBehav_combined"]
        interactions <- c()
        
        if (row["Inter_speciesLogical"] == "Yes") {
                interactions <- c(interactions, "Inter-species Interaction")
        }
        if (row["Intra_speciesLogical"] == "Yes") {
                interactions <- c(interactions, "Intra-species Interaction")
        }
        
        if (length(interactions) > 0) {
                if (behav == "" || is.na(behav)) {
                        return(paste(interactions, collapse = ", "))
                } else {
                        return(paste(behav, paste(interactions, collapse = ", "), sep = ", "))
                }
        } else {
                return(behav)
        }
})
unique(BFox$FOBehav_combined)
case11 = BFox %>% filter(FOBehav_combined == "")

## Now, remove some unncessary columns and rename some!
BFox = BFox %>%
        select(-XMPToolkit, -FoxBehavior, -Directory, -Draft.File.Name) %>%   
        rename(Species = metadata_Species)%>% 
        rename(FOBehav_Single = FOBehav)

## Check the dataframe to see if everything is allright
BFox %>% filter(Species == "Oriental Magpie-Robin")
# BFox <- BFox %>%
#         mutate(
#                 Time = ifelse(Species == "Oriental Magpie-Robin", "08:29:06", Time),
#                 Date = ifelse(Species == "Oriental Magpie-Robin", "02-04-2024", Date),
#                 DateTimeOriginal = ifelse(Species == "Oriental Magpie-Robin", "2024-04-02 08:29:06", DateTimeOriginal)
#         )
# BFox %>% filter(Species == "Oriental Magpie-Robin")
str(BFox$Time)
str(BFox$Date)
str(BFox$DateTimeOriginal)

## Saving
save(BFox, file = "BFox_cleaned_Apr20.RData")
write.csv(BFox, file = "BFox_cleaned_Apr20.csv", row.names = FALSE)

