####################  Bengal Fox Denning Behaviour  ############################
################               Bangladesh             ##########################
################               script by              ##########################
################             Muntasir Akash           ##########################
############   estimating KDE-based activity density     #######################
############             with package activity           ####################### 


##_______________________________________________________
##########################################################
## getwd()  # check directory location 
## ?setwd() # set it in a desired location

## to switch between input and output directory; see
## https://amywhiteheadresearch.wordpress.com/2014/11/12/copying-files-with-r/ 

## E.G. 
## set the input directory
## setwd("E:/Trap data_station directories") # a pre-created folder 
## set the output directory
## wherethecsvwillgo = 
## ("E:/5.2 MY R DATABASE/1. rProjects/1. Cameraoperationmatrix") # see below



## ---------------------------------------------------------------------------##
## dependencies
library(camtrapR)  # updated February 24, 2024
library(overlap)
library(tidyverse) # data wrangling
library(lubridate) # data wrangling
library(dplyr)     # data wrangling
library(activity)  # activity pattern analyses
library(ggplot2)   # plotting
library(gridExtra) # plotting
library(ggpubr)    # plotting
library(patchwork) # plotting
library(cowplot)   # plotting
library(grid)      # plotting
library(magick)    # plotting
## ---------------------------------------------------------------------------##




rm(list = ls())
my_custom_theme = theme(
        text = element_text(family = "Times New Roman"),
        axis.line = element_line(colour = 'black', linetype = 'solid'),
        axis.ticks = element_line(colour = 'black', linetype = 'solid'),
        axis.text =  element_text(size = 12),
        axis.title = element_text(size = 14, colour = 'gray50', face = "bold"),
        axis.title.x = element_text(vjust = -0.01),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        # panel.grid.major.x = element_line(colour = 'lightgrey', linetype = 'dashed', linewidth = 0.5),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.direction = "horizontal")



## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##
## load dataset 
Fox = read.csv("/Users/lynx025/Desktop/BengalFox_analyses/5.3 Activity Pattern Analyses_KDE/BFox_cleaned_Apr20.csv") 
# Data = read.csv(file = "Fox_MA_edits.csv", header = T, row.names = NULL)
## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##



## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##
## STEP ONE: DATE-TIME AND DIEL CYCLE CALCULATIONS
## 1.1 Basic data checking and tweaking
## 1.2 Considering co-detections
## 1.3 Classifying diel cycle  [test]
## 1.4 Classifying diel cycle  [within the original dataset]
## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##


#### 1.1__________________________________________________________________#######
## Basic checks and tweaks
head(Fox)
str(Fox)
unique(Fox$Species)

# select only relevant columns
Fox = Fox %>% 
        droplevels %>% 
        select(Station, Time, Date, DateTimeOriginal, Species) %>% 
        mutate(DateTimeOriginal = ymd_hms(DateTimeOriginal))

# Fox =  Fox %>%
#         select(Station, Time, Date, DateTimeOriginal, Species) %>%
#         mutate(
#                 DateTimeOriginal = lubridate::ymd_hms(DateTimeOriginal, tz = "Asia/Dhaka"),
#                 Time = hms::as_hms(Time),
#                 Date_parsed = as.Date(Date, format = "%d-%m-%Y"))

head(Fox)
unique(Fox$Species)
sum(is.na(Fox$Species))
# Fox = Fox[!is.na(Fox$Species), ]
#######__________________________________________________________________########


#### 1.2__________________________________________________________________#######
## reconsidering Double-Species Entries
Fox = Fox %>%
        filter(!is.na(Species)) %>%
        mutate(Species_split = strsplit(Species, " and ")) %>%
        unnest(Species_split) %>%
        mutate(Species_split = str_trim(Species_split))

head(Fox)
unique(Fox$Species_split)
table(Fox$Species_split)
#######__________________________________________________________________########
## STEP ONE ends



## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##
## STEP TWO: SUMMARISE DETECTIONS
## 2.1 Meeting KDE requirement of independence
## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##



#### 2.1__________________________________________________________________#######
## Meeting KDE requirement of independence

# NOTE: data-thinning set the threshold that define independence independence
# between subsequent records ## an KDE requirement
# aggregate data to reduce short-term temporal dependence

independence_interval = 0 # minutes # can be any time-interval

# now, the aggregation
dat_event = Fox %>% 
        mutate(DateTimeOriginal = ymd_hms(DateTimeOriginal)) %>% 
        group_by(Species_split, Station) %>%      # group by species and location
        arrange(DateTimeOriginal, .by_group = TRUE) %>% # arrange record temporally within each group
        mutate(timediff = as.numeric(difftime(DateTimeOriginal, lag(DateTimeOriginal), units="mins")), # calculate time difference (in minutes) between consecutive events
               timediff = replace_na(timediff, 0),
               grp = ifelse(timediff >= independence_interval, 1, 0), # create events
               grp_ind = cumsum(grp) + 1) 
dat_event = dat_event %>%
        group_by(Species_split, Station, grp_ind) %>%
        slice(1)
head(dat_event)
table(dat_event$Species_split)
#######__________________________________________________________________########


#### 2.2__________________________________________________________________#######
## Convert time values to radians 

# requirement of the packages activity, circular, overlap
# specify the format of the column Time
dat_event$Time = hms::as_hms(dat_event$Time)
str(dat_event$Time)
dat_event$Time_Rad = (hour(dat_event$Time)*60 + 
                              minute(dat_event$Time))/(60*24)*2*pi
head(dat_event$Time_Rad)
#######__________________________________________________________________########
## STEP TWO ends



## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##
## STEP THREE: ACTIVITY PATTERN ANALYSES
## 3.1 Running activity::fitact
## 3.2 Plotting
## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##


#### 3.1__________________________________________________________________#######
## Running activity::fitact

# split by Species_split from the processed dataset
dat_ls = dat_event %>% group_by(Species_split) %>% group_split()

# preallocate KDE list
KDE_ls = vector("list", length = length(dat_ls))

# we need to loop through species and estimate KDE
for(i in 1:length(dat_ls)) {
        # run fitact on the Time_Rad column and get the KDE result
        fit_result = activity::fitact(dat_ls[[i]]$Time_Rad, reps = 999, 
                                       sample = "model", show = FALSE)
        
        # create a data frame with the KDE result
        KDE_result = as.data.frame(fit_result@pdf[, 1:3])
        
        # ensure the number of rows is consistent with the original data
        KDE_result$Species = rep(unique(dat_ls[[i]]$Species_split), 
                                  nrow(KDE_result))
        
        # replicate the Time_Rad values for the KDE results
        # Note: here, you need to extract the unique Time_Rad values from the dataset.
        # if there are multiple Time_Rad points per species, use representative values.
        KDE_result$Time_Rad_predicted = fit_result@pdf[, 1]  # predicted time in radians
        KDE_result$Time_predicted = hms::as_hms((KDE_result$Time_Rad_predicted / (2 * pi)) * 86400)  # Predicted time in actual clock time
        
        # replicate the Original Time values from the dataset # just in case to compare
        KDE_result$Original_Time = rep(dat_ls[[i]]$Time, length.out = nrow(KDE_result))  
        KDE_result$Original_Time_Rad = rep(dat_ls[[i]]$Time_Rad, length.out = nrow(KDE_result))
        KDE_result$Original_species = rep(dat_ls[[i]]$Species, length.out = nrow(KDE_result))
        # Add this data to the list
        KDE_ls[[i]] = KDE_result
}

nrow(KDE_ls[[1]])
length(dat_ls)

KDE = do.call(rbind.data.frame, KDE_ls)
head(KDE)
unique(KDE$Species)
#######__________________________________________________________________########


#### 3.2__________________________________________________________________#######
## Plotting

# single species approach
ggplot(KDE %>% filter(Species == "Bengal Fox"), aes(x = x/(2*pi)*24, y = y)) +
        geom_ribbon(aes(ymin = y - 1.96*se, ymax = y + 1.96*se, 
                        color = Species, 
                        fill = Species), 
                    alpha = 0.3, 
                    linewidth = 0.25) +
        geom_line(aes(color = Species), linewidth = 1) +
        theme_minimal() +
        scale_color_manual(values = c("orange")) +  # Only Bengal Fox color
        scale_fill_manual(values = c("orange")) +  # Only Bengal Fox fill
        labs(
                x = "time of day (hours)",
                y = "activity density",
                # y = "activity density \n (30 min interval)",
                title = "",
                tags = "A") + 
        theme_classic2() +
        my_custom_theme +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = seq(0, 24, 4), labels = seq(0, 24, 4))

# loop over each species and create a plot at a time
species_list = unique(KDE$Species)
for(species in species_list) {
        plot = ggplot(KDE %>% filter(Species == species), aes(x = x/(2*pi)*24, y = y)) +
                geom_ribbon(aes(ymin = y - 1.96*se, ymax = y + 1.96*se, color = Species, fill = Species), alpha = 0.3, linewidth = 0.25) +
                geom_line(aes(color = Species), linewidth = 1) +
                theme_minimal() +
                scale_color_manual(values = c("orange")) +  # Only Bengal Fox color
                scale_fill_manual(values = c("orange")) +  # Only Bengal Fox fill
                labs(x = "Time of Day (Hour)", y = "Predicted Activity Pattern \n (density) \n", 
                     title = paste("Activity Pattern for", species)) +
                my_custom_theme +
                scale_x_continuous(breaks = seq(0, 24, 4), labels = seq(0, 24, 4))
        
        print(plot)  # Printing the plot within the loop
}


# two species comparison
# filter data for the two comparisons: 
# Bengal Fox vs Bengal Monitor and Bengal Fox vs Human
KDE_bengal_fox_vs_monitor = KDE %>% filter(Species %in% c("Bengal Fox", 
                                                          "Bengal Monitor"))
KDE_bengal_fox_vs_human = KDE %>% filter(Species %in% c("Bengal Fox", 
                                                        "Human"))

# create the plot for Bengal Fox vs Bengal Monitor
plot_fox_vs_monitor = ggplot(KDE_bengal_fox_vs_monitor, 
                             aes(x = x/(2*pi)*24, y = y, color = Species, 
                                 fill = Species)) +
        geom_ribbon(aes(ymin = y - 1.96*se, ymax = y + 1.96*se), 
                    alpha = 0.3, linewidth = 0.25) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = c("Bengal Fox" = "orange", 
                                      "Bengal Monitor" = "green")) + 
        scale_fill_manual(values = c("Bengal Fox" = "orange", 
                                     "Bengal Monitor" = "green")) + 
        labs(x = "Time of Day (Hour)", 
             y = "Predicted Activity Pattern \n (density)", 
             title = "Activity Pattern: Bengal Fox vs Bengal Monitor") +
        my_custom_theme + 
        scale_x_continuous(breaks = seq(0, 24, 4), labels = seq(0, 24, 4))

# create the plot for Bengal Fox vs Human
plot_fox_vs_human = ggplot(KDE_bengal_fox_vs_human, 
                           aes(x = x/(2*pi)*24, y = y, 
                               color = Species, fill = Species)) +
        geom_ribbon(aes(ymin = y - 1.96*se, ymax = y + 1.96*se), 
                    alpha = 0.3, linewidth = 0.25) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = c("Bengal Fox" = "orange", 
                                      "Human" = "blue")) + 
        scale_fill_manual(values = c("Bengal Fox" = "orange",
                                     "Human" = "blue")) + 
        labs(x = "Time of Day (Hour)", 
             y = "Predicted Activity Pattern \n (density)", 
             title = "Activity Pattern: Bengal Fox vs Human") +
        my_custom_theme + 
        scale_x_continuous(breaks = seq(0, 24, 4), labels = seq(0, 24, 4))

## display both plots side by side
gridExtra::grid.arrange(plot_fox_vs_monitor, plot_fox_vs_human, ncol = 2)
#######__________________________________________________________________########
### STEP THREE ends



## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##
## STEP FOUR: GENERATING SURVEY REPORT
## 4.1 Dealing with camera trap table
## 4.2 Generating survey report
## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##


#### 4.1__________________________________________________________________#######
## Dealing with camera trap table

# loading camera trap data
cameratraptable = 
        read.csv("/Users/lynx025/Desktop/BengalFox_analyses/5.3 Activity Pattern Analyses_KDE/CamOp_BFox.csv") 

# making station name uniform
cameratraptable$Station = gsub(" ", "", cameratraptable$Station)

# making date and time in correct format
cameratraptable = cameratraptable %>%
        mutate(Setup_Date = format(as.Date(Setup_Date, format="%d-%m-%Y"), "%d/%m/%Y"))%>%
        mutate(Retrieval_Date = format(as.Date(Retrieval_Date, format="%d-%m-%Y"), "%d/%m/%Y"))%>%
        mutate(Problem1_from = format(as.Date(Problem1_from, format="%d-%m-%Y"), "%d/%m/%Y"))%>%
        mutate(Problem1_to = format(as.Date(Problem1_to, format="%d-%m-%Y"), "%d/%m/%Y"))%>%
        mutate(Problem2_from = format(as.Date(Problem2_from, format="%d-%m-%Y"), "%d/%m/%Y"))%>%
        mutate(Problem2_to = format(as.Date(Problem2_to, format="%d-%m-%Y"), "%d/%m/%Y"))%>%
        mutate(Problem3_from = format(as.Date(Problem3_from, format="%d-%m-%Y"), "%d/%m/%Y"))%>%
        mutate(Problem3_to = format(as.Date(Problem3_to, format="%d-%m-%Y"), "%d/%m/%Y"))

# add a dummy station row
# NOTE: single camera trap station
# the matrix can not be rendered in that scenario
# not necessary, if there are multiple stations
cameratraptable_test = rbind(
        cameratraptable,
        cameratraptable[1, ])
head(cameratraptable_test)

# rename the duplicated row
cameratraptable_test$Station[2] = 
        paste0(cameratraptable_test$Station[1], "_dummy")
head(cameratraptable_test)

# recording camop_problem
camop_problem = camtrapR::cameraOperation(
        CTtable      = cameratraptable_test,
        stationCol   = "Station",
        setupCol     = "Setup_Date",
        retrievalCol = "Retrieval_Date",
        writecsv     = FALSE,
        hasProblems  = TRUE,
        dateFormat   = "%d/%m/%Y")
head(camop_problem)

date_range = which(colnames(camop_problem) %in% c("2024-04-02", 
                                                   "2024-04-03", 
                                                   "2024-04-04", 
                                                   "2024-04-05", 
                                                   "2024-04-06", 
                                                   "2024-04-07", 
                                                   "2024-04-08", 
                                                   "2024-04-09"))
camop_problem["StationA", date_range] = 0.5
camop_problem = camop_problem[-2, , drop = F]
#######__________________________________________________________________########


#### 4.2__________________________________________________________________#######
## Generating survey report

reportTest = camtrapR::surveyReport(
        recordTable          = recordtable,
        CTtable              = cameratraptable_test,
        camOp                = camop_problem,
        speciesCol           = "Species",
        stationCol           = "Station",
        setupCol             = "Setup_Date",
        retrievalCol         = "Retrieval_Date",
        CTDateFormat         = "%d/%m/%Y",
        recordDateTimeCol    = "DateTimeOriginal",
        recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
        sinkpath             = getwd(),
        Xcol                 = "utm_x",
        Ycol                 = "utm_y",
        makezip              = T
)

class(reportTest) # a list with
length(reportTest) # 5 elements
reportTest[[1]] # camera trap operation times and image date ranges
reportTest[[2]] # number of species by station
reportTest[[3]] # number of events and number of stations by species
reportTest[[4]] # number of species events by station
reportTest[[5]] # number of species events by station including 0s (non-observed species)


# Optional: estimating activity density with overlap
# head(recordtable$Species_split)
# activityDensity(recordTable = recordtable,
#                 speciesCol = "Species_split",
#                 allSpecies = TRUE,
#                 writePNG = FALSE,
#                 plotR = TRUE,
#                 add.rug = TRUE)
# ?activityDensity

# species4activity = "Bengal Fox" # = Vulpes bengalensis
# activityDensity(recordTable = recordtable,
#                 species = species4activity,
#                 plotR = TRUE,
#                 add.rug = TRUE)

# Optional: creating shapefile
# longlat = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# withV = recordtable

# richness = detectionMaps(CTtable = cameratraptable_test,
#                          recordTable = recordtable,
#                          Xcol="longitude",
#                          Ycol = "latitude",
#                          stationCol = "Station",
#                          speciesCol = "Species_split",
#                          richnessPlot = T,
#                          speciesPlots = T,
#                          writeShapefile = T,
#                          shapefileName = "testshape",
#                          shapefileDirectory = 
#                                  "/Users/lynx025/Desktop/BengalFox_analyses/5.1 Activity Pattern Analyses_KDE/shape",
#                          plotDirectory= 
#                                  "/Users/lynx025/Desktop/BengalFox_analyses/5.1 Activity Pattern Analyses_KDE/plot",
#                          shapefileProjection = longlat)
#######__________________________________________________________________########
## STEP FOUR ends



## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##
## STEP FIVE: OVERLAP COEFFICIENTS
## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##

# split by species
# fox_times = KDE_bengal_fox_vs_human$Original_Time_Rad[KDE_bengal_fox_vs_human$Species == "Bengal Fox"]
# human_times = KDE_bengal_fox_vs_human$Original_Time_Rad[KDE_bengal_fox_vs_human$Species == "Human"]

# # estimate overlap
# overlap_result = overlapEst(fox_times, human_times)

# direct from original dataset
Fox$rad = astroFns::hms2rad(Fox$Time)
fox_times = Fox$rad[Fox$Species_split == "Bengal Fox"]
human_time = Fox$rad[Fox$Species_split == "Human"]; human_time
Bengal_Monitor_time = Fox$rad[Fox$Species_split == "Bengal Monitor"]; Bengal_Monitor_time

# remove NA, if any
fox_times_clean = fox_times[!is.na(fox_times)]
human_times_clean = human_time[!is.na(human_time)]
Bengal_Monitor_times_clean = Bengal_Monitor_time[!is.na(Bengal_Monitor_time)]

# estimate overlap
# n < 75 per group
overlapEst(fox_times_clean, human_times_clean,
           type = "Dhat1"
           )
# n > 75 per group
overlapEst(fox_times_clean, Bengal_Monitor_times_clean,
           type = "Dhat4"
           )

# bootstrapping # 95% confidence interval
# ?bootCI
fox_human_boot = bootstrap(fox_times_clean, human_times_clean, 
                          nb = 1000, type = "Dhat1")
mean(fox_human_boot)
hist(fox_human_boot)
bootCI(overlapEst(fox_times_clean, human_times_clean,
                  type = "Dhat1"), fox_human_boot)


fox_monitor_boot = bootstrap(fox_times_clean, Bengal_Monitor_times_clean, 
                             nb = 1000, type = "Dhat4")

mean(fox_monitor_boot)
hist(fox_monitor_boot)
bootCI(overlapEst(fox_times_clean, Bengal_Monitor_times_clean,
                  type = "Dhat4"), fox_monitor_boot)
#######__________________________________________________________________########
