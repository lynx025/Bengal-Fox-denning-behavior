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
load("BFox_cleaned_Apr20.RData")
# Data = read.csv(file = "Fox_MA_edits.csv", header = T, row.names = NULL)
## ---------------------------------------------------------------------------##

## ---------------------------------------------------------------------------##
## Excluding "Blank"
## remove rows that are exactly "Blank"
BFoxFinal = BFox[!(BFox$FOBehav_Single %in% c("Blank")), ]
BFoxFinal = BFoxFinal[(BFoxFinal$Species %in% c("Bengal Fox",
                                                "Bengal Fox and Jungle Babbler",
                                                "Bengal Fox and Bengal Monitor")), ]
# BFoxFinal = BFox[!(BFox$FOBehav_Single %in% c("Blank", "Detection")), ]

## remove "Blank," from combined behavior strings
BFoxFinal$FOBehav_combined <- gsub("Blank,", "", BFoxFinal$FOBehav_combined)

## Optional: Also clean up trailing commas or whitespace, if any
BFoxFinal$FOBehav_combined <- gsub(",$", "", BFoxFinal$FOBehav_combined)
BFoxFinal$FOBehav_combined <- trimws(BFoxFinal$FOBehav_combined)

unique(BFoxFinal$FOBehav_combined)
unique(BFox$FOBehav_combined)
unique(BFoxFinal$Species)
case1 = BFoxFinal %>% filter(FOBehav_combined == "")

## The current state of the dataframe tells us about all detections
## during the survey period. 
## The "" in the BFoxFinal$FOBehav_Single corresponds to Inter/Intra-species 
## interaction
## Undefinable will be food-caching behavior

## ---------------------------------------------------------------------------##
## CALCULATING the total survey day and hours of footage
## and total number of footage obtained 
str(BFoxFinal$Time)
str(BFoxFinal$Date)
str(BFox$VisitLength)

sum(BFoxFinal$VisitLength)
# 42297/3600
# 11.75 hours of footage
length(unique(BFoxFinal$Date))
# 56 days of camera-trapping
nrow(BFoxFinal)
# 4502 footage
## ---------------------------------------------------------------------------##

## ---------------------------------------------------------------------------##
## FIGURE: clip length vs no. of clips
BFoxFinal$ClipLengthBin <- cut(
        BFoxFinal$VisitLength,
        breaks = c(0, 5, 10, 15, 20, 30, 60),
        labels = c("01–05s", "06–10s", "11–15s", "16–20s", "21–30s", "31–60s"),
        right = TRUE, include.lowest = TRUE)

ggplot(BFoxFinal, aes(x = ClipLengthBin)) +
        geom_bar(fill = "#69b3a2", color = "black") +
        theme_minimal(base_family = "Times New Roman", base_size = 16) +
        labs(x = "clip length (seconds)",
                y = "no. of  clips") +
        theme( axis.text = element_text(size = 14),
                axis.title = element_text(size = 16),
                # plot.title = element_text(size = 18, face = "bold")
               )
## ---------------------------------------------------------------------------##

## ---------------------------------------------------------------------------##
## FIGURE: no. of clips over time
## NOTE: important to show the impact of camera-trapping on detectability
## and developing camera shyness
BFox_per_day = BFoxFinal %>%
                         group_by(Date) %>%
                         summarise(n_clips = n())

ggplot(BFox_per_day, aes(x = Date, y = n_clips)) +
        geom_col(fill = "#4F81BD") +
        theme_minimal(base_family = "Times New Roman", base_size = 14) +
        labs(x = "Survey Day", y = "Number of Clips",
             title = "Camera-trap Detections per Survey Day")
## NOTE: This is too messy. There are gaps in weeks.

## Let's try months
## extract months
BFoxFinal =  BFoxFinal %>%
                 mutate(Date = as.Date(Date),
                        Month = format(Date, "%B"))
unique(BFoxFinal$Species)
## calculate survey days per month
survey_days_per_month = BFoxFinal %>%
                         distinct(Date, Month) %>%
                         count(Month, name = "SurveyDays")
## calculate clips per month ## histogram
clips_per_month = BFoxFinal %>%
                         count(Month, name = "ClipCount")

## calculate clips per day for each month ## Boxplot and Violin plot
daily_clip_data = BFoxFinal %>%
                   group_by(Date, Month) %>%
                   summarise(n_clips = n(), .groups = "drop") %>%
                   left_join(survey_days_per_month, by = "Month") %>%
                   mutate(MonthLabel = paste0(Month, " (", SurveyDays, 
                                              " days)"))
## for histogram
plot_data =  left_join(clips_per_month, 
                       survey_days_per_month, 
                       by = "Month") %>%
             mutate(MonthLabel = paste0(Month, 
                                        " (", SurveyDays, " days)"))
## for histogram
plot_data$MonthLabel = factor(plot_data$MonthLabel, 
                              levels = plot_data$MonthLabel
                              [order(match(plot_data$Month, 
                                           c("March", "April", "May")))])

## HISTOGRAM
ggplot(plot_data, aes(x = MonthLabel, y = ClipCount)) +
        geom_col(fill = "#4F81BD", alpha = 0.8) +
        theme_minimal(base_family = "Times New Roman", base_size = 14) +
        labs(x = "", y = "no. of clips"
             #title = "Monthly Variation in Camera-trap Detections"
        )

## BOX PLOT
ggplot(daily_clip_data, aes(x = forcats::fct_relevel(MonthLabel, 
                                            "March (16 days)", 
                                            "April (20 days)", 
                                            "May (20 days)"), 
                            y = n_clips)) +
        geom_boxplot(fill = "steelblue", alpha = 0.4, outlier.shape = NA) +
        geom_jitter(width = 0.2, alpha = 0.6, size = 1, color = "brown") +
        theme_minimal(base_family = "Times New Roman", base_size = 14) +
        labs(x = "", y = "no. of clips / day",
             #title = "Variation in Daily Camera-trap Detections by Month"
             )

## VIOLIN PLOT
ggplot(daily_clip_data, aes(x = forcats::fct_relevel(MonthLabel, 
                                                     "March (16 days)", 
                                                     "April (20 days)", 
                                                     "May (20 days)"), 
                            y = n_clips)) +
        geom_violin(fill = "steelblue", alpha = 0.4) +
        geom_jitter(width = 0.2, alpha = 0.6, size = 1, color = "brown") +
        theme_minimal(base_family = "Times New Roman", base_size = 14) +
        labs(x = "", y = "Number of Clips per Day",
             title = "Variation in Daily Camera-trap Detections by Month")
## ---------------------------------------------------------------------------##

## ---------------------------------------------------------------------------##
## RELATIONSHIP between VisitLength and No. of Behaviour Detected
unique(BFoxFinal$FOBehav_combined)
BFoxFinal = BFoxFinal %>%
        mutate(No.Behav = sapply(strsplit(FOBehav_combined, ","), length))
unique(BFoxFinal$No.Behav)
cor(BFoxFinal$No.Behav, BFoxFinal$VisitLength)

ggplot(BFoxFinal, aes(x = No.Behav, y = VisitLength)) +
        geom_point(color = "steelblue", alpha = 0.6) +
        geom_smooth(method = "lm", color = "red", se = FALSE) +
        theme_minimal(base_family = "Times New Roman", base_size = 14) +
        labs(x = "Number of Behaviors", 
             y = "Visit Length (seconds)",
             #title = "Relationship between Number of Behaviors and Visit Length"
             )
## ---------------------------------------------------------------------------##

case11 = BFoxFinal %>% filter(FOBehav_combined == "Passing")
case12 = BFoxFinal %>% filter(FOBehav_combined == "")

BFoxFinal %>%
        filter(EndofVisit == "Yes") %>%
        count()
table(BFoxFinal$FOBehav_combined)
nrow(BFoxFinal)



