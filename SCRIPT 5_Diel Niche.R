####################  Bengal Fox Denning Behaviour  ############################
################               Bangladesh             ##########################
################               script by              ##########################
################             Muntasir Akash           ##########################
##############        estimating Diel Niche Pattern       ######################
############             with package Diel.Niche           #####################



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
##_______________________________________________________
##########################################################



## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##
## libraries
library(camtrapR)  # updated February 24, 2024
library(tidyverse) # data wrangling
library(lubridate) # data wrangling
library(dplyr)     # data wrangling
library(hms)       # date-time conversion
library(astroFns)  # time to radian conversion and vice-versa
library(activity)  # activity pattern analyses
library(suncalc)   # calculating nautical dawn and nautical dusk
library(ggplot2)   # plotting
library(gridExtra) # plotting
library(Diel.Niche)# Diel.Niche activity pattern 
library(lubridate)
library(coda)
library(ggthemes)
library(bayesplot)
# Note: see Gerber et al. (2023) https://doi.org/10.1111/1365-2656.14035
## ---------------------------------------------------------------------------##
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
Fox = read.csv("/Users/lynx025/Desktop/BengalFox_analyses/5.3.1 Diel Niche/BFox_cleaned_Apr20.csv") 
# Data = read.csv(file = "Fox_MA_edits.csv", header = T, row.names = NULL)
## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##



## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##
## STEP ONE: DATE-TIME AND DIEL CYCLE CALCULATIONS
## 1.1 Making date object
## 1.2 Converting time to radian
## 1.3 Classifying diel cycle  [test]
## 1.4 Classifying diel cycle  [within the original dataset]
## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##



#### 1.1__________________________________________________________________#######
## Making date object
head(Fox)
str(Fox)
unique(Fox$Species)

# select only relevant columns
Fox =  Fox %>%
        select(Station, Time, Date, DateTimeOriginal, Species) %>%
        mutate(
                DateTimeOriginal = lubridate::ymd_hms(DateTimeOriginal, tz = "Asia/Dhaka"),
                Time = hms::as_hms(Time),
                Date_parsed = as.Date(Date, format = "%d-%m-%Y"))

# Fox = Fox %>% filter(Species == "Bengal Fox") 
# Fox = Fox %>% filter(Species %in% c("Bengal Fox", 
#                                     "Bengal Fox and Bengal Monitor", 
#                                     "Bengal Fox and Jungle Babbler")) 

table(Fox$Species)
sum(is.na(Fox$Species))
# Fox = Fox[!is.na(Fox$Species), ]
#######__________________________________________________________________#######


#### 1.2__________________________________________________________________#######
## Converting time to radian

# NOTE: requirement of the packages activity, circular, overlap

head(Fox)
str(Fox)

Fox = Fox %>% mutate(Time = as_hms(Time))
Fox$Time_Rad = astroFns::hms2rad(Fox$Time)
head(Fox$Time_Rad)

# using pacakge hms
# dat_event$Time = hms::as_hms(dat_event$Time)
# str(dat_event$Time)
# dat_event$Time_Rad = (hour(dat_event$Time)*60 + 
#                               minute(dat_event$Time))/(60*24)*2*pi
# head(dat_event$Time_Rad)
#######__________________________________________________________________#######


#### 1.3__________________________________________________________________#######
##  Classifying day-time, night-time, and crepuscular time activity
##  for specific dates and outside the original dataset

# NOTE: Define nautical dawn and nautical dusk at the site
# for the deployment period

# Start Date 08-03-2024
# Retrieval Date 28-05-2024

# define location and date
lat = 26.2462      
lon = 88.4985

# for a single date
# dateMar = as.Date("2024-03-08")
# suncalc::getSunlightTimes(date, lat, lon) |> names()
# ?getSunlightTimes
# twilight_march = getSunlightTimes(
#         date = dateMar,
#         lat = lat,
#         lon = lon,
#         keep = c("nauticalDawn", "sunrise", "dawn", "sunriseEnd",
#                  "sunsetStart", "dusk", "sunset", "nauticalDusk"),
#         tz = "Asia/Dhaka")  # time zone
# 
# dateMay = as.Date("2024-05-28")
# twilight_may = getSunlightTimes(
#         date = dateMay,
#         lat = lat,
#         lon = lon,
#         keep = c("nauticalDawn", "sunrise", "dawn", "sunriseEnd",
#                  "sunsetStart", "dusk", "sunset", "nauticalDusk"),
#         tz = "Asia/Dhaka")  # time zone
# 
# dateApr = as.Date("2024-04-30")
# twilight_Apr = getSunlightTimes(
#         date = dateApr,
#         lat = lat,
#         lon = lon,
#         keep = c("nauticalDawn", "sunrise", "dawn", "sunriseEnd",
#                  "sunsetStart", "dusk", "sunset", "nauticalDusk"),
#         tz = "Asia/Dhaka")  # time zone
# 
# print(twilight_march)
# print(twilight_Apr)
# print(twilight_may)

# NOTE: for everyday of the survey period
# twilight_times = Fox %>%
#         distinct(Date_parsed) %>%
#         rowwise() %>%
#         mutate(
#                 twilight = list(getSunlightTimes(
#                         date = Date_parsed,
#                         lat = 26.2462,
#                         lon = 88.4985,
#                         keep = c("nauticalDawn", "sunriseEnd", "sunsetStart", "nauticalDusk"),
#                         tz = "Asia/Dhaka"
#                 ))
#         ) %>%
#         unnest(twilight) %>%
#         mutate(across(c(nauticalDawn, sunriseEnd, sunsetStart, nauticalDusk),
#                       ~ lubridate::with_tz(.x, tzone = "Asia/Dhaka")))
# head(Fox)
# # NOTE: so, it is shifting, naturally. 
#######__________________________________________________________________#######


#### 1.3__________________________________________________________________#######
##  Classifying day-time, night-time, and crepuscular time activity
##  within the original dataset

# make the date column as a date object
Fox = Fox %>%
        mutate(Date_parsed = as.Date(Date, format = "%d-%m-%Y"))
str(Fox$Date_parsed)

# get unique dates and compute twilight times for each
twilight_times = Fox %>%
        distinct(Date_parsed) %>%
        rowwise() %>%
        mutate(
                twilight = list(getSunlightTimes(
                        date = Date_parsed,
                        lat = 26.2462,
                        lon = 88.4985,
                        keep = c("nauticalDawn", "dawn", "sunrise", "sunriseEnd",
                                 "sunsetStart", "sunset", "dusk", "nauticalDusk"),
                        tz = "Asia/Dhaka"))) %>%
        unnest(twilight)

# join twilight times back to fox dataset and classifying
# Fox = Fox %>%
#         left_join(twilight_times, by = c("Date_parsed" = "date")) %>%
#         mutate(
#                 simple_period = case_when(
#                         DateTimeOriginal > sunriseEnd & DateTimeOriginal < sunsetStart ~ "Daylight",
#                         (DateTimeOriginal >= nauticalDawn & DateTimeOriginal <= sunriseEnd) |
#                                 (DateTimeOriginal >= sunsetStart & DateTimeOriginal <= nauticalDusk) ~ "Twilight",
#                         TRUE ~ "Night"
#                 )
#         )

# NOTE: to consider a buffer, follow the approach below:
Fox = Fox %>%
        left_join(twilight_times, by = c("Date_parsed" = "date")) %>%
        mutate(
                simple_period = case_when(
                        # Daylight is now between 30 min after sunriseEnd and 30 min before sunsetStart
                        DateTimeOriginal > (sunriseEnd + minutes(30)) & DateTimeOriginal < (sunsetStart - minutes(30)) ~ "Daylight",

                        # Twilight: 30 min before nauticalDawn to 30 min after sunriseEnd (morning),
                        # and 30 min before sunsetStart to 30 min after nauticalDusk (evening)
                        (DateTimeOriginal >= (nauticalDawn - minutes(30)) & DateTimeOriginal <= (sunriseEnd + minutes(30))) |
                                (DateTimeOriginal >= (sunsetStart - minutes(30)) & DateTimeOriginal <= (nauticalDusk + minutes(30))) ~ "Twilight",

                        # Everything else is Night
                        TRUE ~ "Night"
                )
        )

# NOTE: as is classification, not necessary here
# Fox = Fox %>%
#         mutate(period = case_when(
#                 DateTimeOriginal < nauticalDawn ~ "Night",
#                 DateTimeOriginal < dawn ~ "Nautical Twilight",
#                 DateTimeOriginal < sunrise ~ "Civil Twilight (AM)",
#                 DateTimeOriginal < sunset ~ "Daylight",
#                 DateTimeOriginal < dusk ~ "Civil Twilight (PM)",
#                 DateTimeOriginal < nauticalDusk ~ "Nautical Twilight",
#                 TRUE ~ "Night"))

# checking
table(Fox$simple_period)
table(Fox$Species)
sum(table(Fox$Species))-4502


# test a species
Fox %>% filter(Species == "Feral Dog")
Fox %>% filter(Species == "Human")
#######__________________________________________________________________########
## STEP ONE ends



## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##
## STEP TWO: SUMMARISE DETECTIONS
## 2.1 Summarise detections by period and species
## 2.2 Summarise average twillight start and end period
## 2.3 Considering co-detections
## 2.4 Meeting KDE requirement of independence
## 2.5 Creating Diel.Niche dataframe 
## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##



#### 2.1__________________________________________________________________#######
## summarise detections by period and species
diel_summary = Fox %>%
        count(Species, simple_period) %>%
        tidyr::pivot_wider(names_from = simple_period, 
                           values_from = n, 
                           values_fill = 0) %>%
        rename(day = Daylight,
               night = Night,
               twilight = Twilight) %>%
        mutate(trap_nights = 58,
                nsite = 1,
                min_date = "08-03-2024",
                max_date = "28-05-2024",
                mean_lat = 26.2462,
                mean_lon = 88.4985,
                season = "Summer",
                country = "Bangladesh")

save(Fox, diel_summary, file = "fox_diel_summary.RData")
write.csv(diel_summary, "diel_summary.csv")
#######__________________________________________________________________########


#### 2.2__________________________________________________________________#######
## Summarise average twillight start and end period
twilight_summary = twilight_times %>%
        summarise(
                morning_start = mean(hour(nauticalDawn) + minute(nauticalDawn) / 60) - .5,
                morning_end   = mean(hour(sunriseEnd)   + minute(sunriseEnd)   / 60) + .5,
                evening_start = mean(hour(sunsetStart)  + minute(sunsetStart)  / 60) - .5,
                evening_end   = mean(hour(nauticalDusk) + minute(nauticalDusk) / 60) + .5
        )
twilight_summary
#######__________________________________________________________________########


#### 2.3__________________________________________________________________######
## Considering co-detections
## e.g. Bengal Fox and Bengal Monitor 
Fox2 = Fox %>%
        filter(!is.na(Species)) %>%
        mutate(Species_split = strsplit(Species, " and ")) %>%
        unnest(Species_split) %>%
        mutate(Species_split = str_trim(Species_split))

head(Fox2)
colnames(Fox2)
unique(Fox2$Species_split)
table(Fox2$Species_split)
#######__________________________________________________________________########


#### 2.4__________________________________________________________________#######
## Meeting KDE requirement of independence

# NOTE: in case of considering independent events
# data-thinning set the threshold that define independence independence
# between subsequent records ## an KDE requirement
# aggregate data to reduce short-term temporal dependence

independence_interval = 0 # minutes # can be any time-interval

## now, the aggregation
Fox2 = Fox2 %>% 
        mutate(DateTimeOriginal = ymd_hms(DateTimeOriginal)) %>% 
        group_by(Species_split, Station) %>%      # group by species and location
        arrange(DateTimeOriginal, .by_group = TRUE) %>% # arrange record temporally within each group
        mutate(timediff = as.numeric(difftime(DateTimeOriginal, lag(DateTimeOriginal), units="mins")), # calculate time difference (in minutes) between consecutive events
               timediff = replace_na(timediff, 0),
               grp = ifelse(timediff >= independence_interval, 1, 0), # create events
               grp_ind = cumsum(grp) + 1) 
Fox2 = Fox2 %>%
        group_by(Species_split, Station, grp_ind) %>%
        slice(1)
head(Fox2)
table(Fox2$Species_split)
table(Fox2$Species_split)
#######__________________________________________________________________########


#### 2.5__________________________________________________________________#######
## Creating Diel.Niche summary for diel.niche package analysis
diel_summary2 = Fox2 %>%
        group_by(Species_split, simple_period) %>%
        summarise(detections = n(), .groups = "drop") %>%
        group_by(Species_split) %>%
        summarise(
                day = sum(detections[simple_period == "Daylight"], na.rm = TRUE),
                night = sum(detections[simple_period == "Night"], na.rm = TRUE),
                twilight = sum(detections[simple_period == "Twilight"], na.rm = TRUE)
        ) %>%
        mutate(
                trap_nights = 58,
                nsite = 1,
                min_date = as.Date("08-03-2024", format = "%d-%m-%Y"),
                max_date = as.Date("28-05-2024", format = "%d-%m-%Y"),
                mean_lat = 26.2462,
                mean_lon = 88.4985,
                season = "Summer",
                country = "Bangladesh"
        )

save(Fox2, diel_summary2, file = "DN_summary.RData")
write.csv(diel_summary2, "DN_summary.csv")
#######__________________________________________________________________########
## STEP TWO ends



rm(list = ls())
load("DN_summary.RData")


## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##
## STEP THREE: SUMMARISE DETECTIONS
## 3.1 Diel Niche analyses [Maximizing Hypothesis]
## 3.2 Diel Niche analyses [Selection Hypothesis]
## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##


#### 3.1__________________________________________________________________#######
## Diel.Niche analyses [Maximizing]

# converting date to date object
diel_summary2$min_year = lubridate::year(
        as.POSIXct(
                diel_summary2$min_date,
                format = "%d-%m-%Y"))

# optional, just to match built-in data frame provided with Diel.Niche
bfox = subset(
        diel_summary2,
        season == "Summer" &
        min_year == "2024" &
        Species_split == "Bengal Fox")

# creating the final dataframe to plug in wiht Diel.Niche functions
y = data.frame(
        twilight = bfox$twilight,
        day = bfox$day, 
        night = bfox$night)

rownames(y) = bfox$Species_split

# triplot(hyp = hyp.sets("Traditional"))

# model run
out = diel.fit(
        y = as.matrix(y),
        hyp.set = hyp.sets("Maximizing"),
        post.fit = TRUE, 
        n.chains = 3,
        n.mcmc = 50000,
        burnin = 10000
        )

# model output
out$bf.table

# model checking
out$ms.gelm.diag
plot(coda::as.mcmc(out$post.samp.ms.model))
# NOTE: Gelman–Rubin diagnostic values (also known as R-hat values) 
# for MCMC convergence. 1 indicates convergence of MCMC chains

# model checking
out$ms.ppc
plot_title = ggplot2::ggtitle(
        "Posterior distributions",
        "with medians and 95% intervals")
bayesplot::mcmc_areas(
        out$post.samp.ms.model,
        prob = 0.95
) + plot_title
# NOTE: posterior predictive checking 
# Values near 0.5 indicate a good fit between the model and the observed data.
# Values close to 0 or 1 suggest a poor fit, 
# indicating the model consistently under- or over-predicts.

# final plotting
triplot(out) 
round(t(apply(out$post.samp.ms.model,2,
                        quantile,
                        probs = c(0.025,0.5,0.975))),2)
#######__________________________________________________________________########


#### 3.2__________________________________________________________________#######
## Diel.Niche analyses [Selection]

chicago.hours = suncalc::getSunlightTimes(
        date = bfox$min_date,
        lat = unique(bfox$mean_lat),
        lon = unique(bfox$mean_lon),
        keep = c("sunset", "night", "sunrise", "nightEnd"),
        tz = "Asia/Dhaka"
)


# calculate the hours in each diel period
twilight.hours = (chicago.hours$sunrise - chicago.hours$nightEnd) +
        (chicago.hours$night - chicago.hours$sunset)
day.hours = chicago.hours$sunset - chicago.hours$sunrise
night.hours = 24 - (twilight.hours + day.hours)

# add some new columns to diel.data, but also make these hours
# into proportions.
bfox$prop_twilight = as.numeric(twilight.hours / 24)
bfox$prop_day =  as.numeric(day.hours / 24)
bfox$prop_night = as.numeric(night.hours / 24)

# diel.ineq function before running the model
selection.ineq1 = diel.ineq(
        p.avail =  c(bfox$prop_twilight[1],
                bfox$prop_day[1]))
selection.ineq2 = diel.ineq(
        p.avail =  c(bfox$prop_twilight,bfox$prop_day))

# checking proportions
triplot(hyp = hyp.sets("Selection"),
        diel.setup = selection.ineq1)
triplot(hyp = hyp.sets("Selection"),
        diel.setup = selection.ineq2)

# Diel.Niche format
bfox.y1 = as.matrix(
        bfox[1,c("twilight", "day", "night")])

# model run
out1 = diel.fit(
        bfox.y1,
        hyp.set = hyp.sets("Selection"),
        diel.setup = selection.ineq2,
        post.fit = TRUE, 
        n.chains = 3,
        n.mcmc = 10000,
        burnin = 2000)

# model output 
out1$bf.table
round(bfox.y1 / sum(bfox.y1), 2)
round(bfox[1,c("prop_twilight", "prop_day", "prop_night")], 2)

hist(out1$post.samp.ms.model[,"p_night_1"] / bfox$prop_night[1],
        main = "",
        xlim = c(0,3),
        las = 1,
        xlab = "Selection for nighttime activity relative to availability")
abline(v = 1, lty = 2)

# model checking
out1$ms.gelm.diag
plot(coda::as.mcmc(out1$post.samp.ms.model))
# NOTE: Gelman–Rubin diagnostic values (also known as R-hat values) 
# for MCMC convergence. 1 indicates convergence of MCMC chains

# model checking
out1$ms.ppc
plot_title = ggplot2::ggtitle(
        "Posterior distributions",
        "with medians and 95% intervals")
bayesplot::mcmc_areas(
        out1$post.samp.ms.model,
        prob = 0.95
) + plot_title
# NOTE: posterior predictive checking 
# Values near 0.5 indicate a good fit between the model and the observed data.
# Values close to 0 or 1 suggest a poor fit, 
# indicating the model consistently under- or over-predicts.

# final plotting
triplot(out1,
        diel.setup = selection.ineq1,
        axis.text = "Times")

round(t(apply(out1$post.samp.ms.model,2,
              quantile,
              probs = c(0.025,0.5,0.975))),2)


hist(out1$post.samp.ms.model[,c("p_day_1")]/bfox$prop_day,
        freq = FALSE,
        xlim=c(0,2),
        main="",
        col="yellow",
        las=1,
        xlab = "Selection for day and nighttime activity relative to availability")

hist(out1$post.samp.ms.model[,c("p_night_1")]/bfox$prop_night,
        freq = FALSE,
        col="blue",
        add=TRUE)

hist(out1$post.samp.ms.model[,c("p_crep_1")]/bfox$prop_twilight,
     freq = FALSE, col="green",add=TRUE)


# NOTE: see Allen et al and Gerber et al for the significance of these
# predictive distribution
out1$post.samp.ms.model
twilight_median = median(out1$post.samp.ms.model[, "p_crep_1"])
twilight_bci = quantile(out1$post.samp.ms.model[, "p_crep_1"], probs = c(0.025, 0.975))

twilight_median = median(out$post.samp.ms.model[, "p_crep_1"])
twilight_bci = quantile(out$post.samp.ms.model[, "p_crep_1"], probs = c(0.025, 0.975))

day_median = median(out1$post.samp.ms.model[, "p_day_1"])
day_bci = quantile(out1$post.samp.ms.model[, "p_day_1"], probs = c(0.025, 0.975))

night_median = median(out1$post.samp.ms.model[, "p_night_1"])
night_bci = quantile(out1$post.samp.ms.model[, "p_night_1"], probs = c(0.025, 0.975))

twilight_median
twilight_bci
day_median
twilight_bci
night_median
night_bci
#######__________________________________________________________________########


#######__________________________________________________________________########
## tweaking plot characteristics
fig <- triplot(out)
fig <- fig %>% plotly::layout(
        scene = list(
                xaxis = list(
                        titlefont = list(family = "Times New Roman"),
                        tickfont = list(family = "Times New Roman")
                ),
                yaxis = list(
                        titlefont = list(family = "Times New Roman"),
                        tickfont = list(family = "Times New Roman")
                ),
                zaxis = list(
                        titlefont = list(family = "Times New Roman"),
                        tickfont = list(family = "Times New Roman")
                )
        ),
        legend = list(font = list(family = "Times New Roman",
                                  size = 22))
)
fig

fig2 <- triplot(out1, diel.setup = selection.ineq1)
fig2 <- fig2 %>% plotly::layout(
        scene = list(
                xaxis = list(
                        titlefont = list(family = "Times New Roman"),
                        tickfont = list(family = "Times New Roman")
                ),
                yaxis = list(
                        titlefont = list(family = "Times New Roman"),
                        tickfont = list(family = "Times New Roman")
                ),
                zaxis = list(
                        titlefont = list(family = "Times New Roman"),
                        tickfont = list(family = "Times New Roman")
                )
        ),
        legend = list(font = list(family = "Times New Roman",
                                  size = 22))
)
fig2
#######__________________________________________________________________########
## STEP THREE ends



## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##
## STEP FOUR: RELATIVE ABUNDANCE CALCULATION
## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##

# Set trap nights
trap_nights = 58

# Count detections per species
rai_table = Fox %>%
        filter(!is.na(Species)) %>%
        count(Species, name = "detections") %>%
        mutate(RAI = round((detections / trap_nights), 2)) %>%
        arrange(desc(RAI))
print(rai_table)
#######__________________________________________________________________########
#######__________________________________________________________________########
## STEP FOUR ends


## MISC 
# add a month column (as ordered factor for clarity)
Fox = Fox %>%
        mutate(month = factor(month(Date_parsed, label = TRUE, abbr = TRUE), 
                              levels = month.abb, ordered = TRUE))

# NOTE: necessary for the Result Table
monthly_detections_wide = Fox %>%
        filter(!is.na(Species)) %>%
        count(Species, month, name = "detections") %>%
        pivot_wider(names_from = month, values_from = detections, values_fill = 0) %>%
        arrange(Species)

print(monthly_detections_wide)

