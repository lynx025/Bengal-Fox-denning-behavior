## ---------------------------------------------------------------------------##
## PREPARE recordData and camOP for checking detection probability

## dependencies
library(camtrapR)    # updated February 24, 2024
library(tidyverse)   # data wrangling
library(lubridate)   # data wrangling
library(dplyr)       # data wrangling
library(lorelogram)  # 
library(ggplot2)    # plotting
library(gridExtra)   # plotting
library(ggpubr)      # plotting
## ---------------------------------------------------------------------------##

rm(list = ls())

## read data: 
## read recordTable file as created using function 
## recordTable in the camtrapR package 
## see https://cran.r-project.org/web/packages/camtrapR/vignettes/DataExtraction.html#camera-operation
recordData <- read.csv("/Users/lynx025/Desktop/BengalFox_analyses/4. Detection Probability/BFox_cleaned_Apr20.csv") 
# Fox = Fox %>% filter(Species == "Bengal Fox") 
# Fox = Fox %>% filter(Species %in% c("Bengal Fox", 
#                                     "Bengal Fox and Bengal Monitor", 
#                                     "Bengal Fox and Jungle Babbler")) 
head(recordData)
str(recordData)

recordData = recordData %>% 
        droplevels %>% 
        select(Station, Time, Date, DateTimeOriginal, Species) %>% 
        mutate(DateTimeOriginal = ymd_hms(DateTimeOriginal, tz = "UTC"))%>%
        mutate(Date = dmy(Date))%>%
        mutate(Time = hms(Time))

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

unique(recordData$Species)
table(recordData$Species)

## make the data.frame shorter
recordData = recordData %>%
        dplyr::select(Station, Time, Date,
                      Species = Species_split, 
                      DateTimeOriginal)

## check with minimum and maximum date
str(recordData)
min(recordData$DateTimeOriginal, na.rm = TRUE)
max(recordData$DateTimeOriginal, na.rm = TRUE)

recordData = recordData %>%
        mutate(
                roundedDateTime = round_date(DateTimeOriginal, unit = "minute"),
                checks = case_when(
                        roundedDateTime > as.POSIXct("2024-03-08 00:00:00") & roundedDateTime <= as.POSIXct("2024-03-18 16:00:00") ~ "Round 01",
                        roundedDateTime > as.POSIXct("2024-03-18 16:00:01") & roundedDateTime <= as.POSIXct("2024-03-31 12:00:00") ~ "Round 02",
                        roundedDateTime > as.POSIXct("2024-03-31 12:00:01") & roundedDateTime <= as.POSIXct("2024-04-19 12:00:00") ~ "Round 03",
                        roundedDateTime > as.POSIXct("2024-04-19 12:00:01") & roundedDateTime <= as.POSIXct("2024-04-30 11:00:00") ~ "Round 04",
                        roundedDateTime > as.POSIXct("2024-04-30 11:00:01") & roundedDateTime <= as.POSIXct("2024-05-15 12:00:00") ~ "Round 05",
                        roundedDateTime > as.POSIXct("2024-05-15 12:00:01") & roundedDateTime <= as.POSIXct("2024-05-28 14:00:00") ~ "Round 06",
                        TRUE ~ "Round 02"
                )
        ) 

write.csv(recordData, "recordData.csv")
save(recordData, file = "recordData.Rdata")
## ---------------------------------------------------------------------------##


## ---------------------------------------------------------------------------##
load("recordData.RData")
head(recordData)
str(recordData)
unique(recordData$Species)

## checks
## 08/03/2024 DEPLOYMENT
## 08/03/2024 00:00:00 - 18/03/2024 16:00:00 - Round 01
## 18/03/2024 16:00:01 - 31/03/2024 12:00:00 - Round 02
## 31/03/2024 12:00:01 - 19/04/2024 12:00:00 - Round 03
## 19/04/2024 12:00:01 - 30/04/2024 11:00:00 - Round 04
## 30/04/2024 11:00:01 - 15/05/2024 12:00:00 - Round 05
## 15/05/2024 12:00:01 - 28/05/2024 14:00:00 - Round 06 
## 28/05/2024 RETRIEVAL 
foxData <- recordData %>%
        filter(Species == "Bengal Fox") %>%  # Corrected the filter syntax
        mutate(checks = case_when(
                DateTimeOriginal >= as.POSIXct("2024-03-08 00:00:00") & DateTimeOriginal <= as.POSIXct("2024-03-18 16:00:00") ~ "Round 01",
                DateTimeOriginal > as.POSIXct("2024-03-18 16:00:01") & DateTimeOriginal <= as.POSIXct("2024-03-31 12:00:00") ~ "Round 02",
                DateTimeOriginal > as.POSIXct("2024-03-31 12:00:01") & DateTimeOriginal <= as.POSIXct("2024-04-19 12:00:00") ~ "Round 03",
                DateTimeOriginal > as.POSIXct("2024-04-19 12:00:01") & DateTimeOriginal <= as.POSIXct("2024-04-30 11:00:00") ~ "Round 04",
                DateTimeOriginal > as.POSIXct("2024-04-30 11:00:01") & DateTimeOriginal <= as.POSIXct("2024-05-15 12:00:00") ~ "Round 05",
                DateTimeOriginal > as.POSIXct("2024-05-15 12:00:01") & DateTimeOriginal <= as.POSIXct("2024-05-28 14:00:00") ~ "Round 06",
                TRUE ~ "Round 02"  # This handles any dates/times that fall outside the specified ranges|)
        ))
head(foxData)


## make detection dataframe
# detection = recordData %>%
#         filter(Species == "Bengal Fox") %>%
#         # Extract the date from DateTimeOriginal and add it as a new column
#         # Group by 'checks' and 'DateOnly'
#        group_by(checks) %>%
#         group_by(Date, checks)%>%
#         # Count the number of detections per day for each check
#         summarise(detection_count = n(), .groups = "drop") %>%
#         mutate(JulianDay = yday(Date)) %>%
#         mutate(Date = as.Date(Date))

detection <- foxData %>%
        filter(Species == "Bengal Fox") %>%
        group_by(Date, checks) %>%
        summarise(detection_count = n(), .groups = "drop") %>%
        mutate(
                Date = as.Date(Date, format = "%d-%m-%Y"), 
                JulianDay = yday(Date))%>%
        mutate(Date = format(Date,"%d-%m-%Y"))

detection[16:17,]
head(detection)
head(foxData)


## make detection dataframe
## add two more covariates
## check today (1/0)
## days after the last check


# Step 1: Identify actual check dates (first day of each check round)
check_days <- detection %>%
        group_by(checks) %>%
        summarise(check_date = min(Date), .groups = "drop")

# Step 2: Join this info back to the main data to flag check days
detection <- detection %>%
        left_join(check_days, by = "checks") %>%
        mutate(check_today = if_else(Date == check_date, 1, 0))
head(detection)


# Step 3: Calculate days since last check
detection <- detection %>%
        mutate(Date = as.Date(Date, format = "%d-%m-%Y")) %>%  # fix type!
        arrange(Date) %>%
        mutate(
                last_check_date = if_else(check_today == 1, Date, NA_Date_)  # now works
        ) %>%
        fill(last_check_date, .direction = "down") %>%
        mutate(days_since_last_check = as.integer(Date - last_check_date) + 1) 


# Step 4: Calculate counts of human and monitor detections
covariates <- recordData %>%
        filter(Species %in% c("Human", "Bengal Monitor")) %>%
        mutate(checks = case_when(
                DateTimeOriginal >= as.POSIXct("2024-03-08 00:00:00") & DateTimeOriginal <= as.POSIXct("2024-03-18 16:00:00") ~ "Round 01",
                DateTimeOriginal > as.POSIXct("2024-03-18 16:00:01") & DateTimeOriginal <= as.POSIXct("2024-03-31 12:00:00") ~ "Round 02",
                DateTimeOriginal > as.POSIXct("2024-03-31 12:00:01") & DateTimeOriginal <= as.POSIXct("2024-04-19 12:00:00") ~ "Round 03",
                DateTimeOriginal > as.POSIXct("2024-04-19 12:00:01") & DateTimeOriginal <= as.POSIXct("2024-04-30 11:00:00") ~ "Round 04",
                DateTimeOriginal > as.POSIXct("2024-04-30 11:00:01") & DateTimeOriginal <= as.POSIXct("2024-05-15 12:00:00") ~ "Round 05",
                DateTimeOriginal > as.POSIXct("2024-05-15 12:00:01") & DateTimeOriginal <= as.POSIXct("2024-05-28 14:00:00") ~ "Round 06",
                TRUE ~ "Round 02"  # This handles any dates/times that fall outside the specified ranges|)
        )) %>%
        mutate(Date = as.Date(Date, format = "%d-%m-%Y")) %>%
        group_by(Date, checks, Species) %>%
        summarise(species_count = n(), .groups = "drop") %>%
        pivot_wider(names_from = Species, values_from = species_count, values_fill = 0) %>%
        rename(
                human_count = Human,
                monitor_count = `Bengal Monitor`
        )
detection <- detection %>%
        left_join(covariates, by = c("Date", "checks"))
detection$human_count[is.na(detection$human_count)] <- 0
detection$monitor_count[is.na(detection$monitor_count)] <- 0

# Step 5: Checking
# detection_count should be totalled to 4502 (fox total detection count)
detection_merged <- detection %>%
        group_by(JulianDay) %>%
        summarise(
                detection_count = sum(detection_count, na.rm = TRUE),
                check_today = max(check_today),  # Assuming if a check was done, it's marked 1
                days_since_last_check = max(days_since_last_check),
                checks = paste(unique(checks), collapse = ", "),  # Combine round names (e.g., Round 02, Round 03)
                .groups = 'drop'
        )

str(detection)
head(detection)
str(detection)
## ---------------------------------------------------------------------------##


## ---------------------------------------------------------------------------##
library("mgcv")
library("corrplot") 
library("ggpubr")
library("gratia")
library("purrr")


## checking explanatory covariates
## correlation and VIF
det = detection %>% dplyr::select(JulianDay,
                                  days_since_last_check, 
                                  human_count, 
                                  monitor_count) 
cordet = cor(det) # now, run it
colnames(cordet) = colnames(det)
rownames (cordet) = colnames(det)
## NOTE: NO correlation 

cordet = MASS::ginv(cor(det)) ## ginv is a function of MASS
colnames(cordet) = colnames(det)
rownames (cordet) = colnames(det) 
corrplot(corr = cordet, method = "number", is.corr= F)
## NOTE: VIF OK

## check distribution of the dependent variable
## normality test
ggdensity(detection$detection_count)
car::qqPlot(detection$detection_count)
shapiro.test(detection$detection_count)
## NOTE: Not normal, naturally

## checking zero-inflation 
100*sum(detection$detection_count == 0)/nrow(detection)
ggplot(detection, aes(detection_count))+
        geom_histogram(binwidth=5, position = "dodge") + 
        xlab("Number of incidents")+
        theme_classic()
## NOTE: No zero inflation, naturally

## checking mean-variance ratio the nature of the response variable  
summary(detection$detection_count)
detection$detection_count = as.numeric(detection$detection_count)
var(detection$detection_count)
var(detection$detection_count)/mean(detection$detection_count)
?mgcv::gam()
## NOTE: negative binomial should be the distribution


## MODEL RUN
## detection_count ~ JulianDay + days_since_last_check +
##                   human_count + monitor_count + check_today
knots = list(JulianDay = c(0.5, 365.5), 
             days_since_last_check = c(0.5, 14.5)) 
#knots account for the curves in the  smooths
model1 =  gam(detection_count ~ 
                      s(JulianDay, bs = "cr") + 
                      s(days_since_last_check, bs = "cr") +
                      human_count + 
                      monitor_count +
                      as.factor(check_today), 
                 method = 'REML', 
                 # knots = knots, 
                 family = nb, 
                 data = detection)

model2 =  gam(detection_count ~ 
                      s(JulianDay, bs = "cr") + 
                      s(days_since_last_check, bs = "cr") +
                      human_count + 
                      monitor_count 
                      # as.factor(check_today)
              , 
              method = 'REML', 
              # knots = knots, 
              family = nb, 
              data = detection)

model3 =  gam(detection_count ~ 
                      s(JulianDay, bs = "cr") + 
                      s(days_since_last_check, bs = "cr") +
                      human_count 
              # monitor_count 
              # as.factor(check_today)
              , 
              method = 'REML', 
              # knots = knots, 
              family = nb, 
              data = detection)

model4 =  gam(detection_count ~ 
                      s(JulianDay, bs = "cr") + 
                      s(days_since_last_check, bs = "cr") 
              # human_count 
              # monitor_count 
              # as.factor(check_today)
              , 
              method = 'REML', 
              # knots = knots, 
              family = nb, 
              data = detection)

model5 =  gam(detection_count ~ 
                      s(JulianDay, bs = "cr")  
              # s(days_since_last_check, bs = "cc") 
              # human_count 
              # monitor_count 
              # as.factor(check_today)
              , 
              method = 'REML', 
              # knots = knots, 
              family = nb, 
              data = detection)

model6 =  gam(detection_count ~ 1
              # s(JulianDay, bs = "cc")  
              # s(days_since_last_check, bs = "cc") 
              # human_count 
              # monitor_count 
              # as.factor(check_today)
              , 
              method = 'REML', 
              # knots = knots, 
              family = nb, 
              data = detection)

GAM = list("model1" = gam(detection_count ~ 
                                  s(JulianDay, bs = "cr") + 
                                  s(days_since_last_check, bs = "cr") +
                                  human_count + 
                                  monitor_count +
                                  as.factor(check_today), 
                          method = 'REML', 
                          #knots = knots, 
                          family = nb, 
                          data = detection),
           
           "model2" =  gam(detection_count ~ 
                                  s(JulianDay, bs = "cr") + 
                                  s(days_since_last_check, bs = "cr") +
                                  human_count + 
                                  monitor_count 
                          # as.factor(check_today)
                          , 
                          method = 'REML', 
                          #knots = knots, 
                          family = nb, 
                          data = detection),
           
           "model3" = gam(detection_count ~ 
                                  s(JulianDay, bs = "cr") + 
                                  s(days_since_last_check, bs = "cr") +
                                  human_count 
                          # monitor_count 
                          # as.factor(check_today)
                          , 
                          method = 'REML', 
                          #knots = knots, 
                          family = nb, 
                          data = detection),
           
           "model4" = gam(detection_count ~ 
                                  s(JulianDay, bs = "cr") + 
                                  s(days_since_last_check, bs = "cr") 
                          # human_count 
                          # monitor_count 
                          # as.factor(check_today)
                          , 
                          method = 'REML', 
                          #knots = knots, 
                          family = nb, 
                          data = detection),
           
           "model5" = gam(detection_count ~ 
                                  s(JulianDay, bs = "cr")  
                          # s(days_since_last_check, bs = "cc") 
                          # human_count 
                          # monitor_count 
                          # as.factor(check_today)
                          , 
                          method = 'REML', 
                          #knots = knots, 
                          family = nb, 
                          data = detection),
           
           "model6" = gam(detection_count ~ 1
                          # s(JulianDay, bs = "cc")  
                          # s(days_since_last_check, bs = "cc") 
                          # human_count 
                          # monitor_count 
                          # as.factor(check_today)
                          , 
                          method = 'REML', 
                          #knots = knots, 
                          family = nb, 
                          data = detection),
           
           "model7" = gam(detection_count ~ 
                          s(JulianDay, bs = "cc") + 
                          s(days_since_last_check, bs = "cc") +
                          # human_count 
                          monitor_count 
                          # as.factor(check_today)
                          , 
                          method = 'REML', 
                          #knots = knots, 
                          family = nb, 
                          data = detection))

## model relative fit
modelsummary::modelsummary(GAM$model1) 
AIC(GAM$model1, GAM$model2, GAM$model3, 
    GAM$model4, GAM$model5, GAM$model6, GAM$model7)
purrr::map_dbl(GAM, AIC)
purrr::map_dbl(GAM, BIC)
summary(GAM$model7)


anova(model1, model2, test = "LRT")
anova(GAM$model4, GAM$model1, test = "LRT")
anova(GAM$model5, GAM$model1, test = "LRT")
anova(model4, model5, test = "LRT")

performance::check_overdispersion(model5)
performance::check_zeroinflation(model5)

summary(GAM$model1)
summary(gam_model)$pTerms.df
summary(model2)
summary(model3)
summary(model4)
summary(model5)
plot(model5)
gam.check(model2, pages =1 )
gam.check(GAM$model1)
gratia::draw(GAM$model1)
log(-0.92)


## estimating 95% confidence intervals
3.67 - 1.96*0.12
3.67 + 1.96*0.12

- 0.86 - 1.96*0.43 
- 0.86 + 1.96*0.43 

0.01 - 1.96*0.01
0.01 + 1.96*0.01

0.01 - 1.96*0.06
0.01 + 1.96*0.06


## PLOTTING
# Extract smooth estimates and add 95% CI
smooths(GAM$model1)
sm1 <- smooth_estimates(GAM$model1, select = "s(JulianDay)") %>%
    add_confint()
p1 = ggplot(sm1, aes(x = JulianDay, y = .estimate)) +
    geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci),
                alpha = 0.3, fill = "#E1BE6A") +
    geom_line(color = "#E1BE6A", linewidth = 1.2) +
    labs(x = "Julian day", y = "partial effect",
         title = "", tags = "A") +
    theme_classic2(base_family = "Times") +
    theme(
        axis.title = element_text(size = 14, color = "gray40", face = "bold"),
        axis.text = element_text(size = 14, color = "gray40"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_text(vjust = -0.01),
        plot.tag = element_text(size = 16, color = "black", face = "bold")
    )

sm2 <- smooth_estimates(GAM$model1, select = "s(days_since_last_check)") %>%
    add_confint()
p2 = ggplot(sm2, aes(x = days_since_last_check, y = .estimate)) +
    geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci),
                alpha = 0.3, fill = "#E1BE6A") +
    geom_line(color = "#E1BE6A", linewidth = 1.2) +
    labs(x = "days since last check", y = "partial effect",
         title = "", tag = "B") +
    theme_classic2(base_family = "Times") +
    theme(
        axis.title = element_text(size = 14, color = "gray40", face = "bold"),
        axis.text = element_text(size = 14, color = "gray40"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_text(vjust = -0.01),
        plot.tag = element_text(size = 16, color = "black", face = "bold")
    )

library(patchwork)
final = p1 + p2; final
ggsave("gam_partial_effects.png", plot = final, 
       width = 10, height = 5, dpi = 300)
