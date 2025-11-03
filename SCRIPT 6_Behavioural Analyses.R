####################  Bengal Fox Denning Behaviour  ############################
################               Bangladesh             ##########################
################               script by              ##########################
################             Muntasir Akash           ##########################
################         classifying behaviours        #########################



## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##
## libraries
library("tidyverse")   ## data wrangling
library("dplyr")       ## data wrangling
library("stringr")     ## data wrangling
library("lubridate")   ## date time wrangling    
library("hms")         ## date time wrangling
library("bipartite")   ## network analyses
library("tidygraph")   ## network analyses
library("ggraph")   ## network analyses
library("igraph")   ## network analyses
library("ggplot2")     ## plotting
library("reshape2")    ## plotting
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
load("BFox_cleaned_Apr20.RData")
# Data = read.csv(file = "Fox_MA_edits.csv", header = T, row.names = NULL)
## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##



## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##
## STEP ONE: MINOR DATA CLEANING
## 1.1 Excluding "Blank"
## 1.2 Generating data summary [hours of footage]
## 1.3 Selecting Bengal Fox only detection 
## 1.4 Generating data summary only for foxes
## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##


#### 1.1__________________________________________________________________#######
## REMOVING species rows that are exactly "Blank"
table(BFox$Species)
# NOTE: should match with the information provided in Table 02
# cross-check, if necessary
BFox = BFox[!is.na(BFox$Species), ]
#######__________________________________________________________________#######


#### 1.2__________________________________________________________________#######
## Generating data summary
## CALCULATING the total survey day and hours of footage
## and total number of footage obtained 
str(BFox$Time)
str(BFox$Date)
str(BFox$VisitLength)

# hours of footage
sum(BFox$VisitLength)
mean(BFox$VisitLength)
sd(BFox$VisitLength)
# 43866/3600
# 12.185 hours of footage

# calendar days camera-trap was active
length(unique(BFox$Date))
# 58 days 
# NOTE: number of active camera-trap night = 54

nrow(BFox)
# 4641 footage 
#######__________________________________________________________________#######


#### 1.3__________________________________________________________________#######
## Selecting Bengal Fox only detection 
BFoxBhav = BFox[(BFox$Species %in% c("Bengal Fox",
                                      "Bengal Fox and Jungle Babbler",
                                      "Bengal Fox and Bengal Monitor",
                                      "Bengal Monitor")),]
# is there still any NA?
sum(is.na(BFoxBhav$Species))

# select footage where fox is present
BFoxBhav = BFoxBhav %>% filter(!(BFoxBhav$Species == "Bengal Monitor" 
                                  & BFoxBhav$FoxN == 0
                                 ))
# NOTE: should match the number provided in Table 2
# cross-check if necessary
#######__________________________________________________________________#######


#### 1.4__________________________________________________________________#######
## Generating data summary only for foxes
# hours of footage
sum(BFoxBhav$VisitLength)
# 42317/3600
# 11.75 hours of footage

# calendar days camera-trap was active
length(unique(BFoxBhav$Date))
# 56 days 
# NOTE: number of active camera-trap night = 54

nrow(BFox)
# 4502 footage 
#######__________________________________________________________________#######
## STEP ONE ends



## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##
## STEP TWO: MINOR STANDARDIZATION OF BEHAVIOURAL DATA
## 2.1 Excluding "Blanks" and "Detection" only footage 
##     Fixing minor typo
## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##


#### 2.1__________________________________________________________________#######
## Excluding "Blanks" and "Detection" only footage 

# remove "Blank," from combined behavior strings
unique(BFoxBhav$FOBehav_combined)
unique(BFoxBhav$FOBehav_Single)
BFoxBhav$FOBehav_combined = gsub("Blank,", "", BFoxBhav$FOBehav_combined)
unique(BFoxBhav$FOBehav_combined)

# NOTE: otherwise follow these codes

# BFoxBhav = BFoxBhav[!(BFoxBhav$FOBehav_Single %in% c("Blank")), ]
# BFoxBhav = BFoxBhav[!(BFoxBhav$FOBehav_combined %in% c("Blank")), ]
# BFoxBhav = BFox[!(BFox$FOBehav_Single %in% c("Blank", "Detection")), ]

#  remove "Detection" from single behavior strings
#  NOTE: how many
BFoxBhav %>% filter(FOBehav_combined == "Detection") %>% count()
BFoxBhav %>% filter(FOBehav_combined == "") %>% count()
BFoxBhav = BFoxBhav[!(BFoxBhav$FOBehav_combined == "Detection"), ]
unique(BFoxBhav$FOBehav_combined)

# remove "Detection," from combined behavior strings
BFoxBhav$FOBehav_combined = gsub("Detection,", "", BFoxBhav$FOBehav_combined)
unique(BFoxBhav$FOBehav_combined)
# NOTE: total footage number will not decrease

# one silly typo
BFoxBhav = BFoxBhav %>%
        mutate(FOBehav_combined = str_replace_all(FOBehav_combined,
                                                  "Restng",
                                                  "Resting"))
unique(BFoxBhav$FOBehav_combined)

## Optional: Also clean up trailing commas or whitespace, if any
# BFoxBhav$FOBehav_combined = gsub(",$", "", BFoxBhav$FOBehav_combined)
# BFoxBhav$FOBehav_combined = trimws(BFoxBhav$FOBehav_combined)
# case1 = BFoxBhav %>% filter(FOBehav_combined == "")

# NOTE:
# The current state of the dataframe tells us about all detections
# during the survey period. 
# The "" in the BFoxBhav$FOBehav_Single corresponds to Inter/Intra-species 
# interaction
# Undefinable will be food-caching behavior

BFoxBhav = BFoxBhav %>% mutate(FOBehav_combined = if_else(
        str_detect(FOBehav_combined, "Undefinable"), "Food-Caching", 
        FOBehav_combined))  %>%
        mutate(FOBehav_combined = str_replace_all(FOBehav_combined,
                                                  "\\bGrooming\\b", "Self-Grooming"))
unique(BFoxBhav$FOBehav_combined)
#######__________________________________________________________________#######
## STEP TWO ends



## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##
## STEP THREE: GENERAING SUMMARY OF BEHAVIOURAL BOUTS
## 3.1 Summary of clip lengths, no. of clips
## 3.2 Sorting considering EndofVisit = "Yes"
## 3.3 Ratio between Single and Combined Behaviour Detection 
## 3.4 Relationship between VisitLength and No. of Behaviour Detected    
## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##


#### 3.1__________________________________________________________________#######
## Summary of clip lengths, no. of clips

# create a column cliplengthbin
BFoxBhav$ClipLengthBin = cut(
        BFoxBhav$VisitLength,
        breaks = c(0, 5, 10, 15, 20, 30, 60),
        labels = c("01–05s", "06–10s", "11–15s", "16–20s", "21–30s", "31–60s"),
        right = TRUE, include.lowest = TRUE)

# plotting
ggplot(BFoxBhav, aes(x = ClipLengthBin)) +
        geom_bar(fill = "#69b3a2", color = "black", alpha = 0.5) +
        theme_minimal() +
        labs(x = "clip length (seconds)",
             y = "no. of  clips") +
        my_custom_theme

table(BFoxBhav$ClipLengthBin)

mean(BFoxBhav$VisitLength)
sd(BFoxBhav$VisitLength)


# no. of clips over day
# NOTE: important to show the impact of camera-trapping on detectability
# and developing camera shyness 
BFox_per_day = BFoxBhav %>%
        group_by(Date) %>%
        summarise(n_clips = n())

ggplot(BFox_per_day, aes(x = Date, y = n_clips)) +
        geom_col(fill = "#4F81BD", alpha = 0.5) +
        theme_minimal() +
        labs(x = "survey Day", y = "no. of clips",
             title = "camera-trap detections per survey day") +
        my_custom_theme
# NOTE: This is too messy. There are gaps in weeks.


# no. of clips over months
BFoxBhav =  BFoxBhav%>%
        mutate(Date = as.Date(Date),
               Month = format(Date, "%B"))

# calculate survey days per month
survey_days_per_month = BFoxBhav %>%
        distinct(Date, Month) %>%
        count(Month, name = "SurveyDays")

# calculate clips per month 
clips_per_month = BFoxBhav %>%
        count(Month, name = "ClipCount")

# calculate clips per day for each month
daily_clip_data = BFoxBhav %>%
        group_by(Date, Month) %>%
        summarise(n_clips = n(), .groups = "drop") %>%
        left_join(survey_days_per_month, by = "Month") %>%
        mutate(MonthLabel = paste0(Month, " (", SurveyDays, 
                                   " days)"))
# plotting: histogram
plot_data =  left_join(clips_per_month, 
                       survey_days_per_month, 
                       by = "Month") %>%
        mutate(MonthLabel = paste0(Month, 
                                   " (", SurveyDays, " days)"))

# for histogram
plot_data$MonthLabel = factor(plot_data$MonthLabel, 
                              levels = plot_data$MonthLabel
                              [order(match(plot_data$Month, 
                                           c("March", "April", "May")))])

# plotting
ggplot(plot_data, aes(x = MonthLabel, y = ClipCount)) +
        geom_col(fill = "#4F81BD", alpha = 0.5) +
        theme_minimal() +
        labs(x = "", y = "no. of clips",
             title = "monthly variation in camera-trap detections") +
        my_custom_theme

# box plot
ggplot(daily_clip_data, aes(x = forcats::fct_relevel(MonthLabel, 
                                                     "March (16 days)", 
                                                     "April (20 days)", 
                                                     "May (20 days)"), 
                            y = n_clips)) +
        geom_boxplot(fill = "steelblue", alpha = 0.5, outlier.shape = NA) +
        geom_jitter(width = 0.2, alpha = 0.5, size = 1, color = "brown") +
        theme_minimal() +
        labs(x = "", y = "no. of clips / day",
             title = "monthly variation in camera-trap detections") +
        my_custom_theme

# violin plot
ggplot(daily_clip_data, aes(x = forcats::fct_relevel(MonthLabel, 
                                                     "March (16 days)", 
                                                     "April (20 days)", 
                                                     "May (20 days)"), 
                            y = n_clips)) +
        geom_violin(fill = "steelblue", alpha = 0.4) +
        geom_jitter(width = 0.2, alpha = 0.6, size = 1, color = "brown") +
        theme_minimal(base_family = "Times New Roman", base_size = 14) +
        labs(x = "", y = "no. of clips / day",
             title = "monthly variation in camera-trap detections")+
        my_custom_theme
#######__________________________________________________________________#######


#### 3.2__________________________________________________________________#######
## Filtering 
BFoxBhav =  BFoxBhav %>% filter(EndofVisit == "Yes")

# NOTE: this might sound like a unsuitable sorting method because one fox might
# be ending its behaviour while another one is still exhibiting something
# However, EndofVisit == "Yes" was considered only when all foxes in a footage
# left the camera-trap frame

# BFoxBhav %>% filter(EndofVisit == "Yes") %>% count()
# BFoxBhav =  BFoxBhav%>% filter(EndofVisit == "Yes") 
# BFoxBhav = BFoxBhav %>% mutate(No.Behav = sapply(strsplit(FOBehav_combined, ","), 
#                                 length))

# NOTE: You can check without considering EndofVist = "Yes
# repeat the process ignoring the step 3.2
#######__________________________________________________________________#######


#### 3.3__________________________________________________________________#######
## Ratio

# classify as 'Single' or 'Combined' detections
BFoxBhav = BFoxBhav %>%
        mutate(BehaviorType = ifelse(grepl(",", FOBehav_combined), "Combined", "Single"))

# count occurrences
behav_summary = BFoxBhav %>%
        group_by(BehaviorType) %>%
        summarise(Count = n()) %>%
        mutate(Percentage = round(100 * Count / sum(Count), 1)); behav_summary

# plotting
ggplot(behav_summary, aes(x = BehaviorType, y = Count, fill = BehaviorType)) +
        geom_bar(stat = "identity", color = "black", alpha = 0.5) +
        geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5) +
        theme_minimal() +
        labs(title = "single vs combined behaviours in clips",
             x = "behaviour type",
             y = "number of clips",
             fill = "") + # sets legend title
        scale_fill_manual(values = c("skyblue", "tomato"),
                          labels = c(c("single behaviour", "combined behaviours"))) +
        ylim(0, max(behav_summary$Count) * 1.1) +
        my_custom_theme

table(BFoxBhav$FOBehav_combined)
#######__________________________________________________________________#######


#### 3.4__________________________________________________________________#######
## Relationship VisitLength vs No. of Behaviour
unique(BFoxBhav$FOBehav_combined)

# count different behaviours
BFoxBhav = BFoxBhav %>%
                    mutate(No.Behav = sapply(strsplit(FOBehav_combined, ","), 
                                             length))
unique(BFoxBhav$No.Behav)

# correlation
cor(BFoxBhav$No.Behav, BFoxBhav$VisitLength)
# 0.4255

# plotting
ggplot(BFoxBhav, aes(x = No.Behav, y = VisitLength)) +
        geom_point(color = "steelblue", alpha = 0.5) +
        geom_smooth(method = "lm", color = "red", se = FALSE, alpha = 0.5) +
        theme_minimal() +
        labs(x = "no. of detected behavior type", 
             y = "visit length (s)",
             title = "no. of detected behavior type vs visit length (s)") +
        my_custom_theme

# plotting # percentage barchart
ggplot(BFoxBhav, aes(x = ClipLengthBin, fill = as.factor(No.Behav))) +
        geom_bar(position = "fill", color = "white", alpha = 0.5) +
        theme_minimal() +
        labs(title = "maximum numer of behaviour detected per footage",
             x = "footage length (s)",
             y = "proportion of footage",
             fill = "") +
        scale_y_continuous(labels = scales::percent_format()) +
        my_custom_theme + 
        guides(fill = guide_legend(nrow = 1))

# NOTE: In this way, three behaviors were observed in combo AT MOST
# max 3 behaviours detected in a single frame
#######__________________________________________________________________#######
## STEP THREE ends


## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##
## STEP FOUR: GENERAING SUMMARY OF MAX NUMBER OF FOX DETECTED IN A FOOTAGE
## 4.1 Summary of fox numbers
## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##


#### 4.1__________________________________________________________________#######
## Fox Number Summary
table(BFoxBhav$FoxN)
mean(BFoxBhav$FoxN)
sd(BFoxBhav$FoxN)

# correlation
cor_VisitLength_FoxN = cor(BFoxBhav$VisitLength, BFoxBhav$FoxN, use = "complete.obs")
cor_VisitLength_NoBehav = cor(BFoxBhav$VisitLength, BFoxBhav$No.Behav, use = "complete.obs")
cor_FoxN_NoBehav = cor(BFoxBhav$FoxN, BFoxBhav$No.Behav, use = "complete.obs")

cor_results = data.frame(
        Variable_Pair = c("VisitLength vs FoxN", 
                          "VisitLength vs No.Behav", 
                          "FoxN vs No.Behav"),
        Correlation = c(cor_VisitLength_FoxN, 
                        cor_VisitLength_NoBehav, 
                        cor_FoxN_NoBehav))
cor_results

# plotting # percentage barchart
ggplot(BFoxBhav, aes(x = ClipLengthBin, fill = as.factor(FoxN))) +
        geom_bar(position = "fill", color = "white", alpha = 0.5) +
        theme_minimal() +
        labs(title = "maximum numer of fox detected per footage",
             x = "footage length (s)",
             y = "proportion of footage",
             fill = "") +
        scale_y_continuous(labels = scales::percent_format()) +
        my_custom_theme + 
        guides(fill = guide_legend(nrow = 1))

unique(BFoxBhav$Species)
#######__________________________________________________________________#######
## STEP FOUR ends



## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##
## STEP FIVE: GENERAING SUMMARY OF INTRA-SPECIES, INTER-SPECIES and FEEDING
## 5.1 Summary of these three behaviours
## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##
table(BFoxBhav$Inter_Species)
table(BFox$Inter_Species)

table(BFoxBhav$Intra_Species)
table(BFox$Intra_Species)

table(BFoxBhav$Feeding)
table(BFox$Feeding)

table(BFoxBhav$FOBehav_combined)
#######__________________________________________________________________#######
## STEP FIVE ends



# save(BFoxBhav, file = "Behav_filtered.Rdata")
# rm(list = ls())
# load("Behav_filtered.Rdata")



## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##
## STEP SIX: MODULARITY ANALYSES
## 6.1 Basic Checking
## 6.2 Adjacency Matrix
## 6.3 Modularity Analysis
## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##


#### 6.1__________________________________________________________________#######
## Basic Checking
nrow(BFoxBhav)
table(BFoxBhav$Species)
table(BFoxBhav$FoxN)
table(BFoxBhav$Intra_Species)
table(BFoxBhav$Inter_Species)

unique(BFoxBhav$FOBehav_combined)
unique(BFoxBhav$FOBehav_combined)[!grepl(",", unique(BFoxBhav$FOBehav_combined))]
unique(BFoxBhav$Intra_Species)[!grepl(",", unique(BFoxBhav$Intra_Species))]
unique(BFoxBhav$Inter_Species)[!grepl(",", unique(BFoxBhav$Inter_Species))]
unique(BFoxBhav$Intra_Species)

BFoxBhav %>% filter(FOBehav_combined == "Passing") %>% count()
#######__________________________________________________________________#######


#### 6.2__________________________________________________________________#######
## Adjacency Matrix

#  combine the columns row-wise
combined_behaviors = apply(BFoxBhav[, c("FOBehav_combined", 
                                         "Intra_Species", 
                                         "Inter_Species")], 1, 
                            function(x) {x = na.omit(x)                       # Remove NAs
                            x = unique(unlist(strsplit(x, ",\\s*")))   # split multi-behaviors 
                            # (if comma-separated)
                            paste(x, collapse = ",")                    # recombine into a string
                            })

#  all behaviors across all rows
all_behavs = unique(unlist(strsplit(combined_behaviors, ",\\s*")))
str(all_behavs)

# producing adjacency matrix 
combined_behaviors = apply(BFoxBhav[, c("FOBehav_combined",
                                         "Intra_Species",
                                         "Inter_Species")], 1, function(x) {
                                                 x = na.omit(x)
                                                 x = unlist(strsplit(x, ",\\s*"))
                                                 unique(x)})

#  get all pairwise combinations per event
behavior_pairs = do.call(rbind, lapply(combined_behaviors, function(bh) {
        if (length(bh) < 2) return(NULL)
        combn(bh, 2, simplify = FALSE)}))

#  convert to dataframe
edge_df = do.call(rbind, lapply(behavior_pairs, 
                                 function(x) data.frame(From = x[1], 
                                                        To = x[2])))
#  count frequencies
edge_df = edge_df %>%
        group_by(From, To) %>%
        summarise(weight = n(), .groups = "drop")

#  make sure From and To are factors with same levels
nodes = sort(unique(c(edge_df$From, edge_df$To)))

edge_df = edge_df %>%
        mutate(
                From = factor(From, levels = nodes),
                To   = factor(To,   levels = nodes))

#  create a full matrix filled with zeros
adj_matrix = matrix(0, nrow = length(nodes), ncol = length(nodes),
                     dimnames = list(nodes, nodes))

#  fill in weights (symmetric)
for (i in 1:nrow(edge_df)) {
        f = edge_df$From[i]
        t = edge_df$To[i]
        w = edge_df$weight[i]
        adj_matrix[f, t] = adj_matrix[f, t] + w
        adj_matrix[t, f] = adj_matrix[t, f] + w  # Symmetric
}

#  view
adj_matrix
diag(adj_matrix)
dim(adj_matrix)

# optional: 
# # set threshold
# adj_matrix_thresh = adj_matrix
# threshold = 10
# # apply threshold: keep only entries >= threshold
# adj_matrix_thresh[adj_matrix_thresh < threshold] = 0

# # optionally: drop rows and columns that are now all 0
# nonzero_rows = rowSums(adj_matrix_thresh) > 0
# adj_matrix_thresh = adj_matrix_thresh[nonzero_rows, nonzero_rows]
# rowSums(adj_matrix_thresh) > 0

# filter some rownames and colnames to avoid repeatations
to_remove = c(
        "Passing",
        "Dry-humping",
        "Interacting with human or human-induced disturbance/flight",
        "Interacting with monitor/chasing",
        "Interacting with monitor/flight",
        "Interacting with monitor/stand-off",
        "Nursing",
        "Food-Caching",
        "Greeting",
        "Grooming",
        "Playing (non-territorial fights/chases)",
        "Pooping")

# remove corresponding rows and columns
adj_matrix = adj_matrix[!(rownames(adj_matrix) %in% to_remove),
                        !(colnames(adj_matrix) %in% to_remove)]

# rename "Self-grooming" to "Autogrooming"
colnames(adj_matrix)[colnames(adj_matrix) == "Self-Grooming"] <- "Autogrooming"
rownames(adj_matrix)[rownames(adj_matrix) == "Self-Grooming"] <- "Autogrooming"

# save(adj_matrix_thresh, file = "foxmatrixthresh.Rdata")
# save(adj_matrix, file = "foxmatrix.Rdata")
# load("foxmatrix.Rdata")
# colnames(adj_matrix)
# rownames(adj_matrix)
#######__________________________________________________________________#######


#### 6.3__________________________________________________________________#######
## Modularity Checking

web = adj_matrix                    # thresholded adjacency matrix
mod_obs_obj = computeModules(web)          # S4 object

mod_obs = slot(mod_obs_obj, "likelihood")  # extract log-likelihood
# mod_obs = mod_obs_obj@likelihood

# simulation run
nulls = vaznull(N = 1000, web)             # be sure N is the first argument
mod_null = sapply(nulls, function(m) slot(computeModules(m), "likelihood"))

# p-value checking
p_value = mean(mod_null >= mod_obs)        # one-tailed test
p_value
mod_obs

# null distribution of modularity
hist(mod_null, breaks = 30, main = "Null Distribution of Modularity",
     xlab = "Modularity (log-likelihood)")
abline(v = mod_obs, col = "red", lwd = 2)

mean(mod_null)
sd(mod_null)
mod_obs
#######__________________________________________________________________#######
## STEP SIX ends



## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##
## STEP SEVEN: PLOTTING MODULARITY MATRIX
## 7.1 Plotting
## ---------------------------------------------------------------------------##
## ---------------------------------------------------------------------------##


#### 7.1__________________________________________________________________#######
## Plotting

# convert matrix to numeric (if needed)
adj_numeric = as.matrix(adj_matrix)
storage.mode(adj_numeric) = "numeric"

# calculate occurrence (total outgoing interactions per behavior)
occurrence = rowSums(adj_numeric)

# convert matrix to tidygraph
graph_tbl = tidygraph::as_tbl_graph(adj_numeric, directed = TRUE) %>%
        mutate(behavior = name)

# categorize edge weights
graph_tbl = graph_tbl %>%
        activate(edges) %>%
        mutate(weight_category = case_when(
                weight < quantile(weight, 0.33) ~ "Low",
                weight < quantile(weight, 0.66) ~ "Medium",
                TRUE ~ "High"
        ))

# show all edges
graph_tbl %>% 
        activate(edges) %>% 
        as_tibble() %>% 
        print(n = Inf)

# show all nodes
graph_tbl %>% 
        activate(nodes) %>% 
        as_tibble() %>% 
        print(n = Inf)

# plotting
ggraph(graph_tbl, layout = "circle") +
        geom_edge_link(aes(width = weight, color = weight_category),
                       end_cap = circle(1, 'mm'), alpha = 0.5,
                       show.legend = FALSE,
                       position = "identity",
                       angle_calc = "rot",
                       force_flip = FALSE) +
        geom_node_point(aes(size = occurrence*100),
                        color = "gray10", alpha = 0.7,
                        show.legend = FALSE) +
        geom_node_text(aes(label = behavior, x = x * 1.2, y = y * 1.11),
                       family = "Times New Roman", 
                       size = 4) +
        scale_edge_color_manual(values = c(
                "Low" = "#E69F00",    # orange
                "Medium" = "#56B4E9", # sky blue
                "High" = "#D55E00"    # vermillion
        )) +
        scale_edge_width(range = c(0.2, 2)) +
        scale_size(range = c(4, 10)) +
        theme_graph(base_family = "Times New Roman")
#######__________________________________________________________________#######
## STEP SEVEN ends




# Combine all behavior categories
BFox_summary <- bind_rows(
        BFox %>% 
                filter(!is.na(Intra_Species)) %>%
                group_by(Behavior = Intra_Species) %>%
                summarise(
                        Count = n(),
                        Mean_Duration = mean(VisitLength, na.rm = TRUE),
                        SD_Duration = sd(VisitLength, na.rm = TRUE),
                        Type = "Intra-Species"
                ),
        
        BFox %>% 
                filter(!is.na(Inter_Species)) %>%
                group_by(Behavior = Inter_Species) %>%
                summarise(
                        Count = n(),
                        Mean_Duration = mean(VisitLength, na.rm = TRUE),
                        SD_Duration = sd(VisitLength, na.rm = TRUE),
                        Type = "Inter-Species"
                ),
        
        BFox %>% 
                filter(!is.na(Feeding)) %>%
                group_by(Behavior = Feeding) %>%
                summarise(
                        Count = n(),
                        Mean_Duration = mean(VisitLength, na.rm = TRUE),
                        SD_Duration = sd(VisitLength, na.rm = TRUE),
                        Type = "Feeding"
                )
)

# View the table
print(BFox_summary, n = Inf)
write.csv(BFox_summary, "summary.csv")
