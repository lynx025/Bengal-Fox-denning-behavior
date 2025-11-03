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
                                 #& BFoxBhav$FoxN == 0
))
# NOTE: should match the number provided in Table 2
# cross-check if necessary
#######__________________________________________________________________#######


#### 1.4__________________________________________________________________#######
## Generating data summary only for foxes
# hours of footage
sum(BFoxBhav$VisitLength)
# 42317/3600
# 12.185 hours of footage

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
table(BFoxBhav$FOBehav_combined)
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
combined_behaviors = apply(BFoxBhav[, c("FOBehav_combined", "Intra_Species", "Inter_Species")], 1, function(x) {
        x = na.omit(x)
        unlist(strsplit(x, ",\\s*"))
})

#  all behaviors across all rows
all_behavs = sort(unique(unlist(combined_behaviors)))
str(all_behavs)

# initialize symmetric matrix with zeros
adj_matrix = matrix(0, nrow = length(all_behavs), ncol = length(all_behavs),
                     dimnames = list(all_behavs, all_behavs))

# count total appearances for each individual behavior (for diagonal)
diag_counts = table(unlist(combined_behaviors))
diag(adj_matrix)[names(diag_counts)] = diag_counts

# generate pairwise co-occurrences (only if row has >1 behavior)
behavior_pairs = lapply(combined_behaviors, function(bh) {
        bh = unique(bh)
        if (length(bh) < 2) return(NULL)
        combn(bh, 2, simplify = FALSE)
})
behavior_pairs = do.call(c, behavior_pairs)

# count pairwise frequencies and fill off-diagonal cells symmetrically
for (pair in behavior_pairs) {
        from = pair[1]
        to = pair[2]
        adj_matrix[from, to] = adj_matrix[from, to] + 1
        adj_matrix[to, from] = adj_matrix[to, from] + 1
}

# View final matrix
adj_matrix

# removing second-order categories
to_remove = c(
        "Dry-humping",
        "Interacting with human or human-induced disturbance/flight",
        "Interacting with monitor/chasing",
        "Interacting with monitor/flight",
        "Interacting with monitor/stand-off",
        "Nursing",
        "Greeting",
        "Grooming",
        "Playing (non-territorial fights/chases)")

adj_matrix = adj_matrix[!(rownames(adj_matrix) %in% to_remove),
                        !(colnames(adj_matrix) %in% to_remove)]


#  view
adj_matrix
diag(adj_matrix)
dim(adj_matrix)

# summing
# diagonal: individual behavior counts
diag_total = sum(diag(adj_matrix))

# off-diagonal: unique pairwise co-occurrences (only once per pair)
off_diag_total = sum(adj_matrix[lower.tri(adj_matrix)])

# grand total = individual behavior occurrences + unique pairwise co-occurrences
grand_total = diag_total + off_diag_total

grand_total

# save
write.csv(adj_matrix, file = "Behavior_Adjacency_Matrix.csv")
#######__________________________________________________________________#######
