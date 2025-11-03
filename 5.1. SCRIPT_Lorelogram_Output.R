## ---------------------------------------------------------------------------##
## CHECK lorelogram 

## dependencies
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ezknitr)
library(knitr)
library(tictoc)
library(data.table)
library(lorelogram)
library(ggpubr)

## INSTALLING lorelogram
# knitr::opts_chunk$set(fig.width=10, fig.height=5, message = FALSE) # figures setting
# Load lorelogram package
# if (!require("devtools")) install.packages("devtools", 
#                                             repos = "http://cran.us.r-project.org", 
#                                             dependencies = "Imports")
# devtools::install_github("FabiolaIannarilli/lorelogram") 
## ---------------------------------------------------------------------------##

rm(list = ls())

## ---------------------------------------------------------------------------##
## load data 
BFox_minute <- as.data.frame(fread("/Users/lynx025/Desktop/BengalFox_analyses/5.1 Activity_Lorelogram_Minute Level/DetHist_BFox_minute.csv",
                                   header = TRUE))
load("DetHist_BFox_minute.RData")
load("LORnew.RData")
## ---------------------------------------------------------------------------##


## ---------------------------------------------------------------------------##
## run lorelogram ## log-odds ratio up to 360 minute
## ?lor_lag_to_indep
tic() # for timing code
BF_lor <- lorelogram(BFox_minute, max_lag = 180, plot_LOR = FALSE)
plot_LOR_minute <- lor_plot(BF_lor, colour =  "#F6511D", title = "a)", 
                            x_axis_title = "Time Lag (Minute)", ylim = c(-1,10));
plot_LOR_minute
toc()
lor_lag_to_indep(BF_lor)
str(BF_lor)
## ---------------------------------------------------------------------------##


## ---------------------------------------------------------------------------##
## run lorelogram ## log-odds ratio up to 180 minute
## ?lor_lag_to_indep
tic() # for timing code
BF_lor360 <- lorelogram(BFox_minute, max_lag = 360, plot_LOR = FALSE)
plot_LOR_minute <- lor_plot(BF_lor360, colour =  "#F6511D", title = "a)", 
                            x_axis_title = "Time Lag (Minute)", ylim = c(-1,10)) 
toc()
lor_lag_to_indep(BF_lor360)
str(BF_lor)
save(BF_lor, BF_lor360, file = "LORnew.Rdata")
# load("LOR.Rdata")
## ---------------------------------------------------------------------------##

## ---------------------------------------------------------------------------##
## plotting ## replace BF_lor with BF_lor360 if necessary
my_custom_theme = theme(
        text = element_text(family = "Times New Roman"),
        axis.line = element_line(colour = 'black', linetype = 'solid'),
        # axis.ticks = element_line(colour = 'black', linetype = 'solid'),
        axis.text =  element_text(size = 16, color = "gray40"),
        axis.title = element_text(size = 16, color = "black"),
        axis.title.x = element_text(vjust = -0.01),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        # panel.grid.major.x = element_line(colour = 'lightgrey', linetype = 'dashed', linewidth = 0.5),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 14, face = "bold")
)



ggplot(BF_lor360, aes(x = Lag, y = LORs)) +
        geom_line(color = "blue") +  # Plot the log-odds ratio as a line
        geom_ribbon(aes(ymin = L_95_CI, ymax = U_95_CI), alpha = 0.2) +  # Add the confidence intervals as a shaded ribbon
        labs(x = "time lag (minute)", y = "log-odds ratio") +
        theme_classic2() +
        my_custom_theme 
        # theme(
        #         axis.title = element_text(size = 12),
        #         axis.text = element_text(size = 10)
        # )
## ---------------------------------------------------------------------------##


## ---------------------------------------------------------------------------##
## prettier plots ## indicating where the line starts flattening

## define a small tolerance around 1 (e.g., within 0.1 of 1)
tolerance <- 0.1

## find the Lag values where LORs are within the tolerance range of 1
crossings <- BF_lor360[abs(BF_lor360$LORs - 0.5) <= tolerance, ]

## add an indicator column to differentiate the first crossing from others
crossings$LineColor <- c("red", rep("gray50", nrow(crossings) - 1))

## get the first crossing for labeling
first_crossing <- crossings[1, ]

## plotting
lor = ggplot(BF_lor360, aes(x = Lag, y = LORs)) +
        geom_line(color = "steelblue") +  # Plot the log-odds ratio as a line
        geom_ribbon(aes(ymin = L_95_CI, ymax = U_95_CI),fill = "steelblue", alpha = 0.3) +  # Add the confidence intervals as a shaded ribbon
        #geom_vline(data = crossings, aes(xintercept = Lag, color = LineColor), linetype = "dashed") +  # Mark crossings with different colors
        geom_point(data = first_crossing, aes(x = Lag, y = LORs), 
                   color = "red", size = 3, shape = 20, fill = "white") +
        # geom_text(data = first_crossing, aes(x = Lag, y = LORs, label = round(Lag, 2)), 
        #           color = "red", vjust = 2) +  # Add label for the first crossing
        labs(title = "", 
             x = "time lag (minute)", 
             y = "log-odds ratio") +
        scale_color_identity() +  # Ensure the color identity is respected
        theme_classic2 () +
        my_custom_theme +
        theme(
                axis.title = element_text(size = 14, color = "gray40", face = "bold"),
                axis.text = element_text(size = 14, color = "gray40"),
                plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
                axis.title.x = element_text(vjust = -0.01),
                plot.tag = element_text(size = 16, color = "black", face = "bold")
        )
        # theme(
        #         axis.title = element_text(size = 12),
        #         axis.text = element_text(size = 10)
        # )
        # 
ggsave("lor360.png", plot = lor, 
       width = 8, height = 5, dpi = 300)

## ---------------------------------------------------------------------------##



