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
# BFox_minute <- as.data.frame(fread("/Users/lynx025/Desktop/BengalFox_analyses/5.2 Activity_Lorelogram_Minute Level per Checks/DetHist_BFox_minuteR6.csv",
#                                    header = TRUE))
load("DetHist_Minute_R1_to_R6_Updated.RData")
class(DetHist_Minute_R1)

# Convert each detection history matrix to a data frame
DetHist_Minute_R1 <- as.data.frame(DetHist_Minute_R1)
DetHist_Minute_R2 <- as.data.frame(DetHist_Minute_R2)
DetHist_Minute_R3 <- as.data.frame(DetHist_Minute_R3)
DetHist_Minute_R4 <- as.data.frame(DetHist_Minute_R4)
DetHist_Minute_R5 <- as.data.frame(DetHist_Minute_R5)
DetHist_Minute_R6 <- as.data.frame(DetHist_Minute_R6)

# Now create the list of data frames
DetHist_Minute_List <- list(DetHist_Minute_R1, 
                            DetHist_Minute_R2, 
                            DetHist_Minute_R3, 
                            DetHist_Minute_R4, 
                            DetHist_Minute_R5, DetHist_Minute_R6)

# Check the structure of the list to ensure it's now a list of data frames
print(dim(DetHist_Minute_List[[1]])) 
## ---------------------------------------------------------------------------##


## ---------------------------------------------------------------------------##
## run lorelogram ## log-odds ratio up to 360 minute
## ?lor_lag_to_indep
tic() # for timing code
for (i in 1:6) {
        
        # Get the detection history matrix for the current round
        DetHist <- DetHist_Minute_List[[i]]
        
        # Check the structure of the current matrix
        print(dim(DetHist))  # Should have multiple rows (stations) and columns (occasions)
        
        # Run lorelogram on the ith detection history matrix
        BF_lor <- lorelogram(DetHist, max_lag = 360, plot_LOR = FALSE)
        
        # Assign the BF_lor result to a unique variable for each round
        assign(paste0("BF_lor", i), BF_lor)
        
        # Create a plot for the current round with customized parameters
        plot_LOR_minuteRi <- lor_plot(BF_lor, colour = "#F6511D", title = paste0("a", i), 
                                      x_axis_title = "Time Lag (Minute)", ylim = c(-1, 10))
        
        # Print the plot (if you want it to show within the loop)
        print(plot_LOR_minuteRi)
        
        # Calculate lag-to-independence for the current BF_lor
        lor_lag_to_indep(BF_lor)
        
        # Optionally save the plot to a file with a unique name
        ggsave(paste0("plot_LOR_minuteR", i, ".png"), plot = plot_LOR_minuteRi)
}
toc()
lor_lag_to_indep(BF_lor1)
lor_lag_to_indep(BF_lor2)
lor_lag_to_indep(BF_lor3)
lor_lag_to_indep(BF_lor4)
lor_lag_to_indep(BF_lor5)
lor_lag_to_indep(BF_lor6)
str(BF_lor)
## ---------------------------------------------------------------------------##


## ---------------------------------------------------------------------------##
## plotting 
# Put all BF_lor objects into a list
BF_lor_list <- list(BF_lor1, BF_lor2, BF_lor3, BF_lor4, BF_lor5, BF_lor6)

# Loop over the 6 BF_lor data frames
for (i in 1:6) {
        
        # Generate the plot
        p <- ggplot(BF_lor_list[[i]], aes(x = Lag, y = LORs)) +
                geom_line(color = "blue") +
                geom_ribbon(aes(ymin = L_95_CI, ymax = U_95_CI), 
                            alpha = 0.2) +
                labs(title = paste0("Check 0", i), 
                     x = "lag (minutes)", 
                     y = "log-odds ratio") +
                theme_minimal(base_family = "Times New Roman") + 
                theme(axis.title = element_text(size = 12),
                      axis.text = element_text(size = 10),
                      plot.title = element_text(hjust = 0, 
                                                size = 14))
        
        # Print the plot
        print(p)
        
        # Optionally save each plot
        # ggsave(filename = paste0("Chek_0", i, ".png"), plot = p, width = 6, height = 4, dpi = 300)
}
## ---------------------------------------------------------------------------##
save(BF_lor1, BF_lor2, BF_lor3, BF_lor4, BF_lor5,
     BF_lor6, file = "BF_lor_rounds.RData")
load("BF_lor_rounds.RData")

#######
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


check01 = ggplot(BF_lor_list[[1]], aes(x = Lag, y = LORs)) +
        geom_line(color = "steelblue") +
        geom_ribbon(aes(ymin = L_95_CI, ymax = U_95_CI), 
                    fill = "steelblue", alpha = 0.3) +
        labs(title = "A", 
             x = "time lag (minute)", 
             y = "log-odds ratio") +
        theme_classic2() + 
        my_custom_theme; check01
        # theme_minimal(base_family = "Times New Roman") + 
        # theme(axis.title = element_text(size = 12),
        #       axis.text = element_text(size = 10),
        #       plot.title = element_text(hjust = 0, 
        #                                 size = 14))


check02 = ggplot(BF_lor_list[[2]], aes(x = Lag, y = LORs)) +
        geom_line(color = "steelblue") +
        geom_ribbon(aes(ymin = L_95_CI, ymax = U_95_CI), 
                    fill = "steelblue", alpha = 0.3) +
        labs(title = "B", 
             x = "time lag (minute)", 
             y = "log-odds ratio") +
        theme_classic2() + 
        my_custom_theme; check02

check03 = ggplot(BF_lor_list[[3]], aes(x = Lag, y = LORs)) +
        geom_line(color = "steelblue") +
        geom_ribbon(aes(ymin = L_95_CI, ymax = U_95_CI), 
                    fill = "steelblue", alpha = 0.3) +
        labs(title = "C", 
             x = "time lag (minute)", 
             y = "log-odds ratio") +
        theme_classic2() + 
        my_custom_theme; check03


check04 = ggplot(BF_lor_list[[4]], aes(x = Lag, y = LORs)) +
        geom_line(color = "steelblue") +
        geom_ribbon(aes(ymin = L_95_CI, ymax = U_95_CI), 
                    fill = "steelblue", alpha = 0.3) +
        labs(title = "D", 
             x = "time lag (minute)", 
             y = "log-odds ratio") +
        theme_classic2() + 
        my_custom_theme; check04


check05 = ggplot(BF_lor_list[[5]], aes(x = Lag, y = LORs)) +
        geom_line(color = "steelblue") +
        geom_ribbon(aes(ymin = L_95_CI, ymax = U_95_CI), 
                    fill = "steelblue", alpha = 0.3) +
        labs(title = "E", 
             x = "time lag (minute)", 
             y = "log-odds ratio") +
        theme_classic2() + 
        my_custom_theme; check05


check06 = ggplot(BF_lor_list[[6]], aes(x = Lag, y = LORs)) +
        geom_line(color = "steelblue") +
        geom_ribbon(aes(ymin = L_95_CI, ymax = U_95_CI), 
                    fill = "steelblue", alpha = 0.3) +
        labs(title = "F", 
             x = "time lag (minute)", 
             y = "log-odds ratio") +
        theme_classic2() + 
        my_custom_theme; check06

library(patchwork)
library(patchwork)

(check01 + check02 + check03 +
 check04 + check05 + check06) + 
        plot_layout(ncol = 3, nrow = 2) 
        # plot_annotation(
        #         title = "Round-wise lorelogram (0-360 minutes) for Bengal Fox camera trap detection data",
        #         theme = theme(
        #                 plot.title = element_text(size = 14, face = "bold", hjust = 0, 
        #                                           family = "Times New Roman")
        #         ))


library(patchwork)
library(ggplot2)

# Combine your six plots
combined_plot <- (check01 + check02 + check03 +
                          check04 + check05 + check06) +
        plot_layout(ncol = 3, nrow = 2, guides = "collect") & 
        theme(
                plot.title = element_text(size = 14, face = "bold", hjust = 0, family = "Times New Roman"),
                axis.title.x = element_blank(),
                axis.title.y = element_blank()
        )

# Add common titles using plot_annotation
final_plot <- combined_plot +
        plot_annotation(
                title = "",
                theme = theme(
                        plot.title = element_text(size = 0, face = "bold", hjust = 0, family = "Times New Roman"),
                        plot.margin = margin(0, 0, 0, 0) # space for axis titles
                )
        ) &
        theme(
                plot.margin = unit(c(1, 1, 1, 1), "lines")
        )

# Now add common x and y labels using `grid` package
library(grid)

# Print the plot
print(final_plot)

# Add shared axis labels
grid.text("time lag (minute)", y = unit(0.03, "npc"), rot = 0, gp = gpar(fontsize = 16, fontfamily = "Times New Roman"))
grid.text("log-odds ratio", x = unit(0.02, "npc"), rot = 90, gp = gpar(fontsize = 16, fontfamily = "Times New Roman"))








        