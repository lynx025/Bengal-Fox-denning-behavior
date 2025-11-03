# Load required libraries
library(ggplot2)
library(patchwork)
library(cowplot)
library(grid)
library(magick)


# Correct PhyloPic URLs via API
fox_url     <- "https://images.phylopic.org/images/a1116e25-7b50-4666-bef5-de18b6e2778c/raster/1527x1536.png"
monitor_url <- "https://images.phylopic.org/images/ce6a78bc-3ef1-4d60-ab40-113eb84c7802/raster/1536x841.png"
human_url   <- "https://images.phylopic.org/images/036b96de-4bca-408e-adb6-2154fcd724ef/raster/602x1536.png"

# Read images
fox_img     <- image_read(fox_url)
monitor_img <- image_read(monitor_url)
human_img   <- image_read(human_url)

# Step 1: Create the base plot without a legend
B_clean <- ggplot(KDE_bengal_fox_vs_human , 
                  aes(x = x/(2*pi)*24, y = y, fill = Species, color = Species)) +
        geom_ribbon(aes(ymin = y - 1.96*se, ymax = y + 1.96*se), 
                    alpha = 0.3, linewidth = 0.25) +
        geom_line(linewidth = 0.5) +
        # 🌅 Twilight bands
        annotate("rect", xmin = 4.81-.5, xmax = 5.73+.5, ymin = -Inf, ymax = Inf,
                 fill = "gray10", alpha = 0.1) +
        annotate("rect", xmin = 18.5-.5, xmax = 19.4+.5, ymin = -Inf, ymax = Inf,
                 fill = "gray10", alpha = 0.1) +
        
        # geom_vline(xintercept = c(5.5, 7.5, 17.5, 19.5),
        #            linetype = "dashed",
        #            color = "grey70",
        #            linewidth = 0.25) +
        ggpubr::theme_classic2() +
        scale_color_manual(values = c(
                "Bengal Fox" = scales::alpha("#E1BE6A", 0.5), 
                "Human" = scales::alpha("#40B0A6", 0.5))) + 
        scale_fill_manual(values = c(
                "Bengal Fox" = scales::alpha("#E1BE6A", 0.3), 
                "Human" = scales::alpha("#40B0A6", 0.3))) +
        labs( x = "time of day (hours)",
              y = "activity density",
              title = "", tags = "B") +
        my_custom_theme + 
        theme(
                legend.position = "none",
                axis.title = element_text(size = 14, color = "gray40", face = "bold"),
                axis.text = element_text(size = 14, color = "gray40"),
                axis.title.x = element_text(vjust = -0.01),
                axis.title.y = element_text(vjust = 2),
                plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
                plot.tag = element_text(size = 18, color = "black", face = "bold")
        ) +
        scale_x_continuous(breaks = seq(0, 24, 4), 
                           labels = seq(0, 24, 4))


# Step 2: Add the silhouettes and labels using ggdraw
B_with_legend <- ggdraw() +
        draw_plot(B_clean) +
        
        # Silhouettes and labels (adjust x/y to place top-left)
        draw_image(fox_img,     x = 0.22, y = .85, width = 0.05, height = 0.08) +
        draw_label("Bengal Fox", x = 0.40, y = 0.89, 
                   color = "#E1BE6A", fontfamily = "Times New Roman", size = 14) +
        
        draw_image(human_img,   x = 0.23, y = 0.79, width = 0.05, height = 0.06) +
        draw_label("Human",     x = 0.364, y = 0.82,
                   color = "#40B0A6", fontfamily = "Times New Roman", size = 14)

(A_with_legend | B_with_legend) 
