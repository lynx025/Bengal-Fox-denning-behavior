# Step 1: Base plot without legend
A_clean <- ggplot(KDE_bengal_fox_vs_monitor, 
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
                "Bengal Monitor" = scales::alpha("#40B0A6", 0.5))) + 
        scale_fill_manual(values = c(
                "Bengal Fox" = scales::alpha("#E1BE6A", 0.3), 
                "Bengal Monitor" = scales::alpha("#40B0A6", 0.3))) +
        labs(x = "time of day (hours)",
             y = "activity density", title = "", tags = "A") +
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

# Step 2: Add silhouette legend using cowplot
A_with_legend <- ggdraw() +
        draw_plot(A_clean) +
        
        draw_image(fox_img,     x = 0.22, y = .85, width = 0.05, height = 0.08) +
        draw_label("Bengal Fox", x = 0.40, y = 0.89, 
                   color = "#E1BE6A", fontfamily = "Times New Roman", size = 14) +
        
        draw_image(monitor_img, x = 0.22, y = 0.77, width = 0.07, height = 0.1) +
        draw_label("Bengal Monitor", x = 0.44, y = 0.82,
                   color = "#40B0A6", fontfamily = "Times New Roman", size = 14)



combinedplot = (A_with_legend | B_with_legend) 
ggsave(
        filename = "combinedplot.tiff",         # or .tiff or .pdf depending on target
        plot = combinedplot,
        width = 9.2,                             # width in inches
        height = 5.5,                             # height in inches
        dpi = 600,                              # high resolution for print
        units = "in",                           # inches are typical for print
        bg = "white"                            # avoid transparency issues
)
