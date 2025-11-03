library (patchwork)

my_custom_theme = theme(
        text = element_text(family = "Times New Roman"),
        axis.line = element_line(colour = 'black', linetype = 'solid'),
        axis.ticks = element_line(colour = 'black', linetype = 'solid'),
        axis.text =  element_text(size = 12),
        axis.title = element_text(size = 14, colour = 'gray50', face = "bold"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = 'lightgrey', linetype = 'dashed', linewidth = 0.5),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 14, face = "bold")
)


library("gratia")
# JulianDay plot
p1 <- draw(gam_model2, select = "s(JulianDay)") +
        labs(
                title = "",
                x = "Julian Day",
                y = "partial effect on log(Detections)"
        ) +
        theme_minimal() +
        my_custom_theme
        # +
        # theme(
        #         plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "darkgreen"),
        #         axis.title = element_text(size = 14, face = "bold", color = "black"),
        #         axis.text = element_text(size = 12))

# days_since_last_check plot
p2 <- draw(gam_model2, select = "s(days_since_last_check)") +
        labs(title = "",
             x = "days since last check",
             y = "partial effect on log(Detections)") +
        theme_minimal() + 
        my_custom_theme
        # +
        # theme(
        #         plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "steelblue"),
        #         axis.title = element_text(size = 14, face = "bold", color = "black"),
        #         axis.text = element_text(size = 12))

library(patchwork)

(p1 + p2) + 
        plot_layout(ncol = 2) + 
        plot_annotation(
                title = "detection probability \ndetection_count ~ s(Julian Day) + s(days since last check) + check_today (Y/N) \nno thinning done. accumulated total count per camera trap day.",
                theme = theme(
                        plot.title = element_text(size = 14, face = "bold", hjust = 0, 
                                                  family = "Times New Roman")
                )
        )

names(sm)
sm <- smooth_estimates(model5, select = "s(JulianDay)")
sm <- sm %>%
        mutate(
                lower_ci = .estimate - 1.96 * .se,
                upper_ci = .estimate + 1.96 * .se)
ggplot(sm, aes(x = JulianDay, y = .estimate)) +
        geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.3, fill = "forestgreen") +
        geom_line(color = "forestgreen", size = 1.2) +
        labs(
                x = "Julian Day",
                y = "Partial Effect",
                title = "Partial effect of Julian Day") +
        theme(text = element_text(family = "Times"))








