library(mgcv)
library(gratia)
library(ggplot2)
library(ggpubr)

# Extract smooth estimates and add 95% CI
smooths(GAM$model1)
sm <- smooth_estimates(GAM$model1, select = "s(JulianDay)") %>%
        add_confint()
ggplot(sm, aes(x = JulianDay, y = .estimate)) +
        geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci),
                    alpha = 0.3, fill = "#E1BE6A") +
        geom_line(color = "#E1BE6A", linewidth = 1.2) +
        labs(x = "Julian day", y = "partial effect",
             title = "") +
        theme_classic2(base_family = "Times") +
        theme(
                axis.title = element_text(size = 16, color = "gray20"),
                axis.text = element_text(size = 12, color = "gray30"),
                plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
        )

sm <- smooth_estimates(GAM$model1, select = "s(days_since_last_check)") %>%
        add_confint()
ggplot(sm, aes(x = days_since_last_check, y = .estimate)) +
        geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci),
                    alpha = 0.3, fill = "forestgreen") +
        geom_line(color = "forestgreen", linewidth = 1.2) +
        labs(x = "days since last check", y = "partial effect",
             title = "") +
        theme_classic2(base_family = "Times") +
        theme(
                axis.title = element_text(size = 16, color = "gray20"),
                axis.text = element_text(size = 12, color = "gray30"),
                plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
        )
