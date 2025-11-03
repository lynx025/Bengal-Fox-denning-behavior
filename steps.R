lm_model <- lm(detection_count ~ 
                       JulianDay + 
                       check_today + 
                       days_since_last_check, 
               data = detection)


library(nlme)
lme_model <- lme(
        detection_count ~ JulianDay + 
                check_today + days_since_last_check,
        random = ~1 | checks,
        data = detection)


library(nlme)
library(segmented)

# Mixed-effects model (random intercept by round)
lme_model1 <- lme(detection_count ~ JulianDay + check_today + days_since_last_check,
                  random = ~1 | checks,
                  data = detection)

lme_model2 <- lme(detection_count ~ JulianDay + check_today + days_since_last_check,
                  random = ~1 | checks,
                  data = detection_merged)


summary(lme_model1)
summary(lme_model2)


library(segmented)
# Segmented regression on fixed effect of JulianDay
seg_model <- segmented.lme(lme_model, Z = ~ JulianDay, psi = 100)  # adjust psi as needed
segmented_model <- segmented(lme_model, seg.Z = ~ JulianDay, psi = list(JulianDay = c(68, 149)))




# Fit a basic linear model
lm_model <- lm(detection_count ~ JulianDay + check_today + days_since_last_check, data = detection)

# Fit segmented regression on JulianDay
seg_model <- segmented(lm_model, seg.Z = ~ JulianDay, psi = 100)  # try psi within range (e.g. 100)
summary(seg_model)


library(segmented)
seg_model <- segmented(lm_model, seg.Z = ~ 
                               JulianDay, psi = 100)  
# or try different psi values


detection$after_break <- pmax(0, detection$JulianDay - 99)
lme_piecewise <- lme(
        detection_count ~ JulianDay + after_break + check_today + days_since_last_check,
        random = ~1 | checks,
        data = detection
)




