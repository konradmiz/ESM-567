#Step 4 of the Streams Process

source("NMDS Bubble Plots.R")

early_reg <- lm(early_nmds_points$PCT_SCRAPERS ~ early_reg_sp$`coast_data$PCT_FN` + early_reg_sp$`coast_data$PCT_SA`)
mid_reg <- lm(mid_nmds_points$PCT_SCRAPERS ~ mid_reg_sp$`coast_data$PCT_FN` + mid_reg_sp$`coast_data$PCT_SA`)
late_reg <- lm(late_nmds_points$PCT_SCRAPERS ~ late_reg_sp$`coast_data$PCT_FN` + late_reg_sp$`coast_data$PCT_SA`)
summary(early_reg)
summary(mid_reg)
summary(late_reg)

par(mfrow = c(2,2))
plot(early_reg)
plot(mid_reg)
plot(late_reg)
