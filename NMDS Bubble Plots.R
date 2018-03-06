#Step 3 of the Streams Process

source("NMDS, Vector Fitting, PCA.R")
library(ggplot2)
library(ggrepel)

#Gets the NMDS points
early_nmds_points <- data.frame(early_nmds$points)
#Gets the relative abundance of scrapers (cols 1:11) of each site
early_nmds_points$PCT_SCRAPERS = rowSums(early_reg_sp[, 1:11])
#Gets the right arrow lengths on the vector fitting
early_nmds_arrows <- data.frame(scores(early_vector_fit, display = "vector"))
early_nmds_arrows$Names <- rownames(early_nmds_arrows)


mid_nmds_points <- data.frame(mid_nmds$points)
mid_nmds_points$PCT_SCRAPERS = rowSums(mid_reg_sp[, 1:11])

mid_nmds_arrows <- data.frame(scores(mid_vector_fit, display = "vector"))
mid_nmds_arrows$Names <- rownames(mid_nmds_arrows)


late_nmds_points <- data.frame(late_nmds$points)
late_nmds_points$PCT_SCRAPERS = rowSums(late_reg_sp[, 1:11])

late_nmds_arrows <- data.frame(scores(late_vector_fit, display = "vector"))
late_nmds_arrows$Names <- rownames(late_nmds_arrows)

#NMDS plot with vector-fitting overlay

#Early
ggplot() + 
  geom_point(data = early_nmds_points, aes(MDS1, MDS2, size = 2 * PCT_SCRAPERS)) + 
  geom_segment(data = early_nmds_arrows, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
               arrow = arrow(length = unit(0.25, "cm"))) + 
  geom_text_repel(data = early_nmds_arrows, aes(x = NMDS1, y = NMDS2, label = Names)) + 
  ggtitle("Early Season Vector Fitting")

#Mid
ggplot() +
  geom_point(data = mid_nmds_points, aes(MDS1, MDS2, size = 2 * PCT_SCRAPERS)) + 
  geom_segment(data = mid_nmds_arrows, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
               arrow = arrow(length = unit(0.25, "cm"))) + 
  geom_text_repel(data = mid_nmds_arrows, aes(x = NMDS1, y = NMDS2, label = Names)) + 
  ggtitle("Mid Season Vector Fitting")


#Late
ggplot() +
  geom_point(data = late_nmds_points, aes(MDS1, MDS2, size = 2 * PCT_SCRAPERS)) + 
  geom_segment(data = late_nmds_arrows, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
               arrow = arrow(length = unit(0.25, "cm"))) + 
  geom_text_repel(data = late_nmds_arrows, aes(x = NMDS1, y = NMDS2, label = Names)) + 
  ggtitle("Late Season Vector Fitting")


