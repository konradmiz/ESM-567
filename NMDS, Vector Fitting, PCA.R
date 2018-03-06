#Step 2 of the Streams Process. 

library(dplyr)
library(vegan)
source("Data Input and Tidying.R")

### NMDS
early_nmds <- metaMDS(sqrt(early_sp), k = 2, autotransform = FALSE)
mid_nmds <- metaMDS(sqrt(mid_sp), k = 2, autotransform = FALSE)
late_nmds <- metaMDS(sqrt(late_sp), k = 2, autotransform = FALSE)


### Vector Fitting
vector_fn <- function(vector_fit) {
  vector_arrows_df <- data.frame(vector_fit$vectors[1])
  vector_data <- cbind(vector_arrows_df, vector_fit$vectors[2], vector_fit$vectors[3], vector_fit$vectors[4])
  
  vector_data <- vector_data %>%
    mutate(Names = rownames(vector_data)) %>%
    select(Names, everything())
  
  factor_data <- data.frame(vector_fit$factors[2], vector_fit$factors[3], vector_fit$factors[4])
  
  factor_data <- factor_data %>%
    mutate(Names = rownames(factor_data)) %>%
    select(Names, everything())
  
  return_list <- list(vector_data, factor_data)
  
  return(return_list)
}

#Early Vector Fitting

early_vector_fit <- envfit(ord = early_nmds, env = early_env)

early_vector_scores <- vector_fn(early_vector_fit)
early_vectors <- early_vector_scores[[1]]
early_factors <- early_vector_scores[[2]]

#Mid Vector Fitting

mid_vector_fit <- envfit(ord = mid_nmds, env = mid_env)

mid_vector_scores <- vector_fn(mid_vector_fit)
mid_vectors <- mid_vector_scores[[1]]
mid_factors <- mid_vector_scores[[2]]

#Late Vector Fitting
late_vector_fit <- envfit(ord = late_nmds, env = late_env)

late_vector_scores <- vector_fn(late_vector_fit)
late_vectors <- late_vector_scores[[1]]
late_factors <- late_vector_scores[[2]]

