#Step 1 of the streams process

library(dplyr)
library(lubridate)

streams <- readr::read_csv("Oregon EMAP Streams Data.csv")
streams <- streams[, -1]

streams <- streams %>% #removes 12 sites with NAs
  filter(!is.na(Eco3.x))

streams$LONG.y <- NULL ## Note--Konrad removed this section
streams$Strm_Power.y <- NULL
streams$LAT.y <- NULL
streams$Land_Owner <- NULL

streams$ELEV_m.y <- NULL ## Anna removed these
streams$Elev_FT <- NULL
streams$Area..SQFT. <- NULL
streams$STATION_KEY.1 <- NULL
streams$M_Slp_sqt <- NULL
streams$DATE.y <- NULL
streams$YEAR_EST <- NULL
streams$El_m_sqt <- NULL
streams$LSUB_DMM <- NULL
streams$LSUBD_SD <- NULL

streams$STATION_KEY.x <- NULL
streams$Map_Slope.x <- NULL
streams$X.transitional <- NULL
streams$sample_day <- NULL
streams$Precip_in <- NULL
streams$X.past_hay <- NULL
streams$V1W_MSQ <- NULL
streams$V4W_MSQ <- NULL
streams$Watershed_HUC <- NULL

streams$YEAR <- NULL
streams$REACHLEN <- NULL
streams$V1TM100 <- NULL
streams$sample_month <- NULL
streams$hifrag <- NULL
streams$medfrag <- NULL
streams$lowfrag <- NULL

coast_data <- streams %>%
  filter(Eco3.x == 1) 

#Adds sample date, sample day, and sample month. Sample day/sample month used for season delineation
coast_data <- coast_data %>%
  mutate(sample_date = dmy(coast_data$DATE.x)) %>%
  mutate(sample_month = month(sample_date)) %>%
  mutate(sample_day = day(sample_date)) %>%
  mutate(Season = case_when(sample_month < 7 | (sample_month == 7 & sample_day <= 20) ~ "Early",
                            (sample_month == 7 & sample_day > 20) | (sample_month == 8 & sample_day <= 20) ~ "Mid",
                            sample_month != "Early" & sample_month != "Mid" ~ "Late")) 

#These are the species identified by Dr. Edwards as the scrapers. Includes 4 variables of interest and season data

reg_sp <- tibble(coast_data$Rhithrogena, coast_data$Epeorus_albertae, 
                 coast_data$Epeorus_deceptivus, coast_data$Epeorus_grandis, coast_data$Epeorus_longimanus, 
                 coast_data$Glossosoma,
                 coast_data$Drunella_coloradensis.flavilinea, coast_data$Drunella_doddsi, 
                 coast_data$Drunella_grandis, coast_data$Drunella_pelosa, coast_data$Drunella_spinifera, 
                 coast_data$PCT_FA, coast_data$PCT_FN, coast_data$PCT_SAFN, coast_data$PCT_SA, coast_data$Season)

# subset the species by season
early_reg_sp <- reg_sp %>%
  filter(coast_data$Season == "Early")

mid_reg_sp <- reg_sp %>%
  filter(coast_data$Season == "Mid")

late_reg_sp <- reg_sp %>%
  filter(coast_data$Season == "Late")


# A function to clean the environmental data that we have. Takes the environmental data. 
# Drops numeric variables missing more than two sites, and imputes the mean for variables with 1 or 2 missing observations.
# Drops character -> factor variables that are missing any observations. 
# This function can return only numeric variables, or categorical and numeric variables. 
# This function is called by the subset-data-function which divides data into species or environmental
# and subsequently needs missing environmental data imputed 
clean_data_fn <- function(env_data, numerics = FALSE) {
  numeric_vars <- env_data %>% #select numeric variables
    select_if(is.numeric)
  
  missing_numerics <- which(colSums(is.na(numeric_vars)) <= 2) #keeps numeric variables with <= 2 NAs
  
  no_missing_numerics <- numeric_vars[, missing_numerics]
  
  mean_of_cols <- colMeans(no_missing_numerics, na.rm = TRUE)
  
  for (i in 1:ncol(no_missing_numerics)){ #goes through the variables and adds in the mean if there is an NA
    temp <- which(is.na(no_missing_numerics[, i]))
    if (length(temp) != 0)
    {
      no_missing_numerics[temp, i] <- mean_of_cols[i]
    }
  }
  
  if (numerics == TRUE) #if we're looking just for numeric variables
  {
    return(no_missing_numerics)
  }
  
  char_vars <- env_data %>% #select character variables
    select_if(is.character)
  
  factor_vars <- data.frame(lapply(char_vars, as.factor)) #make them factors
  
  no_missing_factors <- which(colSums(is.na(factor_vars)) == 0) #drop the ones without missing observations
  
  no_missing_vars <- cbind(no_missing_numerics, factor_vars[, no_missing_factors])
  
  return(no_missing_vars) #return the numeric and factor variables
}

#This function subsets the streams by season, specifying the season, whether we want species or 
#environmental variables, and whether we just want numeric environmental variables
subset_data_fn <- function(data, season, var, numerics = FALSE) {
  season_to_filter <- season
  
  data_name <- data %>%
    filter(Season == season_to_filter)
  
  if (var == "sp") { #select the species we have
    data_name <- data_name %>%
      select(Acentrella_insignificans:Staphylinidae)
    
  } else
  {
    data_name <- data_name %>% #select the environmental/associated variables
      select(site.id, SITE_ID:ncol(coast_data))
    
    if (numerics == TRUE) #check if we want only numeric variables (numerics = TRUE) or numeric and factor (FALSE)
    {
      data_name <- clean_data_fn(data_name, numerics = TRUE)
    } else
    {
      data_name <- clean_data_fn(data_name, numerics = FALSE)
      
    }
  }
  
  return(data_name)
  
}

## Subset the data by season

#Early
#Species
early_sp <- subset_data_fn(data = coast_data, season = "Early", var = "sp")
#Environmental (categorical + numerical)
early_env <- subset_data_fn(data = coast_data, season = "Early", var = "env", numerics = FALSE)
#Environmental (numerical)
early_env_numerics <- subset_data_fn(data = coast_data, season = "Early", var = "env", numerics =TRUE)

#Keep only variables that have distinct values (remove variables where each value is identical)
early_env <- early_env[sapply(early_env, function(x) length(unique(x))>1)]
#Remove variables that have duplicated values elsewhere in the dataset
early_env <- early_env[!duplicated(lapply(early_env, summary))]

early_env_numerics <- early_env_numerics[sapply(early_env_numerics, function(x) length(unique(x))>1)]
early_env_numerics <- early_env_numerics[!duplicated(lapply(early_env_numerics, summary))]

#Mid
mid_sp <- subset_data_fn(data = coast_data, season = "Mid", var = "sp")
mid_env <- subset_data_fn(data = coast_data, season = "Mid", var = "env", numerics = FALSE)
mid_env_numerics <- subset_data_fn(data = coast_data, season = "Mid", var = "env", numerics = TRUE)

mid_env <- mid_env[sapply(mid_env, function(x) length(unique(x))>1)]
mid_env <- mid_env[!duplicated(lapply(mid_env, summary))]

mid_env_numerics <- mid_env_numerics[sapply(mid_env_numerics, function(x) length(unique(x))>1)]
mid_env_numerics <- mid_env_numerics[!duplicated(lapply(mid_env_numerics, summary))]

#Late
late_sp <- subset_data_fn(data = coast_data, season = "Late", var = "sp")
late_env <- subset_data_fn(data = coast_data, season = "Late", var = "env", numerics = FALSE)
late_env_numerics <- subset_data_fn(data = coast_data, season = "Late", var = "env", numerics = TRUE)


late_env <- late_env[sapply(late_env, function(x) length(unique(x))>1)]
late_env <- late_env[!duplicated(lapply(late_env, summary))]

late_env_numerics <- late_env_numerics [sapply(late_env_numerics , function(x) length(unique(x))>1)]
late_env_numerics <- late_env_numerics[!duplicated(lapply(late_env_numerics, summary))]



### Previous work on identifying variables (Ignore)
#early_vars <- data.frame(Season = "Early", Vars = colnames(early_env))

#mid_vars <- data.frame(Season = "Mid", Vars = colnames(mid_env))
#late_vars <- data.frame(Season = "Late", Vars = colnames(late_env))

#diff_vars <- mid_vars %>%
#  anti_join(mid_vars, by = c("Season" = "Season"))

#colnames(early_env)[which(!(colnames(early_env) %in% colnames(mid_env)))]
#colnames(mid_env)[which(!(colnames(mid_env) %in% colnames(early_env)))]

#colnames(early_env)[which(!(colnames(early_env) %in% colnames(late_env)))]
#colnames(late_env)[which(!(colnames(late_env) %in% colnames(early_env)))]

#colnames(mid_env)[which(!(colnames(mid_env) %in% colnames(late_env)))]
#colnames(late_env)[which(!(colnames(late_env) %in% colnames(mid_env)))]
