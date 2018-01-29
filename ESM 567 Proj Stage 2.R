library(readr)
library(dplyr)


kaggle <- read_csv("C:\\Users\\Konrad\\Downloads\\GlobalLandTemperaturesByCity.csv\\GlobalLandTemperaturesByCity.csv")
majorcity <- read_csv("C:\\Users\\Konrad\\Downloads\\GlobalLandTemperaturesByMajorCity.csv\\GlobalLandTemperaturesByMajorCity.csv")
continents <- read_csv("C:\\Users\\Konrad\\Downloads\\Countries-Continents-csv.csv")
continents <- continents[, c(1,2)]


str(majorcity)

#majorcity$City <- as.factor(majorcity$City)

majorcity$dt <- as.Date(majorcity$dt)
majorcity$Country <- as.factor(majorcity$Country)


majorcity_summary <- majorcity %>% 
  group_by(City) %>%
  count(!is.na(AverageTemperature)) #counts # of observations in each city (TRUE) and # missing (FALSE)

majorcity_summary <- majorcity_summary %>%
  rename(observations = "!is.na(AverageTemperature)") %>%
  filter(observations == TRUE & n > 120)


top_observed_cities <- head(majorcity_summary[order(-majorcity_summary$n),], 100)

top_observed_cities <- top_observed_cities[,-2]

top_observed_cities["duplicates"] <- NA
top_observed_cities["missing_vals_since_1891"] <- NA
top_observed_cities["missing_vals_since_1791"] <- NA


for (i in 1:nrow(top_observed_cities))
{
  current_city_df <- majorcity %>%
    filter(City == as.character(top_observed_cities[i, 1]))
  
  top_observed_cities[i,3] <- sum(duplicated(current_city_df$dt))
   
  missing_since_1891 <- current_city_df %>%
    filter(dt >= "1891-01-01" & dt < "2013-09-01") %>%
    count(is.na(AverageTemperature))
  
  if (nrow(missing_since_1891) == 2)
  {
    top_observed_cities[i, 4] <- missing_since_1891[2, 2]
  } else {
    top_observed_cities[i, 4] <- 0
  }
  
  missing_since_1791 <- current_city_df %>%
    filter(dt >= "1791-01-01" & dt < "2013-09-01") %>%
    count(is.na(AverageTemperature))
  
  if (nrow(missing_since_1791) == 2)
  {
    top_observed_cities[i, 5] <- missing_since_1791[2, 2]
  } else {
    top_observed_cities[i, 5] <- 0
  }
  
  
}

write_csv(top_observed_cities, "C:\\Users\\Konrad\\Desktop\\ESM 567\\top_observed_major_cities.csv")


#length(which(duplicate_rongcheng))

aggregated_major_city <- majorcity %>%
  group_by(City) %>%
  select(City, Country, Latitude, Longitude) %>%
  distinct()

aggregated_major_city <- aggregated_major_city %>% left_join(continents)
aggregated_major_city$Continent <- as.factor(aggregated_major_city$Continent)

summary(aggregated_major_city$Continent)

colnames(top_observed_cities)

top_observed_cities <- top_observed_cities %>% 
  left_join(aggregated_major_city) %>%
  select(Continent, Country, City, n, Latitude, Longitude, duplicates, everything() )


top_observed_cities[, c(1, 2, 3)] <- lapply(top_observed_cities[, c(1, 2, 3)], factor)
summary(top_observed_cities)

#top_observed_cities[6, 1] <- "Europe" #Russia
#top_observed_cities[9, 1] <- "Europe" #Russia
top_observed_cities[20, 1] <- "Asia" #Burma
top_observed_cities[59, 1] <- "Asia" #Korea
top_observed_cities[69, 1] <- "Asia" #Taiwan
top_observed_cities[87, 1] <- "Africa" #Ivory Coast
top_observed_cities[90, 1] <- "Africa" #DRC (Congo)

losangeles <- majorcity %>%
  filter(City == "Los Angeles")

write_excel_csv(top_observed_cities, "C:\\Users\\Konrad\\Desktop\\ESM 567\\top_observed_major_cities.csv")

