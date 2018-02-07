# First plots for Class (jan 24)

###### Libraries #####
library(raster)
library(tidyverse)
library(sqldf)

###### Import data #####
dem   <- raster("dem20.tif")        # digital elevation model  
start_daily <- read_csv("daily_ws.csv")   # daily temperatures
stations <- read_csv("stations.csv")# spatial station info
###### Attach elevation values to daily ws spread sheet #####

# Extract elevation
coordinates(stations) = ~ EASTING + NORTHING  # df to spatial df
stations$elevation <- raster::extract(dem,stations)      # extract elevation
stations <- as.data.frame(stations)          # spatial df to df

## Join elevation to daily temperatures
daily <- inner_join(start_daily, stations, by = "stationid") 

daily <- dplyr::select(daily, 
                       stationid, 
                       temp_mean, 
                       EASTING, 
                       NORTHING, 
                       elevation,
                       day,
                       month,
                       year) %>%
  filter(is.na(elevation) != TRUE,
         is.na(temp_mean) != TRUE)


###### 1. Map temp_mean of one day for all years ######

# Subset to single day
set_day <- 166 # set julian day
# temp_day <- sqldf(
#   sprintf("select * from daily
#           where day = %s",day)
#   )
temp_day <- filter(daily, day == set_day)

# Plot temp_mean for day faceted by year
plots <- ggplot(data = temp_day) +
  geom_point(mapping =  aes(x = EASTING, y= NORTHING,
                            colour = temp_mean)) +
  facet_wrap(~ year, nrow = 2) +
  coord_fixed() +
  ggtitle(paste("Mean Temp in SWNS on julian day: ",day)) 

# Plot w/o eastings and northings on ticks
plots + theme(axis.text.x = element_blank(), 
          axis.text.y = element_blank())

###### 2. Mean temp vs. elevation for one day, all years ######


# Plot
ggplot(temp_day, mapping=aes(x = elevation, y = temp_mean)) +
  geom_point() + geom_smooth() +
  facet_wrap(~ year) +
  ggtitle(paste0("Mean temp vs elevation on julian day: ",day))

###### 3. Mean temp vs. elevation for one week, one year ######


# Subset to one week of one year
week_start <- 100 # set julian day 1
set_year <- 2017      # set year
temp_week <- filter(daily, 
                    day >= week_start, 
                    day <= (week_start + 7),
                    year == set_year)

ggplot(temp_week, mapping=aes(x = elevation, y = temp_mean)) +
  geom_point() + geom_smooth()+
  facet_wrap(~ day) + ggtitle(paste0("Mean Temp vs. Elevation for One Week in ",year))





