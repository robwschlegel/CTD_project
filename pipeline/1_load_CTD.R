#############################################################################
###"pipeline/load_CTD.R"
## This script does:
# 1. Load DAFF pelagic survey data and save
# 2. Load DEA survey data and save
## DEPENDS ON:
library(tidyverse)
library(purrr)
library(lubridate)
## USED BY:
# pipeline/pipeline.R
## CREATES:
# Lekker cleaned up CTD data products
#############################################################################


# 1. Load DAFF pelagic survey data and save -------------------------------

# The function for loading pelagic spawner biomass data
load_DAFF_pelagic <- function(file_name){
  # Note that limiting the columns in this way shaves off all of the extra variables that may occasionally exist
  file_1 <- read_csv(file_name, skip = 1, col_types = "cccccnnnnnnnnnnn",
                     col_names = c("cruise", "station",	"type", "date", "time",	"lat", "lon",	"bot_depth", 
                                   "depth",	"qf_depth", "temp",	"qf_temp",	"salinity",	"qf_salinity", "oxygen", "qf_oxygen"))
  file_1$date <- as.Date(as.character(parse_date_time(file_1$date, "mdy", tz = "Africa/Johannesburg")))
  file_1$time <- parse_date_time(paste0(file_1$date," ",file_1$time), "ymdHM", tz = "Africa/Johannesburg")
  file_1 <- file_1 %>% 
    select(cruise:time, lon, lat, depth, bot_depth, temp, salinity, oxygen)
  return(file_1)
}

# Load the files
files_DAFF_pelagic <- dir("data/DAFF_pelagic_survey", full.names = TRUE)
# Note that warnings are thrown when shaving columns
# This is not useful to show so they are supressed here
suppressWarnings(system.time(DAFF_pelagic <- map_dfr(files_DAFF_pelagic, load_DAFF_pelagic))) # 13 seconds
## NB: A large chunk of the AFR171 cruise CTD data are missing dates

# Clean up the few errant values
  # The raw data containing these errors were manually checked
  # The errors likely come from mechanical issue
  # It was decided to leave the raw data as is and make the corrections here
## Temp
DAFF_pelagic$temp[DAFF_pelagic$temp > 30] <- NA
DAFF_pelagic$temp[DAFF_pelagic$temp < 2.91] <- NA
# range(DAFF_pelagic$temp, na.rm = T) # check...
## Lat
DAFF_pelagic$lat[DAFF_pelagic$lat > -5] <- NA
# range(DAFF_pelagic$lat, na.rm = T) # check...
## Lon
DAFF_pelagic$lon[DAFF_pelagic$lon < 5] <- NA
# range(DAFF_pelagic$lon, na.rm = T) # check...
## Date
# DAFF_pelagic$date[DAFF_pelagic$date < as.Date("1985-01-01")] <- NA
# range(DAFF_pelagic$date, na.rm = T) # check...
## Save
save(DAFF_pelagic, file = "data/DAFF_pelagic.Rdata")
# write.csv(DAFF_pelagic, file = "data/DAFF_pelagic.csv", row.names = F)


# 2. Load DEA survey data and save ----------------------------------------

# Loading function
load_DEA_SADCO <- function(file_name){
  # Note that limiting the columns in this way shaves off all of the extra variables that may occasionally exist
  file_1 <- read_delim(file_name, delim = "\t", skip = 1, col_types = "cccccnnnnnnnn", na = "1e+010",
                     col_names = c("cruise", "station", "type", "date", "time", "lon", "lat",	"bot_depth",
                                   "etopo2_depth", "depth",	"temp",	"salinity",	"oxygen"))
  file_1$date <- as.Date(as.character(parse_date_time(file_1$date, "mdy", tz = "Africa/Johannesburg")))
  file_1$time <- parse_date_time(paste0(file_1$date," ",file_1$time), "ymdHM", tz = "Africa/Johannesburg")
  file_1 <- file_1 %>% 
    select(cruise:lat, depth, bot_depth, temp, salinity, oxygen)
  return(file_1)
}

# Load the files
files_DEA_SADCO <- dir("data/DEA_SADCO", full.names = TRUE)
# Note that warnings are thrown when shaving columns
# This is not useful to show so they are supressed here
suppressWarnings(system.time(DEA_SADCO <- map_dfr(files_DEA_SADCO, load_DEA_SADCO))) # 11 seconds

# Clean up the few errant values
## Temp
DEA_SADCO$temp[DEA_SADCO$temp > 30] <- NA
DEA_SADCO$temp[DEA_SADCO$temp < 2] <- NA
# range(DEA_SADCO$temp, na.rm = T) # check...
## Lat
# DEA_SADCO$lat[DEA_SADCO$lat > -5] <- NA
# range(DEA_SADCO$lat, na.rm = T) # check...
## Lon
# DEA_SADCO$lon[DEA_SADCO$lon < 5] <- NA
# range(DEA_SADCO$lon, na.rm = T) # check...
## Date
# DEA_SADCO$date[DEA_SADCO$date < as.Date("1985-01-01")] <- NA
# range(DEA_SADCO$date, na.rm = T)
## Save
save(DEA_SADCO, file = "data/DEA_SADCO.Rdata")
# write.csv(DEA_SADCO, file = "data/DEA_SADCO.csv", row.names = F)

