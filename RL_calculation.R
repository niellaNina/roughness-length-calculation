# Roughness length calculation Z0
library("rjson")
library("httr")
library("jsonlite")
library("rlist")
library("plyr")
library("dplyr")
library("lubridate")

# Authentication information for frost.met.no
client_id <- "9bc16cd9-227c-4849-8bf6-61ce8f1043f5"
secret <- "9517dfb5-e9ed-4292-9cb1-b2e7608c3a6c"

# constants in Z0 calculation 
Cu <- 1.94 # constant for wind SPEED
Cv<-1.86 # constant for wind DIRECTION
K <- 0.4  # von Karman constant

# ------ import data from given station -----

# To be added when making things interactive
# stnr <- as.data.frame(18700)  # Oslo-Blindern
# referencetime <- ""
# elements <- "&elements=wind_speed%2C%20wind_from_direction"

# build the string to use with frost.met.no
# streng <- buildObsURL(referencetime, elements, stnr)
streng <-"https://frost.met.no/observations/v0.jsonld?sources=SN18700&referencetime=2017-06-01T00%3A00%3A00%2F2018-01-01T00%3A00%3A00&elements=wind_speed%2C%20wind_from_direction&timeresolutions=PT10M&fields=elementId%2C%20value%2C%20unit%2C%20referencetime%2C%20sourceid%2C%20timeResolution"
# remember that the string needs to have time resolution defined! for this time P10M to get 10 min observations and not hourly observations

# importing data
jsondata <- content(GET(streng, authenticate(client_id, secret)),as="text")
mydata <- fromJSON(jsondata, flatten = FALSE, simplifyVector = TRUE)

# extracting data 
#extract stationid from metadata NB! not really neccessary when only looking at one statiton!!
stationid <- as.data.frame(mydata$data$sourceId)
colnames(stationid) <- "stationid"
# remove SN and :0. regex explanation: (?![SN]): after SN, (.*?): everything in between, (?=:), stop before : is reached
# stationid$id <- extract(stationid, stationid, "id", regex = "(?![SN])(.*?)(?=:)")

#extract reference time (single characterlist to df):
ref_time <- as.data.frame(ymd_hms(mydata$data$referenceTime))
colnames(ref_time) <- "ref_time"

#extract observations and rearrange values
observations <- ldply(mydata$data$observations, data.frame)

wind_speed <- filter(observations, elementId=="wind_speed")
wind_direction <- filter(observations, elementId=="wind_from_direction")
wind_direction$sector <- cut(wind_direction$value, 12) #set group for sector of wind direction
wind_speed$sector <- wind_direction$sector#set same group for wind speed

# ------ Calculations ------

# Get Measurement height. From frost or stinfosys? If from frost remember issues with using ldply on line 45. 
# level is a dataframe in a dataframe in a list in the JSON file -> ISSUES!
Z <- 10 # unit = m, levelType 0 "height_above_ground" 

# compute standard deviation of wind speed (sU)
sU<-sd(wind_speed$value)

# compute standard deviation of wind direction (sDir) using the Yamartino method
Sa <- mean(sin(wind_direction$value))
Ca <- mean(cos(wind_direction$value))

eps <- sqrt(1-(Sa^2+Ca^2))

sDir <-asin(eps)*(1+((2/sqrt(3))-1)*eps^3)

# calculate roughness length speed + direction
wind_speed$Z0 <- Z/(exp(Cu*K*wind_speed$value/sU)) 
wind_direction$Z0dir <- Z/(exp(Cv*K*wind_direction$value/sDir)) 

# calculate mean roughness length per sector, remember to also drop all windspeeds over 5 m/s. 
# You might need to merge windspeed and wind direction data.
# Computer estimated values of roughness lenght is only accurate to one ignidicant value, i.e. 0.34 -> 0.3

RL_speed <- wind_speed %>%
    dplyr::group_by(sector) %>%
    dplyr::filter(value>4) %>% # only use cases where windspeed greater than 4 m/s
    dplyr::summarize(avgZ0 = round(mean(Z0),digits = 1)) 

RL_dir <- wind_direction %>%
  dplyr::group_by(sector) %>%
  dplyr::filter(value>4) %>% # only use cases where windspeed greater than 4 m/s
  dplyr::summarize(avgZ0dir = round(mean(Z0dir), digits = 1)) 

RL_sector <- merge(RL_speed, RL_dir, by = 1)
