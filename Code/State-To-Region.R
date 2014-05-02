### State & Region Assignments
library(dplyr)

# Matt did this State-to-Region by hand based on the map here:
#   1) http://www.ncdc.noaa.gov/monitoring-references/maps/us-climate-regions.php
state.reg <- read.csv("data/State_Region.csv")


# WAnli's code from Rcode_local
airinfo = read.csv("data/airports.csv",header=T) # used "airports.csv" from class website

air.state = airinfo %.% filter(country=="USA") %.%
  mutate(origin=iata) %.%
  select(origin, state)

# join two tables make data frame of origin, state, region
names(air.state) <- c("origin", "State")
iata.region = inner_join(air.state, state.reg, by="State") # 3340 rows

# get the unique airports represented in the actual database
popn.data <- read.csv("data/popn_weather_data.csv", header=T)
popn.ports <- unique(popn.data$origin)

#filter airport/region list to just airports in the database.
iata.region = iata.region%.%filter(origin %in% popn.ports) # 344 airports

#save this data
write.csv(iata.region, "data/iata_by_region.csv", row.names=FALSE)


# fiddling with pulling regional lists of airports
west.list <- iata.region%.%filter(Region=="West")%.%select(origin)

iata.region%.%group_by(Region)%.%summarise(n())
summarise(iata.region)