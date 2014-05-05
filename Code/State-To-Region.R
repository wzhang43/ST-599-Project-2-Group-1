### State & Region Assignments
library(dplyr)

# Matt did this State-to-Region by hand based on the map here:
#   1) http://www.ncdc.noaa.gov/monitoring-references/maps/us-climate-regions.php
state.reg <- read.csv("data/State_Region.csv", header=T, stringsAsFactors=F)


# WAnli's code from Rcode_local
airinfo = read.csv("data/airports.csv",header=T, stringsAsFactors=F) # used "airports.csv" from class website

air.state = airinfo %.% filter(country=="USA") %.%
  mutate(origin=iata) %.%
  select(origin, state)

# join two tables make data frame of origin, state, region
names(air.state) <- c("origin", "State")

air.state <- rbind(air.state,
                   c("SPN", NA),
                   c("BKG", "MO"),
                   c("ECP", "FL"), 
                   c("UTM", "MS"), 
                   c("AZA", "AZ")
                   )

iata.region = inner_join(air.state, state.reg, by="State") # 3340 rows

# get the unique airports represented in the actual database
popn.data <- read.csv("data/popn_weather_data.csv", header=T)
popn.ports <- unique(popn.data$origin) ## 360 unique iata identifiers.

#filter airport/region list to just airports in the database.
iata.region = iata.region%.%filter(origin %in% popn.ports) # 355 airports



#save this data
write.csv(iata.region, "data/iata_by_region.csv", row.names=FALSE) # updated 5/4 with previously missing airports


# -----------------------------------------------------------------------------#
# missing 5 observations
iata.origin <- sort(as.character(iata.region$origin))
setdiff(iata.origin, orig)
t <- orig[which(!orig %in% iata.origin)]
t ## "BKG" "ECP" "UTM" "SPN" "AZA"

#
# these five airports are not in the iata file?
#


airinfo[which(airinfo$iata %in% "PDX"),] #test
airinfo[which(airinfo$iata %in% t),] # only SPN is there, it lacks city or State which is why mine didn't pull it. Tehcnically a us territory.
airinfo[which(airinfo$iata %in% "BKG"),] # SW of  Branson, Missouri, MO
airinfo[which(airinfo$iata %in% "ECP"),] # NW flroida beached International airport, FL
airinfo[which(airinfo$iata %in% "UTM"),] # tunica airport, Mississipit, MS
airinfo[which(airinfo$iata %in% "AZA"),] # phoenix/mesa gateway airport, AZ

"SPN", NA,
"BKG", "MO",
"ECP", "FL", 
"UTM", "MS", 
"AZA", "AZ"


# -------------------------------------------------------------------------------#

# Make a list of lists of airport by region

region.df <- tbl_df(read.csv("data/iata_by_region.csv", header=T, stringsAsFactors=F))

# individual regions
ak.list <- region.df[which(region.df$Region=="Alaska"),1]
c.list <- region.df[which(region.df$Region=="Central"),1]
hi.list <- region.df[which(region.df$Region=="Hawaii"),1]
nr.list <- region.df[which(region.df$Region=="NorthRockies"),1]
ne.list <- region.df[which(region.df$Region=="Northeast"),1]
nw.list <- region.df[which(region.df$Region=="Northwest"),1]
so.list <- region.df[which(region.df$Region=="South"),1]
se.list <- region.df[which(region.df$Region=="Southeast"),1]
sw.list <- region.df[which(region.df$Region=="Southwest"),1]
uk.list <- region.df[which(region.df$Region=="Unknown"),1]
um.list <- region.df[which(region.df$Region=="UpperMidwest"),1]
we.list <- region.df[which(region.df$Region=="West"),1]


# combine into a list of lists
r.list <- list(ak.list, c.list, hi.list, nr.list, ne.list, nw.list, so.list, se.list, sw.list, uk.list, um.list, we.list)

# how to reference
o.list <- r.list[[1]]
