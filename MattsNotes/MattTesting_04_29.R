library(dplyr)

endpoint <- "flights.cwick.co.nz"
user <- "student"
password <- "password"

ontime <- src_postgres("ontime",
                       host = endpoint,
                       port = 5432,
                       user = user,
                       password=password)

flights <- tbl(ontime, "flights")
str(flights)

## practice getting data from database for specific year & month
hou <- flights %.%select(year, month, dayofmonth, origin, uniquecarrier, weatherdelay) %.% filter((year=="2010" & month==2))
explain(hou)

# pull the data
data_local <- collect(hou)

#see what it looks like
table(data_local$origin)

unique_ap <- unique(data_local$origin)
# at least in 2/2010, there are 284 airports.




## Airport Codes
#   downloaded files from here: http://ourairports.com/data/

### try to deal with airport codes

airports <- read.csv("data/airports.csv", stringsAsFactors=FALSE)
regions <- read.csv("data/regions.csv", stringsAsFactors=FALSE)

airports.f <- filter(airports, iso_country=="US", iata_code != "")
regions.f <- filter(regions, iso_country=="US")


table(airports.f$type)
#closed       heliport  large_airport medium_airport  seaplane_base  small_airport 
#39             13            149            458             46            471 


## Need to add state into airports.f, but my brain is rebelling against anything more tonight.