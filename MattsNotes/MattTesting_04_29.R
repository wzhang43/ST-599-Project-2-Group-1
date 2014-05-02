library(dplyr)

endpoint <- "flights.cwick.co.nz"
user <- "student"
password <- "password"

ontime <- src_postgres("ontime", 
                       host = endpoint,
                       port = 5432,
                       user = user,
                       password = password)

flights <- tbl(ontime, "flights")
str(flights)

## practice getting data from database for specific year & month
hou <- flights %.%select(year, month, dayofmonth, origin, uniquecarrier, weatherdelay) %.% filter((year=="2010" & month==2 & origin=="PDX"))
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



write.csv(airports.f, "data/airports_US.csv", row.names=FALSE)
write.csv(regions.f, "data/regions_US.csv", row.names=FALSE)



####   5/1 ==============

## Available fields: SELECT "year", "month", "dayofmonth", "dayofweek", "deptime", "crsdeptime", "arrtime", "crsarrtime", "uniquecarrier", "flightnum", "tailnum", "actualelapsedtime", "crselapsedtime", "airtime", "arrdelay", "depdelay", "origin", "dest", "distance", "taxiin", "taxiout", "cancelled", "cancellationcode", "diverted", "carrierdelay", "weatherdelay", "nasdelay", "securitydelay", "lateaircraftdelay", "id"

## Available codes: 

## practice getting data from database for specific year & month
hou <- flights %.% group_by(year, month, origin) %.% 
  summarise(year, month, origin, avg_del=mean(weatherdelay), n_flights=n(), n_wdelay=count(weatherdelay>0)) %.% 
  filter((year=="2003"))
explain(hou)


system.time(weather.data <- collect(hou))
head(weather.data)
weather.data <- weather.data[,-4:-6]

qry04 <- flights %.% group_by(year, month, origin) %.% 
  summarise(avg_del=mean(weatherdelay), n_flights=n(), n_wdelay=count(weatherdelay>0)) %.% 
  filter((year=="2004"))

wd2004 <- collect(qry04)
weather.data <- rbind(weather.data, wd2004)

qry05 <- flights %.% group_by(year, month, origin) %.% 
  summarise(avg_del=mean(weatherdelay), n_flights=n(), n_wdelay=count(weatherdelay>0)) %.% 
  filter((year=="2005"))

system.time(wd05 <- collect(qry05))
weather.data <- rbind(weather.data, wd05)

years <- as.character(seq(2006, 2013,1))

i<-1

for(i in 5:8){
  yr <- years[i]
  qry <- flights %.% group_by(year, month, origin) %.% 
    summarise(avg_del=mean(weatherdelay), n_flights=n(), n_wdelay=count(weatherdelay>0)) %.% 
    filter((year==yr))
  
  wd.new <- collect(qry)
  weather.data <- rbind(weather.data, wd.new)
}




write.csv(weather.data, "data/popn_weather_data.csv", row.names=FALSE)



## pulled PDX data for 2/2010

table(data_local$weatherdelay)
str(data_local)
sum(data_local$weatherdelay, na.rm=TRUE)

wthr <- data_local$weatherdelay[1171:1200]
sum(wthr, na.rm=TRUE)
sum(wthr != NA)
?count


### testing - here, n_wdelay seems to work. why then are there entries where it's ALL weatherdelay?

hou <- flights %.% group_by(uniquecarrier, year, month, origin) %.% 
  summarise( avg_del=mean(weatherdelay), n=n(), n_wdelay=count(weatherdelay>0)) %.% 
  filter((year=="2010" & month==2 & origin=="PDX"))
explain(hou)

# pull the data
data_local <- collect(hou)

## VLD 8/2003 is one of the ones where it was all weather delay all the time.
vld <- flights %.% select(uniquecarrier, year, month, origin, weatherdelay) %.% 
  filter((year=="2003" & month==8 & origin=="VLD"))
explain(vld)

# pull the data
vld.dat <- collect(vld)
## all of them are 0 or have a value. none are NA

pie <- flights %.% select(uniquecarrier, year, month, origin, weatherdelay) %.% 
  filter((year=="2003" & month==3 & origin=="PIE"))
explain(pie)
pie.dat <- collect(pie)
head(pie.dat)

h2 <- flights %.% group_by(uniquecarrier, year, month, origin) %.% 
  summarise( avg_del=mean(weatherdelay), n=n(), n_wdelay=count(weatherdelay>0),
             n_dly2=count(weatherdelay>0.1)) %.% 
  filter((year=="2010" & month==2 & origin=="PDX"))
explain(h2)

h2.dat <- collect(h2)
head(h2.dat)
wd.new <- mutate(wd.new, p_diff = n_wdelay/n_flights)
head(wd.new)
hist(wd.new$p_diff)



jun03 <- flights %.% group_by( year, month, origin, uniquecarrier) %.% 
  summarise( avg_del=mean(weatherdelay), n=n(), n_wdelay=count(weatherdelay>0),
             n_dly2=count(weatherdelay>0.1)) %.% 
  filter((year=="2003" & month==6 ))
explain(jun03)

j03.dat <- collect(jun03)
head(j03.dat)

jun03.2 <- flights %.% group_by( origin, month, year) %.% 
  summarise( avg_del=mean(weatherdelay), n=n(), n_wdelay=count(weatherdelay>0),
             n_dly2=count(weatherdelay>0.1)) %.% 
  filter((year=="2003" & month==6 ))
explain(jun03.2)

j03.2.dat <- collect(jun03.2)
head(j03.2.dat)



jul03 <- flights %.% group_by( origin, month, year) %.% 
  summarise( avg_del=mean(weatherdelay), n=n(), n_wdelay=count(weatherdelay>0),
             n_dly2=count(weatherdelay>0.1)) %.% 
  filter((year=="2003" & month==7 ))
explain(jul03)

jul03.dat <- collect(jul03)
head(jul03.dat)
