

library(dplyr)

### connecting to the database:
  endpoint = "flights.cwick.co.nz"
  user = "student"
  password = "password"

  ontime = src_postgres("ontime", 
    host = endpoint,
    port = 5432,
    user = user,
    password = password)



### convert table "flights" into a tbl_df():
  flights = tbl(ontime, "flights")
  str(flights)
  # dim(flights)


#-------------------------------------------------------------------------#

           #### Matt's code for reading population data ####

### Setup for query cycle
  weather.data = data.frame()
  years = as.character(seq(2003, 2013,1))

## It was taking really long to try and do all 10 years at once, so I broke it down into year by year
## it takes about 80 seconds per year


## UPDATED 5/4 to fix problem with counting flights delayed by weather.
## SUM ignores NA, AVG ignores NA. Count does not, and my attempts to finagle it otherwise didn't seem to work. 


## ==== NOTE ==== This will have the first 6 months of 2003 in it, so we'll need to filter those out by hand.

  for(i in 1:length(years)){
    yr = years[i]
    qry = flights %.% group_by(year, month, origin) %.%
      summarise( ttl_del=sum(weatherdelay), avg_del=mean(weatherdelay), sd_del=sd(weatherdelay),
                n_flights=n(), 
                n_wdelay=sum(if(weatherdelay>0) { 1} else {0})
                ) %.%
      filter(year==yr)
  
    wd.new = collect(qry)
    weather.data = rbind(weather.data, wd.new)
  }


## write to file so we don't have to run query again.

  write.csv(weather.data, "data/popn_weather_data.csv", row.names=FALSE)




#-------------------------------------------------------------------------#

  weather = read.csv("data/popn_weather_data.csv", header=T)

### subset dataset:
  delay = filter(weather, (year>=2003))
  delay = delay[!(delay$year==2003 & delay$month<6),]

  dim(delay)
  # collect(delay)
 

#-------------------------------------------------------------------------#


  
### information on airport IATA code and belonging state:
  airinfo = read.csv("data/airports.csv",header=T) # used "airports.csv" from class website

  air.state = airinfo %.%
    mutate(origin=iata) %.%
    select(origin, state)

  unique(air.state$state) # codes other than US states exist

  delay.state = left_join(delay, air.state, by="origin") # join two tables to include states, use left_join to preserve all data
  dim(delay.state) 
  




### organize all states into regions:
### origin: http://www.ncdc.noaa.gov/monitoring-references/maps/us-climate-regions.php
  
  reg = read.csv("data/State_Region.csv", header=T) # see original file: added "unknown" region label for military bases and protectorates
  reg = reg %.%
    mutate(state=State, region=Region) %.%
    select(state, region)
  
  # add region label to each entry:
  delay.region = left_join(delay.state, reg, by="state")

  delay.region = arrange(delay.region, origin, year, month)
  dim(delay.region)
  head(delay.region, n=30L)




#----------------------------------------------------------------------------#


