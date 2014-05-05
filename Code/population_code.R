

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
  years = as.character(seq(2003, 2013, 1))

## It was taking really long to try and do all 10 years at once, so I broke it down into year by year
## it takes about 80 seconds per year


## UPDATED 5/4 to fix problem with counting flights delayed by weather.
## SUM ignores NA, AVG ignores NA. Count does not, and my attempts to finagle it otherwise didn't seem to work. 


## ==== NOTE ==== This will have the first 6 months of 2003 in it, so we'll need to filter those out by hand.

  for(i in 1:length(years)){
    yr = years[i]
    qry = flights %.% group_by(year, month, origin) %.%
      summarise(ttl_del=sum(weatherdelay), avg_del=mean(weatherdelay), sd_del=sd(weatherdelay),
                n_flights=n(), 
                n_wdelay=sum(if(weatherdelay>0) {1} else {0})
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

  head(delay)
  max(delay$n_wdelay)
  min(delay$n_wdelay)
 

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
  
  reg = read.csv("data/State_Region.csv", header=T) # see original file: added "other" region label for military bases and protectorates
  reg = reg %.%
    mutate(state=State, region=Region) %.%
    select(state, region)
  
  # add region label to each entry:
  delay.region = left_join(delay.state, reg, by="state")

  delay.region = arrange(delay.region, origin, year, month)
  dim(delay.region)
  head(delay.region, n=30L)

## save the data:
  write.csv(delay.region, "data/popn_summary_by_region.csv", row.names=FALSE) # this data set contains popl'n summary information, with region labels added



#----------------------------------------------------------------------------#



          ### Matt's code for regional population analysis ###
                        ### updated 5/4 ###

  pd.f = delay.region

## Add Date, group by date & region
  by.month = pd.f %.% group_by(year, month, region) %.%
    summarise(mean_del = sum(avg_del*n_flights)/sum(n_flights),
      n_all=sum(n_flights), n_del=sum(n_wdelay), p=n_del/n_all) 


## add date: each month is identified by the first day of the month
  by.month = mutate(by.month, dt.str=paste(year, month, "1", sep="-"))
  str(by.month)
  by.month$date = ymd(by.month$dt.str)
  str(by.month)


## write this to file so it can be recalled later
  write.csv(by.month, "data/Popn_summary.csv", row.names=F)


## plot by region

  ggplot(by.month, aes(y=mean_del, x=date, group=region, colour=region))+geom_line()+ggtitle("Average of Delayed Flights by Region")

  ggplot(by.month, aes(y=p, x=date, group=region, colour=region))+geom_line()+ggtitle("Proportion of Delayed Flights by Region")



