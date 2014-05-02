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

## ==== NOTE ==== This will have the first 6 months of 2003 in it, so we'll need to filter those out by hand.

  for(i in 1:length(years)){
    yr = years[i]
    qry = flights %.% group_by(year, month, origin) %.%
      summarise(avg_del=mean(weatherdelay), n_flights=n(), n_wdelay=count(weatherdelay>0)) %.%
      filter((year==yr))
  
    wd.new = collect(qry)
    weather.data = rbind(weather.data, wd.new)
  }


## write to file so we don't have to run query again.

  write.csv(weather.data, "data/popn_weather_data.csv", row.names=FALSE)



#-------------------------------------------------------------------------#

  weather = read.csv("data/popn_weather_data.csv", header=T)

### subset dataset:
  delay = filter(weather, is.na(avg_del)==FALSE) # get rid of all NA's in weatherdelay

  dim(delay)
  # collect(delay)
 

#-------------------------------------------------------------------------#


  
### information on airport IATA code and belonging state:
  airinfo = read.csv("data/airports.csv",header=T) # used "airports.csv" from class website

  air.state = airinfo %.%
    mutate(origin=iata) %.%
    select(origin, state)

  unique(air.state$state) # codes other than US states exist

  delay.state = inner_join(delay, air.state, by="origin") # join two tables to include states
  dim(delay.state) # seems like some of "origins" in the original dataset don't fall into any of the states listed in "airports.csv"
  


### organize all states into regions:
### got this from: http://www.nashua.edu/novakc/regions/Generalinformation.htm

  midwest = c("IL","IN","IA","KS","MI","MN","MO","NE","ND","SD","OH","WI")
  northeast = c("CT","DE","ME","MD","MA","NH","NJ","NY","PA","RI","VT")
  west = c("AK","CA","CO","HI","ID","MT","NV","OR","UT","WA","WY")
  southwest = c("AZ","NM","OK","TX")
  southeast = c("AL","AR","FL","GA","KY","LA","MS","NC","SC","TN","VA","WV")
  allstates = c(midwest, northeast, west, southwest, southeast)


### create regions variable:
  n = nrow(delay.state)
  region = c()
  length(region) = n

  for(i in 1:n){
    if (delay.state$state[i] %in% midwest) {
      region[i] = "MW"
    } else if (delay.state$state[i] %in% northeast) {
      region[i] = "NE"
    } else if (delay.state$state[i] %in% west) {
      region[i] = "W"
    } else if (delay.state$state[i] %in% southwest) {
      region[i] = "SW"
    } else if (delay.state$state[i] %in% southeast) {
      region[i] = "SE"
    } else {
      region[i] = "unknown" # includes NA, military bases and protectorates
    } 
  }


### add new column to data:
  delay.reg = mutate(delay.state, region=region)


#----------------------------------------------------------------------------#










