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



#-------------------------------------------------------------------------#



### subset dataset:
  delay = flights %.%
    arrange(year, month, dayofmonth) %.% 
    select(year, month, dayofmonth, weatherdelay, origin) %.%
    filter(is.na(weatherdelay)==FALSE) # get rid of all NA's in weatherdelay

  # collect(delay)



#-------------------------------------------------------------------------#


  
### information on airport IATA code and belonging state:
  airinfo = read.csv("data/airports.csv",header=T) # used "airports.csv" from class website
  air.state = select(airinfo, iata, state)

  unique(air.state$state) # codes other than US states exist


### organize all states into regions:
### got this from: http://www.nashua.edu/novakc/regions/Generalinformation.htm

  midwest = c("IL","IN","IA","KS","MI","MN","MO","NE","ND","SD","OH","WI")
  northeast = c("CT","DE","ME","MD","MA","NH","NJ","NY","PA","RI","VT")
  west = c("AK","CA","CO","HI","ID","MT","NV","OR","UT","WA","WY")
  southwest = c("AZ","NM","OK","TX")
  southeast = c("AL","AR","FL","GA","KY","LA","MS","NC","SC","TN","VA","WV")


### create regions variable:
  n = nrow(delay) # CAUTION: this takes a LONG time
  region = c()
  length(region) = n

  for(i in 1:n){
    if (delay$state[i] %in% midwest) {
      region[i] = "MW"
    } else if (delay$state[i] %in% northeast) {
      region[i] = "NE"
    } else if (delay$state[i] %in% west) {
      region[i] = "W"
    } else if (delay$state[i] %in% southwest) {
      region[i] = "SW"
    } else if (delay$state[i] %in% southeast) {
      region[i] = "SE"
    } else {
      region[i] = "unknown" # includes NA, military bases and protectorates
    } 
  }


### add new column to data:
  delay.reg = mutate(delay, region=region)


#----------------------------------------------------------------------------#










