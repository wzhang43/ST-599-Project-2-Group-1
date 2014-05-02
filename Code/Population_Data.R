### Code that allowed me to pull population level data


library(dplyr)


## basic DB access set up, as provided by Charlotte
endpoint <- "flights.cwick.co.nz"
user <- "student"
password <- "password"

ontime <- src_postgres("ontime", 
                       host = endpoint,
                       port = 5432,
                       user = user,
                       password = password)

flights <- tbl(ontime, "flights")


## Setup for query cycle
weather.data <- data.frame()
years <- as.character(seq(2003, 2013,1))

## It was taking really long to try and do all 10 years at once, so I broke it down into year by year
## it takes about 80 seconds per year

## ==== NOTE ==== This will have the first 6 months of 2003 in it, so we'll need to filter those out by hand.

for(i in 1:length(years)){
  yr <- years[i]
  qry <- flights %.% group_by(year, month, origin) %.% 
    summarise(avg_del=mean(weatherdelay), n_flights=n(), n_wdelay=count(weatherdelay>0)) %.% 
    filter((year==yr))
  
  wd.new <- collect(qry)
  weather.data <- rbind(weather.data, wd.new)
}


## write to file so we don't have to run query again.

write.csv(weather.data, "data/popn_weather_data.csv", row.names=FALSE)