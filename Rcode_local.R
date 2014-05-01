library(dplyr)

### acquiring the database:
endpoint <- "flights.cwick.co.nz"
user <- "student"
password <- "password"

ontime <- src_postgres("ontime", 
  host = endpoint,
  port = 5432,
  user = user,
  password = password)



### convert table "flights" into a tbl_df():
flights <- tbl(ontime, "flights")
str(flights)



### check from when BTS started collecting weather delay records:
temp = filter(flights, year == "2003")
weather = temp %.% select(month, dayofmonth, weatherdelay) %.% arrange(month, dayofmonth)
weather_local = collect(weather)
head(weather_local)

start = filter(weather_local, weatherdelay!="NA")
head(start) # so it's from 6/1/2003 that weather delay records began to be recorded







