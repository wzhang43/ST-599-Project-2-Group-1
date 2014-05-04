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

flights = tbl(ontime, "flights")

mon <- filter(flights, (weatherdelay>0))%.%summarise(year,month,weatherdelay, origin)
explain(mon)
#mon[sample(nrow(mon), 100), ]     sampling didnt work
mon_local <- collect(mon) #935373 rows
write.csv(mon_local, "c:/Users/Nandhita/Desktop/FilteredFullData.csv", row.names=FALSE)

Full = read.csv("c:/Users/Nandhita/Desktop/FilteredFullData.csv",stringsAsFactors = FALSE)
Regions = read.csv("c:/Users/Nandhita/Desktop/iata_by_region.csv", ,stringsAsFactors = FALSE)
mon_names = inner_join(Full, Regions, by="origin") #935373 rows

Data = subset(mon_names, select=c('year','month','weatherdelay','Region'))
write.csv(Data, "c:/Users/Nandhita/Desktop/Data.csv", row.names=FALSE)

v1=summarize(group_by(Data, month, year), number=n())

#one month
tmp=subset(Data, year==2011 & month==1)

summ = tmp[sample(nrow(tmp), 2000), ] %.% group_by(year, month, Region) %.%  #2000 samples
  summarise(avg_del=mean(weatherdelay), n_flights=n())

#for years 2003 and 2004 - test

Data = read.csv("c:/Users/Nandhita/Desktop/Data.csv", ,stringsAsFactors = FALSE)
samp = data.frame()
years = as.character(seq(2003, 2004,1)) 
for(i in 1:length(years)){
  yr = years[i]
  tmp=subset(Data, year==yr)
  qry = tmp[sample(nrow(tmp), 10000), ]%.% group_by(year, month, Region) %.% # sample size=10000 for each year
    summarise(avg_del=mean(weatherdelay), n_flights=n()) %.%
    filter((year==yr))
  
  samp.new = collect(qry)
  samp = rbind(samp, samp.new)
}
write.csv(samp, "c:/Users/Nandhita/Desktop/samp13_14.csv", row.names=FALSE)
