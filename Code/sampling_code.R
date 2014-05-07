

        ### stratified sampling ###


### acquiring population data
library(dplyr)

## connecting to the database:
  endpoint = "flights.cwick.co.nz"
  user = "student"
  password = "password"

  ontime = src_postgres("ontime", 
    host = endpoint,
    port = 5432,
    user = user,
    password = password)


## convert table "flights" into a tbl_df():
  flights = tbl(ontime, "flights")


#-------------------------------------------------------------------#

            #### Matt's code for stratified sampling ####

# initiate for loop
  years = as.character(seq(2003, 2013,1))

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


rand.data <- data.frame()

for(j in 6:12){           # months
    for(k in 1:12) {        # Regions
      yr = years[1]
      mo = j
      o.list = r.list[[k]]
      
      qry = flights %.% select(year, month, origin, weatherdelay) %.%
        filter(year==yr, month==mo, origin %in% o.list, random()<0.025)
      
      dat.temp <- collect(qry)
      
      rand.data <- rbind(rand.data, dat.temp)
      
    } 
  }

#for(i in 1:length(years)){  # years
for(i in 2:11){
  for(j in 1:12){           # months
    for(k in 1:12) {        # Regions
      yr = years[i]
      mo = j
      o.list = r.list[[k]]
      
      qry = flights %.% select(year, month, origin, weatherdelay) %.%
        filter(year==yr, month==mo, origin %in% o.list, random()<0.025)
      
      dat.temp <- collect(qry)
      
      rand.data <- rbind(rand.data, dat.temp)
      
    } 
  }
}

# running for 1 year 2003, got 160,000 lines? that's a weeee bit more than the 14,000 I was expectin
head(rand.data,n=30L)
nrow(rand.data)
rand.2003 <- left_join(rand.data, iata.region, on="origin")
summ.2003 <- rand.2003 %.% group_by(Region) %.%  summarise(n=n())
#looks like we pulled around 30% on average, instead of 2%
# replacing "random() < 0.025" with "random < 0.0025"

## more like expected.
# run for yrs 2-4 - have 70276 obs.
# run for yrs 5-8 - hae 139,033
# run for yrs 9-11 - now 185,527 obs total.

write.csv(rand.data, "data/sample_data.csv", row.names=F)




qry = flights %.% select(year, month, origin, weatherdelay) %.%
        filter(year==2006, month==4, random()<0.0025)


explain(qry)



