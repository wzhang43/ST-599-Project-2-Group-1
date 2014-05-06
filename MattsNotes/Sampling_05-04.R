## 5.4 - matt sampling. 

## i have concerns about what was done.
# explore Nandhita's sampling results file.

n.samp <- read.csv("data/samp13_14.csv", header=T)
n.samp

#e-mail & file says "2013 & 2014" but results say "2003 & 2004"

sum(n.samp$n_flights)

# what she did, was sample 10,000 flights over the entire year, which ended with some months/regions getting 1 flight. not acceptable


# ---------------------------------------------------------------------------------------#

# To query by region, which is our sampling unit, we should be able to do something like:
# WHERE (origin IN <list> )


library(dplyr)

iata.region = read.csv("data/iata_by_region.csv", header=T, stringsAsFactors=F)

# fiddling with pulling regional lists of airports
west.list <- iata.region%.%filter(Region=="West")%.%select(origin)

by.reg <- iata.region%.%group_by(Region)%.%summarise(n=n())
sum(by.reg$n) # that is the 360 airports we expect.
# summarise(iata.region)


## I want a list of lists for regions

region.df <- tbl_df(read.csv("data/iata_by_region.csv", header=T, stringsAsFactors=F))
region.df

nw.list <- region.df[which(region.df$Region=="Northwest"),1]

r.list = list()

r.list = c(r.list, nw.list)

w.list <- region.df[which(region.df$Region=="West"),1]
r.list = c(r.list, w.list) ## This does not work
r.list



str(nw.list)
str(w.list)
r.list <- list(nw.list, w.list) # this does work. now, do i have to do each region by hand? or can I loop it?
# it will be quicker if i just do each region by hand.
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


r.list <- list(ak.list, c.list, hi.list, nr.list, ne.list, nw.list, so.list, se.list, sw.list, uk.list, um.list, we.list)


o.list <- r.list[[1]]

qry = flights %.% group_by(year, month, origin) %.%
  summarise( ttl_del=sum(weatherdelay), avg_del=mean(weatherdelay), sd_del=sd(weatherdelay),
             n_flights=n(), 
             n_wdelay=sum(if(weatherdelay>0) {1} else {0})
  ) %.%
  filter(year==yr, month==10, origin %in% o.list )
explain(qry)

### this is generating a valid query. lets see what happens

system.time(reg.dat <- collect(qry))
sum(reg.dat$n_flights) ## 16994
sum(reg.dat$n_wdelay) # 47

## it works!

# --------------------------------------------------------- ##

# Updated to sample within a region for a specific month and year, since that is the sampling unit we discussed.
# Nandhita's code also is sampling from the Data.csv? which is a 19mb file which looks like it has every flight in it from 2003/2004. That's not going to work for what we need. We need to sample in the database, rather than pulling all the data, sampling from that then going back to the DB. That's horribly inefficent, and kind of the opposite of what we're trying to learn, IMO

# 
samp.data = data.frame()
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

# how to reference
# o.list <- r.list[[1]]

# j = 12 months
# k = 12 regions

# test:
i<-j<-k<-2
# ignoring the for loop, explain(qry) query was good for these years

# test 1 - got "error: index out of bounds". removed "random()<0.001" from filter and tried again
#   that was successful.

for(i in 1:length(years)){
  yr = years[i]
  mo = j
  o.list = r.list[[k]]
  qry = flights %.% group_by(year, month, origin) %.%
    summarise( ttl_del=sum(weatherdelay), avg_del=mean(weatherdelay), sd_del=sd(weatherdelay),
               n_flights=n(), 
               n_wdelay=sum(if(weatherdelay>0) { 1} else {0})
    ) %.%
    filter(year==yr, month==mo, origin %in% o.list )
  
  qry.rand = arrange(qry, random())
  
  system.time(wd.new <- head(qry.rand, n=1000L))
  weather.data = rbind(weather.data, wd.new)
}


system.time(cen.Feb2002 <- collect(qry))

## this is essentially summarizing, and then attempting to take a random sample of the summaries, only there aren't enough of them.

# i think we're going to have to sample individual flights, then summarize on our side.


yr = years[i]
mo = j
o.list = r.list[[k]]

qry = flights %.% select(year, month, origin, weatherdelay) %.%
  filter(year==yr, month==mo, origin %in% o.list, random() < 0.01)


system.time(wd.new <- collect(qry)) # 12 sec
# that only gave me 106 observations out of 96058
sum(cen.Feb2002$n_flights)

wd.new %.% group_by(origin) %.% summarise(n=n())

# 1000/96058 = 0.01
qry = flights %.% select(year, month, origin, weatherdelay) %.%
  filter(year==yr, month==mo, origin %in% o.list, random() < 0.01)


system.time(wd.new <- collect(qry)) # 22 sec, this time 974 observations.


qry = flights %.% select(year, month, origin, weatherdelay) %.%
  filter(year==yr, month==mo, origin %in% o.list)

qry.rnd <- arrange(qry, random())
system.time(cen.s3 <- head(qry.rnd, n=1000L)) #28.07

# I think we're going to have to go with the 2nd one, because otherwise, we can't control how many observations we get.


## test a for loop

rand.data <- data.frame()
n.samp <- 1000L

#for(i in 1:length(years)){  # years
for(i in 1:1){
  for(j in 1:12){           # months
    for(k in 1:12) {        # Regions
      yr = years[i]
      mo = j
      o.list = r.list[[k]]
      
      qry = flights %.% select(year, month, origin, weatherdelay) %.%
        filter(year==yr, month==mo, origin %in% o.list)
      
      qry.rnd <- arrange(qry, random())
      
      dat.temp <- head(qry.rnd, n=n.samp) # can adjust the size if we want to stratify proportionally
      
      dat.summ <- dat.temp %.% group_by(origin, year, month) %.% 
        summarise(
                  ttl_del=sum(weatherdelay, na.rm=T), 
                  avg_del=mean(weatherdelay, na.rm=T), 
                  sd_del=sd(weatherdelay, na.rm=T),
                  n_flights=n(), 
                  n_wdelay=sum(weatherdelay > 0) # we might want to leave the summarization till later, and only retain the original observations
                  ) 

      rand.data <- rbind(rand.data, dat.summ)
      
    } 
  }
}

## this worked, I didn't time it, but expect it took over a minute.

ita.reg <- read.csv("data/iata_by_region.csv", header=T, stringsAsFactors=F)
rand.data <- left_join(rand.data, ita.reg, by="origin")

rand.data %.% ungroup() %.% group_by(Region, year, month) %.% 
  summarise(ttl_del=sum(ttl_del), avg_del=mean(avg_del, na.rm=T), n_flights=sum(n_flights),
            n_wdelay=sum(n_wdelay, na.rm=T))

## this gives me a 144 row table, which is 12 months x 12 REgions.


# ---------------------------------- #
# modify teh loop to pull 2.5% of observations per strata, and to not summarize.

rand.data <- data.frame()
n.samp <- 1000L

#for(i in 1:length(years)){  # years
for(i in 9:11){
  for(j in 1:12){           # months
    for(k in 1:12) {        # Regions
      yr = years[i]
      mo = j
      o.list = r.list[[k]]
      
      qry = flights %.% select(year, month, origin, weatherdelay) %.%
        filter(year==yr, month==mo, origin %in% o.list, random()<0.0025)
      
      dat.temp <- collect(qry)
      
      rand.data <- rbind(rand.data, dat.temp)
      
    } 
  }
}

# running for 1 year 2003, got 160,000 lines? that's a weeee bit more than the 14,000 I was expectin
head(rand.data)
rand.2003 <- left_join(rand.data, iata.region, on="origin")
summ.2003 <- rand.2003 %.% group_by(Region) %.%  summarise(n=n())
#looks like we pulled around 30% on average, instead of 2%
# replacing "random() < 0.025" with "random < 0.0025"

## more like expected.
# run for yrs 2-4 - have 70276 obs.
# run for yrs 5-8 - hae 139,033
# run for yrs 9-11 - now 185,527 obs total.

write.csv(rand.data, "data/sample_data.csv", row.names=F)
