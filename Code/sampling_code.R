

                 ###------ stratified sampling ------###


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

# sample for 2003, June to Dec:
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


# Sample for all other years: 2004--2013
  for(i in 6:11){
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
  
  # for 2003: 95129 oobs
  # yrs 2:5 :816725 ttl
  # yrs 6:11
  

# running for 1 year 2003, got 160,000 lines? that's a weeee bit more than the 14,000 I was expectin
  head(rand.data,n=30L)
  nrow(rand.data)
  rand.2003 <- left_join(rand.data, iata.region, on="origin")
  summ.2003 <- rand.2003 %.% group_by(Region) %.%  summarise(n=n())
#looks like we pulled around 30% on average, instead of 2%
# replacing "random() < 0.025" with "random < 0.0025"
  ## update - former is because i was looking at 2.5% for a month, vs 2.5% for a year. oops- m.e.

  write.csv(rand.data, "data/sample_data_2-5pct.csv", row.names=F)



  
  
  
#---------------------------------------------------------------------------#  
  
  
  
  
  
                    #### sample data analysis ####
  
  dat = read.csv("data/sample_data_2-5pct.csv", header=T)
  dim(dat)
  head(dat, n=30L)
  
## add region label to each observation:
  reg = read.csv("data/iata_by_region.csv", header=T)
  dat.region = left_join(dat, reg, by="origin")
  
  head(dat.region, n=30L)
  

## pull information on stratum sizes:
  popn.strat.size = read.csv("data/popn_strata_size.csv", header=T)
  tbl_df(popn.strat.size)
  popn.size = popn.strat.size %.%
    mutate(Region=region) %.%
    select(Region, year, month, strat.size) 
  
  tbl_df(popn.size)
  
  by.region = dat.region %.%
    group_by(Region, year, month) %.%
    summarise(n_flights=n(), n_delay=sum(weatherdelay>0,na.rm=T)) %.%
    mutate(prop_delay=n_delay/n_flights)  
  tbl_df(by.region)
    
  
## include the strata size info into sample summary:
  by.region.size = left_join(by.region, popn.size)
  tbl_df(by.region.size)
  
  
  by.region.summary = by.region.size %.%
    mutate(prop_se = ((1-n_flights/strat.size)*prop_delay*(1-prop_delay)/(n_flights-1))^(1/2))  
  tbl_df(by.region.summary)
  
  
## pointwise confidence intervals
  z = 1.96
  
  by.region.ci = by.region.summary %.%
    mutate(lower.bound = prop_delay-z*prop_se, upper= ifelse(prop_delay==0, 3/n_flights, prop_delay+z*prop_se)) %.%
    mutate(lower = ifelse(lower.bound<=0, 0, lower.bound)) %.%
    select(Region:prop_se,upper,lower)
  # comment: for zero proportions, we used "rule of three" to construct 95% CI, see
  # wikipage: http://en.wikipedia.org/wiki/Rule_of_three_%28statistics%29
  
  tbl_df(by.region.ci)
  
  write.csv(by.region.ci, "data/sample_summary_ci.csv", row.names=F)
  
  
  
  
#-----------------------------------------------------------------------------#  
  
  

            #### plot proportion and confidence limits ####
  
  samp.ci = read.csv("data/sample_summary_ci.csv", header=T)

  
## add date: each month is identified by the first day of the month
  library(lubridate)
  
  samp.ci = mutate(samp.ci, dt.str=paste(year, month, "1", sep="-"))
  
  samp.ci$date = ymd(samp.ci$dt.str)
  
  tbl_df(samp.ci)

  sum(is.na(samp.ci$prop_delay))

  
  
## plot by region

  library(ggplot2)
  library(reshape2)

  samp.ci$date = as.Date(samp.ci$date)
  tbl_df(samp.ci)

  
  ggplot() +
    geom_line(samp.ci, mapping = aes(y=prop_delay, x=date, group=Region), colour="#660033") +
    geom_line(samp.ci, mapping = aes(y=upper, x=date, group=Region), linetype="blank") +
    geom_ribbon(samp.ci, mapping=aes(x=date, ymin=prop_delay, ymax=upper), fill="cyan", alpha=0.5) +
    geom_ribbon(samp.ci, mapping=aes(x=date, ymin=-0.003, ymax=prop_delay), fill="cyan", alpha=0.5) +
    facet_wrap(~Region, nrow=4) +
    ggtitle("Proportion of Flights Delayed Due to Weather by Region, 6/2003 - 12/2013") +
    theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none") +
    scale_x_date() +
    scale_y_continuous(limits=c(-0.05,0.15))

  ggsave("Images/samp_prop.png", width=8, height=5, units="in", dpi=400)



## plot for south alone
  
  sou = filter(samp.ci, Region=="South")
  
  qplot(date, prop_delay, data = sou, geom = "line") +
    ggtitle("Proportion of Delayed Flights per Month, South (6/2003-12/2013)") +
    geom_smooth(method = "loess", se=F) +
    theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none")

  ggsave("Images/samp_prop_south.png", width=8, height=5, units="in", dpi=400)
  
  
  
  
  
