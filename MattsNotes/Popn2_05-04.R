## 5/4 - still working on the problem of the proportion information not pulling down.
# decided to start fresh in a fresh file to see what I could do.

#UPDATE - my last bit of code seems to be working. Now to incorporate it into the main loop and see what happens.

library(dplyr)
endpoint <- "flights.cwick.co.nz"
user <- "student"
password <- "password"

ontime <- src_postgres("ontime", 
                       host = endpoint,
                       port = 5432,
                       user = user,
                       password = password)

flights <- tbl(ontime, "flights")

## Pull all transactions for a month (?) for one airport
pdx.qry <- flights %.% select(year, month, origin, uniquecarrier, weatherdelay)%.% 
  filter(year=="2003", month==10, origin=="PDX")
explain(pdx.qry)

system.time(pdx.all <- collect(pdx.qry)) ##4753 obs; 5 seconds
head(pdx.all)

sum(pdx.all$weatherdelay) ##737, there are no NA's
sum(pdx.all$weatherdelay > 0) # there are 10 greater than 0
pdx.all$weatherdelay[which(pdx.all$weatherdelay > 0)]

## confirm that the n counted by the summmary function = n I get manually
Oct10.qry <- flights %.% group_by(year, month, origin) %.%
  summarise(n=n(), ttl_dly=sum(weatherdelay))%.% 
  filter(year=="2003", month==10)
explain(Oct10.qry)

system.time(Oct10.dat <- collect(Oct10.qry))  #4.68 seconds, 267 obs

Oct10.dat[which(Oct10.dat$origin=="PDX"),]
# n = 4753, sum = 737, which match.

#
#   year month origin    n ttl_dly
#32 2003    10    PDX 4753     737
#


# try the count (if weatherdelay > 0 1, else 0)

Oct10p.qry <- flights %.% group_by(year, month, origin) %.%
  summarise(n=n(), ttl_dly=sum(weatherdelay), avg_delay=mean(weatherdelay),
            n_del=sum(if(weatherdelay>0) { 1} else {0})
            )%.% 
  filter(year=="2003", month==10)
explain(Oct10p.qry) ## that is actually doing what I wanted

system.time(Oct10p.dat <- collect(Oct10p.qry)) ## 4.79seconds
  # 267 obs, 7 vars

Oct10p.dat[which(Oct10.dat$origin=="PDX"),]
# THIS WAS WITH COUNT(if(weatherdelay>0) { 1} else {0}) 
# -----------------------
#   year month origin    n ttl_dly avg_delay n_del
#32 2003    10    PDX 4753     737   0.15506  4753
#

# ran same code only with sum(if(weatherdelay>0) { 1} else {0})
# -------------------------
#   year month origin    n ttl_dly avg_delay n_del
#32 2003    10    PDX 4753     737   0.15506    10

#
#  that did it. HOoray! **********************
#


##======================================###

# Try for a month with NA's in it'


## Pull all transactions for a month (?) for one airport
pie06.qry <- flights %.% select(year, month, origin, uniquecarrier, weatherdelay)%.% 
  filter(year=="2003", month==5, origin=="VLD")
explain(pie06.qry)

system.time(pieJun.all <- collect(pie06.qry)) ## 1.25, 224 obs
head(pieJun.all)


sum(pieJun.all$weatherdelay)

## when I was messing around on Friday, I found a month & carrier with some NA and some valid values in it. not finding it now.
# so, am just goign to pull data using my code for one year, and look at it.


## was going to test this, but couldn't find a month that worked. 

Feb03.qry <- flights %.% group_by(year, month, origin) %.%
  summarise(n=n(), ttl_dly=sum(weatherdelay), avg_delay=mean(weatherdelay),
            n_del=sum(if(weatherdelay>0) { 1} else {0}),
            n_nna=sum(if(weatherdelay != NA){ 1} else {0})
  )%.% 
  filter(year=="2003", month==5)
explain(Feb03.qry)

## From running the population level data for 2003, it looks like the "n_not_Na" query isn't working. We're just going to have to trust that they have weather delay data on everything.





