### Population Analysis
# Matt E - 5/2
library(dplyr)
library(lubridate)
library(ggplot2)


#### -----------------------------------------------------------------------
##    5/4
##  NOTE: Wanli updated the file 5/5
##
#### -----------------------------------------------------------------------


popn.data <- read.csv("data/popn_weather_data.csv", header=T, stringsAsFactors=F)
head(popn.data, n=50)



#filter out NA's
pd2 <- popn.data %.% filter( avg_del != "NA" )
pd2 <- tbl_df(pd2)
head(pd2, n=50)

## got waylaid by the fact that the n_wdelay was just counting the # of weather delay variables that weren't "NA". in otherwords, I have to redo the data pull once I figure out how to do it right.
## Update - 5/4 -  repulled the pop'n data


#sort by year
#http://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns-in-r
popn.data <- popn.data[with(popn.data, order(year, month, origin)),]

## Add region
iata.file <- read.csv("data/iata_by_region.csv", header=T)

regions <- iata.file$Region
names(regions) <- iata.file$origin

pd.f <- mutate(pd2, region = regions[origin])


# verifying that the overall mean is the mean of the class means
s <- seq(1:50)
c <- rep(c(1,2), 25)
df <- data.frame(s=s, c=c)
mean(s)
df %.% group_by(c) %.% summarise(avg=mean(s))



## Add Date, group by date & region
by.month <- pd.f%.%group_by(year, month, region)%.%
  summarise(mean_del = mean(avg_del), n_all=sum(n_flights), n_del=sum(n_wdelay), p=n_del/n_all
)


by.month <- mutate(by.month,  dt.str=paste(year, month, "1", sep="-"))
#str(by.month)
by.month$date <- ymd(by.month$dt.str)
#str(by.month)

# write this to file so it can be recalled later (hopefully the date stuff doesn't mutate.)
write.csv(by.month, "data/Popn_summary.csv", row.names=F)






## plot by region
ggplot(by.month, aes(y=mean_del, x=date, group=region, colour=region))+geom_line()+ggtitle("Average of Delayed Flights by Region")

ggplot(by.month, aes(y=p, x=date, group=region, colour=region))+geom_line()+ggtitle("Proportion of Delayed Flights by Region")


## Sample size calculations - these are wrong.
head(by.month)
mean(by.month$mean_del) # 2.21
sd(by.month$mean_del) #1.91497
mean(by.month$p) #0.012


# not useful
ggplot(by.month, aes(x=month, y=mean_del, colour=region))+geom_point()


# lets look at by region just aggregated to month
by.month.ng <- ungroup(by.month)

by.reg <- by.month.ng %.% group_by(region, month) %.%
  summarise(mean_del.r=mean(mean_del), n_all.r=sum(n_all), n_del.r=sum(n_del), p=n_del.r/n_all.r)

ggplot(by.reg, aes(y=region, x=mean_del.r, colour=month))+geom_point()+geom_line()
#   This has lines within region, not useful

ggplot(by.reg, aes(y=region, x=mean_del.r, colour=month))+geom_point()+geom_line(aes(group=month))
#   this is a gobbeldy-gook mess.



ggplot(wage.data.st, aes( reorder(factor(State),med_sch),y=med_sch, ymin=Q1_sch, ymax=Q3_sch, group=MIL, colour=factor(MIL_Status)))+geom_point(size=4, position=position_dodge(width=0.4))+
  geom_errorbar( position=position_dodge(width=0.4), size=0.5)+
  xlab("State")+ylab("School")+geom_line()+
  scale_y_continuous(breaks=c(16,17,18,19,20,21,22, 23), labels=c("HS", "GED", "<1YR Coll", "Coll, ND", "AS", "BS", "MS", "Prof >BS"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Median School (with IQR) by State & Service")
=======
## --------------------------------##
# Population proportions

p.data <- popn.data %.% filter( !(year==2003 & month < 6)) # grab everything that is outside the range where they collected this data.
p.data %.% group_by(month) %.% summarise(n=n()) 

region.lst <- read.csv("data/iata_by_region.csv", header=T, stringsAsFactors=F)

p.data <- tbl_df(left_join(p.data, region.lst, on="origin"))
p.data

by.reg.yr.mon <- p.data %.% group_by(Region, year, month) %.% summarise(n_ttl=sum(n_flights), n_del=sum(n_wdelay), p=n_del/n_ttl)

#okay, so the graphs are going to look at proportion in a single region, through time.
#we need a way to compare proportions between geographical regions.


# tables package? http://www.r-statistics.com/tag/tables/

by.reg.yr.mon <- ungroup(by.reg.yr.mon)

by.reg.mon <- by.reg.yr.mon %.% group_by(Region, month) %.% summarise(n_ttl=sum(n_flights), n_del=sum(n_wdelay), p = n_del/n_ttl)
by.reg.mon

by.reg.mon <- ungroup(by.reg.mon)

library(dplyr)
library(tables)
tabular( (factor(Region)+1)~ factor(month)*(n_del), data=by.reg.mon)


tabular( (cat+1)~mon*(Format(digits=2)*dat), data=dat.fam)

# it's wanting some function for the data in the cells, and Its taking too long. I can't figure out how to just have it to a varaible.  just going to do it in excel.

write.csv(by.reg.mon, "data/regon_by_month_summ.csv", row.names=F)

# yay pivot tables. <10 minutes, done.

#### 5/9

by.reg.yr.mon <- ungroup(by.reg.yr.mon)
by.reg.yr <- by.reg.yr.mon %.% group_by(month, Region) %.% summarise(avg_p=mean(p))
by.reg.yr

write.csv(by.reg.yr, "data/mon_by_reg_summ.csv", row.names=F)


samp.dat <- tbl_df(read.csv("data/sample_summary_ci.csv"))

## adding up flights by region and month, and then determining proportion and SE for each strata.
samp.mon.reg <- samp.dat %.% group_by(month, Region) %.% summarise( n_ttl=sum(n_flights), n_del=sum(n_delay), p=n_del/n_ttl, N.h = sum(strat.size), se.p=sqrt((1-n_ttl/N.h)*((p*(1-p))/(n_ttl-1)) ))
samp.mon.reg

write.csv(samp.mon.reg, "data/samp_mon_by_reg_summ.csv", row.names=F)
