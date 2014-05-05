### Population Analysis
# Matt E - 5/2
library(dplyr)
library(lubridate)
library(ggplot2)

popn.data <- read.csv("data/popn_weather_data.csv", header=T)
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
str(by.month)
by.month$date <- ymd(by.month$dt.str)
str(by.month)

# write this to file so it can be recalled later (hopefully the date stuff doesn't mutate.)
write.csv(by.month, "data/Popn_summary.csv", row.names=F)


## plot by region
ggplot(by.month, aes(y=mean_del, x=date, group=region, colour=region))+geom_line()+ggtitle("Average of Delayed Flights by Region")

ggplot(by.month, aes(y=p, x=date, group=region, colour=region))+geom_line()+ggtitle("Proportion of Delayed Flights by Region")
