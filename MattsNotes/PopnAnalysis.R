### Population Analysis
# Matt E - 5/2


popn.data <- read.csv("data/popn_weather_data.csv", header=T)
head(popn.data)

#sort by year
#http://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns-in-r
popn.data <- popn.data[with(popn.data, order(year, month, origin)),]

#filter out NA's
pd2 <- popn.data %.% filter( avg_del != "NA" )
head(pd2)

## got waylaid by the fact that the n_wdelay was just counting the # of weather delay variables that weren't "NA". in otherwords, I have to redo the data pull once I figure out how to do it right.
