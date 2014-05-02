### for Nandhita working on the sampling
library(dplyr)
iata.region <- read.csv("data/iata_by_region.csv", header=T)

#this will give you an array of the airports in the West region. You may have to fiddle with how to get it to work with the SQL statements
west.list <- iata.region%.%filter(Region=="West")%.%select(origin)
