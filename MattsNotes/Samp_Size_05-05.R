library(dplyr)

p.by.r <- read.csv("data/popn_summary_by_region.csv", header=T, stringsAsFactors=F)
36190/144
tbl_df(p.by.r)


summ.by.r <- p.by.r %.% group_by(region, year, month) %.%
  summarise(n_flights=sum(n_flights), sum_del=sum(ttl_del), n_delay=sum(n_wdelay) )

tbl_df(summ.by.r)

hist(summ.by.r$n_flights)

summ.by.r <-ungroup(summ.by.r)
reg <- summ.by.r %.% group_by(region) %.% 
  summarise(avg_n_flights=mean(n_flights), avg_dly_month = mean(sum_del, na.rm=T), sd_dly_mon = sd(sum_del, na.rm=T))
#    Source: local data frame [12 x 2]
#    
#    region avg_flights
#    1        Alaska    3410.354
#    2       Central   84563.787
#    3        Hawaii    8323.890
#    4  NorthRockies    6489.441
#    5     Northeast   75828.803
#    6     Northwest   17963.425
#    7         Other    3031.472
#    8         South   77933.701
#    9     Southeast  115320.504
#    10    Southwest   53104.520
#    11 UpperMidwest   32798.299
#    12         West   81408.622

reg2 <- reg %.% mutate( n.025 = avg_n_flights*0.025)
reg2
sum(reg2$n.025)*10 =# 140,000 lines of data if we do an approximate 2.5% sample.
