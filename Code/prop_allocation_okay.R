

  library(dplyr)

  p <- read.csv("data/popn_summary_by_region.csv", header=T, stringsAsFactors=F)

  tbl_df(p)
  
  tail(p, n=30L)

  strat.p = p %.% 
    group_by(region, year, month) %.% 
    summarise(strat.prop = sum(n_wdelay)/sum(n_flights), strat.size = sum(n_flights), strat.ttl = sum(n_wdelay)) %.%
    arrange(region, year, month)
  tbl_df(strat.p)
  
  strat.p = mutate(strat.p, strat.var = strat.size*strat.prop*(1-strat.prop)/(strat.size-1)) # sample variance for each stratum
  
  
## looked at Lohr Chapter 4.4.1, compare SSB and another quantity to decide if prop allocation is more efficient:  
  
  popn.avg = sum(strat.p$strat.ttl)/sum(strat.p$strat.size)
    
  SSB = strat.p %.% ungroup() %.% summarise(ssb = sum(strat.size*(strat.prop-popn.avg)^2))
  
  popn.size = sum(strat.p$strat.size)
  
  rhs = sum((1-strat.p$strat.size/popn.size)*strat.p$strat.var)
  
  SSB < rhs # false, so we're good to go.
  
  