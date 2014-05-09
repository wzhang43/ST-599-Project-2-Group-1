samp.dat <- read.csv("data/sample_data.csv", header=T, stringsAsFactors=F)



so, what I did was basically fail at math. 

I was looking at the MONTHLY average, and then failed to note that I was pulling data on a YEARLY basis, so the 14,000 I was expected, was per month. so, the 168,000 target would have been correct for year.

In terms of resampling, I think we should look at the avg sample size per month


samp.dat.f <- samp.dat %.% filter(weatherdelay != 'NA')
# ... 80,000 where Weather delay is != NA?

samp.dat %.% group_by(year) %.% summarise(n=n())

samp.dat.f %.% group_by(year) %.% summarise(n=n())

## I think I just found why it jumps up in 2008


iata.dat <- read.csv("data/iata_by_region.csv", header=T, stringsAsFactors=F)

samp.reg <- left_join(samp.dat.f, iata.dat, on='origin')

yr.mo <- samp.reg %.% group_by(year, month, Region) %.% summarise(n=n())
yr.mo <- ungroup(yr.mo)
reg.avg <- yr.mo %.% group_by(Region) %.% summarise(ttl = sum(n), avg=mean(n))
reg.avg
