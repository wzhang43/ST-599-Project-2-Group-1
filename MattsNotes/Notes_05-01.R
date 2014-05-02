### Matt's notes 5/2 - 

mutate( region = regions["PDX"])


#rtest<- c( "PDX"=1, "SFO" =2)

#regions <- c(1,2)
#names(regions) <- c("PDX", "SFO")

ap.df <- data.frame( cbind(airport=c("PDX", "SFO", "TAC"), state=c("OR", "CA", "WA"), region=c(1,2,1)))

r3 <- as.integer(ap.df$region)
names(r3) <- ap.df$airport
r3
r3["PDX"]

# name the regions, not numbers

save intermediate data files

git issues

how big of sample size? may need to do some sample size analysis.



state.reg <- read.csv("data/State_Region.csv")
