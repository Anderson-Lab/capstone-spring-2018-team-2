library(dplyr)
load('prp2015.rda')
prp <- rd.p
load('fyc2015.rda')
fyc <- rd.p
#Remove var to save space
remove(rd.p)

vars <- c(id, plan.dsn,behaviors, controls, target)
prp <- droplevels(prp[prp$NAMECHNG == 2,])

meps <-inner_join(prp, fyc, by=c('DUPERSID'))

