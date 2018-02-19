Join_MEPS <- function(){
library(dplyr)
load('prp_full2015.rda')
prp <- rd
load('fyc_full2015.rda')
fyc <- rd
#Remove var to save space
remove(rd)

prp <- droplevels(prp[prp$NAMECHNG == 2,])
meps <-inner_join(fyc,prp, by=c('DUPERSID'))

return(meps)
}





