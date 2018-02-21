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

Public_Filter <- function(df){
  library(dplyr)
  dfPublic <- filter(df, PUBJA15X==1,PUBFE15X==1,PUBMA15X ==1,
                 PUBAP15X==1,PUBMY15X==1,PUBJU15X==1,
                 PUBJL15X==1,PUBAU15X==1,PUBSE15X==1,
                 PUBOC15X==1,PUBNO15X==1,PUBDE15X==1)
  return(dfPublic)
}

Private_Filter <- function(df){
  library(dplyr)
  dfPrivate <- filter(df, PRIJA15==1,PRIFE15==1,PRIMA15==1,
                      PRIAP15==1,PRIMY15==1,PRIJU15==1,
                      PRIJL15==1,PRIAU15==1,PRISE15==1,
                      PRIOC15==1,PRINO15==1,PRIDE15==1)
  return(dfPrivate)
}







