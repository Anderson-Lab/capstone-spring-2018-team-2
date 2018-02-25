Join_MEPS <- function(){
library(dplyr)
load('prp_full2015.rda')
prp <- rd
load('fyc_full2015.rda')
fyc <- rd
#Remove var to save space
remove(rd)


# this filter knocks prp down to 34673 rows. We want only unique DUPERSIDS, 
# so we'll select the policyholder row with the most 1's in the status fields
prp_pholders = prp %>% filter(PHOLDER == 1)

status_prp_pholders = prp_pholders %>% 
  select(DUPERSID, RN, STATUS1, STATUS2, STATUS3, STATUS4, STATUS5, STATUS6, STATUS7, STATUS8, STATUS9, STATUS10, STATUS11, STATUS12,
         STATUS13, STATUS14, STATUS15, STATUS16, STATUS17, STATUS18, STATUS19, STATUS20, STATUS21, STATUS22, STATUS23, STATUS24)

unique_prp_by_status_sum = status_prp_pholders %>%
  mutate(STAT_SUM=Reduce("+",.[3:26])) %>% # sum horizontally across all the status variables
  group_by(DUPERSID) %>%
  filter(STAT_SUM > -24) %>% #people who at least answered 1 for one of the status periods (covered by insurance for at least one day during month)
  mutate(rank = rank(-STAT_SUM, ties.method = "first")) %>% # rank the rows in each group by STAT_SUM
  top_n(n=1, wt=-rank) # only return the top row
  
prp[unique_prp_by_status_sum$DUPERSID, ] %>% group_by(NAMECHNG) %>% count(NAMECHNG)
meps <-inner_join(fyc,prp[unique_prp_by_status_sum$DUPERSID,], by=c('DUPERSID'))

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


meps <-inner_join(fyc,unique_prp_by_status_sum, by=c('DUPERSID'))








