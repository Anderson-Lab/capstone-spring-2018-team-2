Join_MEPS_14 <- function(){
  library(dplyr)
  load('prp_full2014.rda')
  prp <- rd
  load('fyc_full2014.rda')
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
  dfPublic <- filter(df, PUBJA14X==1,PUBFE14X==1,PUBMA14X ==1,
                     PUBAP14X==1,PUBMY14X==1,PUBJU14X==1,
                     PUBJL14X==1,PUBAU14X==1,PUBSE14X==1,
                     PUBOC14X==1,PUBNO14X==1,PUBDE14X==1)
  return(dfPublic)
}

Private_Filter <- function(df){
  library(dplyr)
  dfPrivate <- filter(df, PRIJA14==1,PRIFE14==1,PRIMA14==1,
                      PRIAP14==1,PRIMY14==1,PRIJU14==1,
                      PRIJL14==1,PRIAU14==1,PRISE14==1,
                      PRIOC14==1,PRINO14==1,PRIDE14==1)
  return(dfPrivate)
}

# to use: df$age.cat <- age.to.cat(df, 'age')
Age.to.Cat <- function(df, Age.col){
  levs <- c('Unknown', 'less than 12', '12-17', '18-24', '25-34', '35-44', '45-54', '55-64', '65-74', '75 and over')
  return(cut(df[,Age.col], breaks=c(-Inf,0, 12, 18, 25, 35, 45, 55, 65, 75, Inf), labels=levs))
}


#meps <-inner_join(fyc,unique_prp_by_status_sum, by=c('DUPERSID'))








