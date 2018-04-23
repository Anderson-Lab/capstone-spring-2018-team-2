library(dplyr)

Join_MEPS <- function(){
  library(dplyr)
  load('raw_data/prp_full2015.rda')
  prp <- rd
  load('raw_data/fyc_full2015.rda')
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

Public_Filter <- function(df, year){
  dfPublic <- filter(df, PUBJA15X==1,PUBFE15X==1,PUBMA15X ==1,
                 PUBAP15X==1,PUBMY15X==1,PUBJU15X==1,
                 PUBJL15X==1,PUBAU15X==1,PUBSE15X==1,
                 PUBOC15X==1,PUBNO15X==1,PUBDE15X==1)
  return(dfPublic)
}

Pri_Filter <- function(df, year){
  dfPublic <- filter(df, PRIJA15X==1,PRIFE15X==1,PRIMA15X ==1,
                     PRIAP15X==1,PRIMY15X==1,PRIJU15X==1,
                     PRIJL15X==1,PRIAU15X==1,PRISE15X==1,
                     PRIOC15X==1,PRINO15X==1,PRIDE15X==1)
  return(dfPrivate)
}

Private_Filter_Uni <- function(df, yr_ending){
  dfPrivate <- filter(df, 
                      df[,sprintf('PRIJA%s', yr_ending)]==1,
                      df[,sprintf('PRIFE%s', yr_ending)]==1,
                      df[,sprintf('PRIMA%s', yr_ending)]==1,
                      df[,sprintf('PRIAP%s', yr_ending)]==1,
                      df[,sprintf('PRIMY%s', yr_ending)]==1,
                      df[,sprintf('PRIJU%s', yr_ending)]==1,
                      df[,sprintf('PRIJL%s', yr_ending)]==1,
                      df[,sprintf('PRIAU%s', yr_ending)]==1,
                      df[,sprintf('PRISE%s', yr_ending)]==1,
                      df[,sprintf('PRIOC%s', yr_ending)]==1,
                      df[,sprintf('PRINO%s', yr_ending)]==1,
                      df[,sprintf('PRIDE%s', yr_ending)]==1)
  return(dfPrivate)
}
Private_Filter <- function(df){
  library(dplyr)
  dfPrivate <- filter(df, PRIJA15==1,PRIFE15==1,PRIMA15==1,
                      PRIAP15==1,PRIMY15==1,PRIJU15==1,
                      PRIJL15==1,PRIAU15==1,PRISE15==1,
                      PRIOC15==1,PRINO15==1,PRIDE15==1)
  return(dfPrivate)
}
#<<<<<<< HEAD
Filter_MEPS_2015 <- function(df15, vars){
  mepsPrivate.2015<-Private_Filter_Uni(df15, 15)
  mepsPrivate.2015 <- mepsPrivate.2015[mepsPrivate.2015$AGE15X > 40,]
  mepsPrivate.2015$age.cat <- Age.to.Cat(mepsPrivate.2015, 'AGE15X')
  mepsPrivate.2015$w <- mepsPrivate.2015$IPDIS15
  mepsPrivate.2015[mepsPrivate.2015$w<1, 'w']<- .3
  
  return(mepsPrivate.2015[,vars])
}

Combine_MEPS_Years <- function(df15, df14, df13, join_vars){
  # If we want to add more years, we need to uses the ellipsis ... in the function
  # arguments and loop through each dataframe.
  mepsPrivate.2015<-Private_Filter(df15, 15)
  mepsPrivate.2014<-Private_Filter(df14, 14)
  mepsPrivate.2013<-Private_Filter(df13, 13)
  # Get vars
  mepsPrivate.2015 <- mepsPrivate.2015[mepsPrivate.2015$AGE15X > 40,]
  mepsPrivate.2014 <- mepsPrivate.2014[mepsPrivate.2014$AGE14X > 40,]
  mepsPrivate.2013 <- mepsPrivate.2013[mepsPrivate.2013$AGE13X > 40,]
  
  mepsPrivate.2015$w <- mepsPrivate.2015$IPDIS15
  mepsPrivate.2014$w <- mepsPrivate.2014$IPDIS14
  mepsPrivate.2013$w <- mepsPrivate.2013$IPDIS13
  
  mepsPrivate.2014$IPDIS15 <- mepsPrivate.2014$IPDIS14
  mepsPrivate.2013$IPDIS15 <- mepsPrivate.2013$IPDIS13
  
  mepsPrivate.2015[mepsPrivate.2015$w<1, 'w']<- .3
  mepsPrivate.2014[mepsPrivate.2014$w<1, 'w']<- .3
  mepsPrivate.2013[mepsPrivate.2013$w<1, 'w']<- .3
  
  mepsPrivate.2015$age.cat <- Age.to.Cat(mepsPrivate.2015, 'AGE15X')
  mepsPrivate.2014$age.cat <- Age.to.Cat(mepsPrivate.2014, 'AGE14X')
  mepsPrivate.2013$age.cat <- Age.to.Cat(mepsPrivate.2013, 'AGE13X')
  
  mepsPrivate.2014$FAMINC15 = mepsPrivate.2014$FAMINC14
  mepsPrivate.2013$FAMINC15 = mepsPrivate.2013$FAMINC13
  
  
  mepsPrivate.allyrs <- do.call("rbind", list(mepsPrivate.2015[,join_vars], mepsPrivate.2014[,join_vars], mepsPrivate.2013[,join_vars]))
  
  return (mepsPrivate.allyrs)
}

#=======
#>>>>>>> parent of 35445bb... Add function to get all data (2015,2014,2013)
# to use: df$age.cat <- age.to.cat(df, 'age')
Age.to.Cat <- function(df, Age.col){
  levs <- c('Unknown', 'less than 12', '12-17', '18-24', '25-34', '35-44', '45-54', '55-64', '65-74', '75 and over')
  return(cut(df[,Age.col], breaks=c(-Inf,0, 12, 18, 25, 35, 45, 55, 65, 75, Inf), labels=levs))
}


#meps <-inner_join(fyc,unique_prp_by_status_sum, by=c('DUPERSID'))








