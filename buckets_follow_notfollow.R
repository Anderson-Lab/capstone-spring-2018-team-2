library(ranger)
library(caret)
library(dplyr)
library(data.table)
library(ggplot2)
source("Join_Data.R")
load('rda_data/meta.rda')


# Get data
meps.2015 <- Join_MEPS(2015)
mepsPrivate.2015 <-Private_Filter(meps.2015, 15)

meps.2014 <- Join_MEPS(2014)
mepsPrivate.2014 <- Private_Filter(meps.2014, 14)

# Get vars
plan.dsn <- c('HOSPINSX','ANNDEDCT', 'HSAACCT', 'PLANMETL')
behaviors <- c('BPCHEK53', 'CHOLCK53', 'NOFAT53', 'CHECK53', 'ASPRIN53', 'PAPSMR53', 
               'BRSTEX53', 'MAMOGR53', 'CLNTST53')
controls <- c('PHOLDER', 'CHBMIX42','BMINDX53','ADGENH42', 'age.cat', 'FAMINC15', 
              'COBRA', 'OOPPREM', 'PREGNT31', 'PREGNT42', 'PREGNT53')
weights <- 'PERWT15F'
vars <- c(plan.dsn, behaviors, controls)
predVars <- c(plan.dsn, controls)
factors <- c(plan.dsn, behaviors, 'PHOLDER','CHBMIX42', 'ADGENH42','COBRA', 
             'OOPPREM', 'PREGNT31', 'PREGNT42', 'PREGNT53')

preventive_behaviors <- c('DENTCK53', 'BPCHEK53', 'CHOLCK53', 'CHECK53',
                          'FLUSHT53', 'PSA53', 'PAPSMR53', 'BRSTEX53', 'MAMOGR53',
                          'CLNTST53', 'SGMTST53')

map.dental <- c('1'='Twice/Yr+', '2'='Once/Yr', '3'='< Once/Yr', '4'='Never')
map.time.since <- c('1'='Within Last Yr', '2'='Within Last 2 Yrs', '3'='Within Last 3 Yrs', '4'='Within Last 5 Yrs', '5'='>5 Yrs Ago', '6'='Never')
map.time.since.ext <- c('1'='Within Last Yr', '2'='Within Last 2 Yrs', '3'='Within Last 3 Yrs', '4'='Within Last 5 Yrs', '5'='Within Last 10 Yrs', '6'='>10 Yrs Ago', '7'='Never')

add.pv.field <- function(field, value.map){
  if (field %in% names(mepsPrivate.2015)){
    mepsPrivate.2015[, field] <<- unname(value.map[as.character(mepsPrivate.2015[,field])])
  }
}

add.pv.field('DENTCK53', map.dental)

# https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=BPCHEK53
add.pv.field('BPCHEK53', map.time.since)

# https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=CHOLCK53
add.pv.field('CHOLCK53', map.time.since)

# https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=CHECK53
add.pv.field('CHECK53', map.time.since)

# https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=PSA53
add.pv.field('PSA53', map.time.since)

# https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=PAPSMR53
add.pv.field('PAPSMR53', map.time.since)

# https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=BRSTEX53
add.pv.field('BRSTEX53', map.time.since)

# https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=MAMOGR53
add.pv.field('MAMOGR53', map.time.since)

# https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=CLNTST53
add.pv.field('CLNTST53', map.time.since.ext)

# https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=SGMTST53
add.pv.field('SGMTST53', map.time.since.ext)

add.pv.field('FLUSHT53', map.time.since.ext)

# for (care in preventive_behaviors){
#   print(meta_named_char[care])
#   careCnt = mepsPrivate.2015 %>%
#     group_by(mepsPrivate.2015[,care]) %>%
#     count()
#   
#   print(careCnt)
#   print('------------------------------')
# }

# https://www.cdc.gov/prevention/

follow = mepsPrivate.2015 %>% filter(
  # male between 40 & 60
  (
    (AGE15X > 40 & AGE15X <= 60) &
    (SEX == 1) &
    (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs'))&
    (PSA53 %in% c('Within Last Yr', 'Within Last 2 Yrs'))
       
  )
  
  |
    
  #female between 40 & 60 
  (
    (AGE15X > 40 & AGE15X <= 60) &
    (SEX == 2) &
    (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) & 
    (PAPSMR53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
    (MAMOGR53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) 
  )
  
  | 
  
  # Male 60+
  ( 
    (AGE15X > 60) &
    (SEX == 1) &
    (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
    (PSA53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
    (CLNTST53 %in% c('Within Last 10 Yrs'))
  )
    
  |
  # Female 60+
  (
    (AGE15X > 60) &
    (SEX == 2) &
    (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
    (PAPSMR53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) & 
    (MAMOGR53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
    (CLNTST53 %in% c('Within Last 10 Yrs'))
  )
  
  |
  
  (
    # 40 and Under both sexes
    (AGE15X <=40) &
    (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs'))
  )
  ) %>%
  mutate(behave_bucket = 'Follow') %>%
  as_data_frame()

#Not Follow Buckets
#Ages40_60
not_follow = mepsPrivate.2015 %>% filter(
  
  (!DUPERSID %in% follow$DUPERSID)
  
  &
    
  (
    # 40 to 60 male
    ( 
      (AGE15X > 40 & AGE15X <= 60) &
      (SEX == 1) &
      (CHECK53 %in% c('>5 Yrs Ago','Never'))  
    )
    
    |
      
    # between 40 and 60 female
    (
      (AGE15X > 40 & AGE15X <= 60) &
      (SEX == 2) &
      (CHECK53 %in% c('>5 Yrs Ago','Never')) & # Physical
      (PAPSMR53 %in% c('>5 Yrs Ago','Never')) | # Pap smear
      (MAMOGR53 %in% c('>5 Yrs Ago','Never')) # Mammogram
    )
    
    |
    
    # over sixty male
    (
      (AGE15X > 60) &
      (SEX == 1) &
      (CHECK53 %in% c('>5 Yrs Ago','Never')) &
      (PSA53 %in% c('>5 Yrs Ago','Never'))  &
      (CLNTST53 %in% c('>10 Yrs Ago', 'Never'))
    )
    
    |
      
    # 60+ female
    (
      (AGE15X > 60) &
      (SEX == 2) &
      (CHECK53 %in% c('>5 Yrs Ago','Never')) &
      (PAPSMR53 %in% c('>5 Yrs Ago','Never')) &
      (MAMOGR53 %in% c('>5 Yrs Ago','Never')) &
      (CLNTST53 %in% c('>10 Yrs Ago', 'Never'))
    )
    
    | 
      
    (
      (AGE15X <= 40) &
      (CHECK53 %in% c('>5 Yrs Ago','Never'))
    )
    
    )
  ) %>%
  mutate(behave_bucket = 'Not Follow') %>%
  as_data_frame()


buckets = rbind(follow, not_follow)
save(buckets, file="rda_data/mepsBehaviorBuckets.rda")

####################################################
####################################################

buckets %>%
  filter(AGE15X > 40 & AGE15X <= 60) %>%
  group_by(behave_bucket) %>%
  summarize(Frequency = n()) %>%
  ggplot(., aes(x = behave_bucket, y = Frequency)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  ggtitle("Those Who Follow CDC Guidelines, Age 40-60 (MEPS 2015)")

###################################################################



