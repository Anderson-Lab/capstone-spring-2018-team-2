library(ranger)
library(caret)
library(dplyr)
library(data.table)
library(ggplot2)
source("Join_Data.R")
load('meta.rda')


# Get data
meps <- Join_MEPS()
meps.p <- meps[meps$PHOLDER == 1,]
mepsPublic<-Public_Filter(meps.p)
mepsPrivate<-Private_Filter(meps.p)

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
  if (field %in% names(mepsPrivate)){
    mepsPrivate[, field] <<- unname(value.map[as.character(mepsPrivate[,field])])
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

for (care in preventive_behaviors){
  print(meta_named_char[care])
  careCnt = mepsPrivate %>%
    group_by(mepsPrivate[,care]) %>%
    count()
  
  print(careCnt)
  print('------------------------------')
}

# Good Behavior: 
#  At least one general checkup (CHECK53) within the last 2 years
#  AND
#  A cholesterol check within the last 2 yrs all ages
#  AND 
#  A mammogram or pap smear for females within the last 2 years age 40+ 
#  OR
#  A prostate exam for males within the last 2 years age 50+
#  AND
#  A colon exam within the last 10 years for males/females age 50+
#  OR 
#  A checkup if under 40 within the past 2 years.

good = mepsPrivate %>% filter(
        # between 40 and 50 male
        (
          (AGE15X > 40 & AGE15X <= 50) &
          (SEX == 1) &
          (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
          (CHOLCK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) 
        )
      |
       # 50+ and male
        ( 
          (AGE15X > 50) &
          (SEX == 1) &
          (PSA53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
          (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
          (CHOLCK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
          (CLNTST53 %in% c('Within Last 10 Yrs', 'Within Last 5 Yrs', 'Within Last 3 Yrs', 'Within Last 2 Yrs', 'Within Last Yr'))
        )
        
      |
        # between 40 and 50 female
        (
          (AGE15X > 40 & AGE15X <= 50) &
          (SEX == 2) &
          (PAPSMR53 %in% c('Within Last Yr', 'Within Last 2 Yrs') | MAMOGR53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
          (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
          (CHOLCK53 %in% c('Within Last Yr', 'Within Last 2 Yrs'))
        )
      
      |
        # 50+ and female
        (
          (AGE15X > 50) &
          (SEX == 2) &
          (PAPSMR53 %in% c('Within Last Yr', 'Within Last 2 Yrs') | MAMOGR53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
          (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
          (CHOLCK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
          (CLNTST53 %in% c('Within Last 10 Yrs', 'Within Last 5 Yrs', 'Within Last 3 Yrs', 'Within Last 2 Yrs', 'Within Last Yr'))
        )
      |
        # 40 and under both sexes
        (AGE15X <=40) & 
        (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
        (CHOLCK53 %in% c('Within Last Yr', 'Within Last 2 Yrs'))
  ) %>%
  mutate(behave_bucket = 'Good')

mepsPrivate %>%
  filter(
    (
      (AGE15X > 40) &
        (SEX == 2) &
        (PAPSMR53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
        (MAMOGR53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
        (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
        (CHOLCK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
        (AGE15X > 50 & CLNTST53 %in% c('Within Last 10 Yrs', 'Within Last 5 Yrs', 'Within Last 3 Yrs', 'Within Last 2 Yrs', 'Within Last Yr'))
    )
  ) %>% select(AGE15X, PAPSMR53, MAMOGR53, CLNTST53) %>%
  as_data_frame()
  
# Fair Behavior: 
#  One general checkup (CHECK53) within the last 2 years
#  AND 
#  At least one of the following:
#  - a mammogram or pap smear for females within the last 2 years age 40+ 
#  - prostate exam for males within the last 2 years age 50+
#  - a colon exam within the last 10 years for males/females age 50+
#  - a cholesterol check within the last 2 yrs for males/females 40+
#  OR 
#  A checkup within the past 5 years.

fair = mepsPrivate %>% filter(
  
    (!DUPERSID %in% good$DUPERSID)  
  
  & 
    
    ( 
      # 50+ and male
      ( 
        (AGE15X > 50) &
        (SEX == 1) &
        (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
          (
            (PSA53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) |
            (CHOLCK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) |
            (CLNTST53 %in% c('Within Last 10 Yrs', 'Within Last 5 Yrs', 'Within Last 3 Yrs', 'Within Last 2 Yrs', 'Within Last Yr'))
          )
      )
    
    |
    # between 40 and 50 female
      (
        (AGE15X > 40 & AGE15X <= 50) &
        (SEX == 2) &
        (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
          (
            (PAPSMR53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) |
            (MAMOGR53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) |
            (CHOLCK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) 
          )
      )
    
    |
      # 50+ and female
      (
        (AGE15X > 50) &
        (SEX == 2) &
        (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
          (
            (PAPSMR53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) |
            (MAMOGR53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) |
            (CHOLCK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) |
            (CLNTST53 %in% c('Within Last 10 Yrs', 'Within Last 5 Yrs', 'Within Last 3 Yrs', 'Within Last 2 Yrs', 'Within Last Yr'))
          )
      )
    |
      (
        # 40 and under both sexes
        (AGE15X <= 40) &
        (
          (CHECK53 %in% c('Within Last 3 Yrs', 'Within Last 5 Yrs')) |
          (CHOLCK53 %in% c('Within Last 3 Yrs', 'Within Last 5 Yrs'))
        )
      )
    )
  ) %>% 
  mutate(behave_bucket = 'Fair') %>%
  as_data_frame()



# Bad Behavior: 
#  NO general checkup (CHECK53) in the past 5 years all ages
#  AND 
#  NO mammogram or pap smear for females within the last 5 years age 40+ 
#     or
#  NO prostate exam for males within the last 5 years age 50+
#  AND
#  NO colon exam within the last 10 years for males/females age 50+
#  AND
#  NO cholesterol check within the last 2 yrs for males/females age 50+

poor = mepsPrivate %>% filter(
  
    (!DUPERSID %in% good$DUPERSID & !DUPERSID %in% fair$DUPERSID)  
  
  & 
    
    ( 
      # 50+ and male
      ( 
        (AGE15X > 50) &
        (SEX == 1) &
        (CHECK53 %in% c('>5 Yrs Ago','Never')) &
        (PSA53 %in% c('>5 Yrs Ago','Never')) &
        (CHOLCK53 %in% c('>5 Yrs Ago','Never')) &
        (CLNTST53 %in% c('>10 Yrs Ago', 'Never'))
      )
      
    |
      # between 40 and 50 female
      (
        (AGE15X > 40 & AGE15X <= 50) &
        (SEX == 2) &
        (PAPSMR53 %in% c('>5 Yrs Ago','Never')) &
        (MAMOGR53 %in% c('>5 Yrs Ago','Never')) &
        (CHECK53 %in% c('>5 Yrs Ago','Never')) &
        (CHOLCK53 %in% c('>5 Yrs Ago','Never'))
      )
    |
      # 50+ female
      (
        (AGE15X > 40 & AGE15X <= 50) &
        (SEX == 2) &
        (PAPSMR53 %in% c('>5 Yrs Ago','Never')) &
        (MAMOGR53 %in% c('>5 Yrs Ago','Never')) &
        (CHECK53 %in% c('>5 Yrs Ago','Never')) &
        (CHOLCK53 %in% c('>5 Yrs Ago','Never')) &
        (CLNTST53 %in% c('>10 Yrs Ago', 'Never'))
      )
    |
      (
        (AGE15X <= 40) &
        (CHECK53 %in% c('>5 Yrs Ago','Never')) &
        (CHOLCK53 %in% c('>5 Yrs Ago','Never'))
      )
    )
  ) %>%
  mutate(behave_bucket = 'poor') %>%
  as_data_frame()

buckets = rbind(c(good, fair, poor))
