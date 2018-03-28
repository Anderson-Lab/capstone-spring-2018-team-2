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

# Poor Behavior: no mammogram or breast exam for females within the last 2 years for folks over 40 years old,
# No checkup, no colon exam within the last 10 years for folks over 50 years, and no cholesterol check

mepsPrivate %>% 
  filter(
      ((SEX == 2 & AGE15X > 40) & ((!BRSTEX53 %in% c('Within Last 2 Yrs', 'Within Last Yr', 'NA')) |
                                   (!MAMOGR53 %in% c('Within Last 2 Yrs', 'Within Last Yr', 'NA')))) 
    |
      ((SEX == 1 & AGE15X > 50) & (!PSA53 %in% c('Within Last 2 Yrs', 'Within Last Yr', 'NA')))
    |
      ((AGE15X > 50) & (CHOLCK53 %in% c('Never') | CLNTST53 %in% c('Never', '>10 Yrs Ago')))
    ) %>%
  count()

# Fair Behavior: At least one checkup, at least one of the following, given guidelines:
# a mammogram or breast exam for females within the last 2 years for folks over 40 years old,
# a colon exam within the last 10 years for folks since they turned 50, or a cholesterol check. Or only a checkup if under 40.


# Good Behavior: At least one checkup, a mammogram or breast exam for females within the last 2 years for folks over 40 years old,
# a colon exam within the last 10 years since they turned 50, and a cholesterol check.

for (care in preventive_behaviors){
  print(meta_named_char[care])
  careCnt = mepsPrivate %>%
    group_by(mepsPrivate[,care]) %>%
    count()
  
  print(careCnt)
  print('------------------------------')
}
