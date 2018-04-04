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

#between 40 and 60 
follow40_60 = mepsPrivate %>% filter(
  # male
  (
    (AGE15X > 40 & AGE15X <= 60) &
      (SEX == 1) &
      (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs'))&
      (PSA53 %in% c('Within Last Yr', 'Within Last 2 Yrs'))
       
  )
  |
  #female
  (
      (AGE15X > 40 & AGE15X <= 60) &
        (SEX == 2) &
        (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs'))& 
        ((PAPSMR53 %in% c('Within Last Yr', 'Within Last 2 Yrs') &
        MAMOGR53 %in% c('Within Last Yr', 'Within Last 2 Yrs'))) 
    ))%>%
  mutate(behave_bucket_40_60 = 'Follow') %>%
  as_data_frame()

#60+
followAbove60 = mepsPrivate %>% filter(
    #Male
    ( 
      (AGE15X > 60) &
        (SEX == 1) &
        (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
        
        ((PSA53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
        (CLNTST53 %in% c('Within Last 10 Yrs')))
    )
    |
    #Female
    (
      (AGE15X > 60) &
        (SEX == 2) &
        (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
        ((PAPSMR53 %in% c('Within Last Yr', 'Within Last 2 Yrs') & 
          MAMOGR53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
        CLNTST53 %in% c('Within Last 10 Yrs'))
    ))%>%
    mutate(behave_bucket_above60 = 'Follow') %>%
    as_data_frame()
   
#Under40  
followUnder40 = mepsPrivate %>% filter(
    #40 and under both sexes
    (AGE15X <=40) &
    (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs'))
) %>%
    mutate(behave_bucket_under40 = 'Follow') %>%
    as_data_frame()

#############################################################################################################################################
#############################################################################################################################################

# fair = mepsPrivate %>% filter(
#   
#   (!DUPERSID %in% good$DUPERSID)  
#   
#   & 
#     
#     ( 
#       # 50+ and male
#       ( 
#         (AGE15X > 50) &
#           (SEX == 1) &
#           (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
#           (
#             (PSA53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) |
#               (CHOLCK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) |
#               (CLNTST53 %in% c('Within Last 10 Yrs', 'Within Last 5 Yrs', 'Within Last 3 Yrs', 'Within Last 2 Yrs', 'Within Last Yr'))
#           )
#       )
#       
#       |
#         # between 40 and 50 female
#         (
#           (AGE15X > 40 & AGE15X <= 50) &
#             (SEX == 2) &
#             (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
#             (
#               (PAPSMR53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) |
#                 (MAMOGR53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) |
#                 (CHOLCK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) 
#             )
#         )
#       
#       |
#         # 50+ and female
#         (
#           (AGE15X > 50) &
#             (SEX == 2) &
#             (CHECK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) &
#             (
#               (PAPSMR53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) |
#                 (MAMOGR53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) |
#                 (CHOLCK53 %in% c('Within Last Yr', 'Within Last 2 Yrs')) |
#                 (CLNTST53 %in% c('Within Last 10 Yrs', 'Within Last 5 Yrs', 'Within Last 3 Yrs', 'Within Last 2 Yrs', 'Within Last Yr'))
#             )
#         )
#       |
#         (
#           # 40 and under both sexes
#           (AGE15X <= 40) &
#             (
#               (CHECK53 %in% c('Within Last 3 Yrs', 'Within Last 5 Yrs')) |
#                 (CHOLCK53 %in% c('Within Last 3 Yrs', 'Within Last 5 Yrs'))
#             )
#         )
#     )
# ) %>% 
#   mutate(behave_bucket = 'Fair') %>%
#   as_data_frame()

#############################################################################################################################################
#############################################################################################################################################

#Not Follow Buckets
#Ages40_60
  notFollow40_60 = mepsPrivate %>% filter(
  (!DUPERSID %in% good$DUPERSID)
  &
    
    (
      # 40 to 60 male
      ( 
        (AGE15X > 40 & AGE15X <= 60) &
          (SEX == 1) &
          (CHECK53 %in% c('>5 Yrs Ago','Never')) #Physical  
      )
      |
        # between 40 and 60 female
        (
          (AGE15X > 40 & AGE15X <= 60) &
          (SEX == 2) &
          (CHECK53 %in% c('>5 Yrs Ago','Never')) & #Physical
          ((PAPSMR53 %in% c('>5 Yrs Ago','Never')) | #Pap smear
          (MAMOGR53 %in% c('>5 Yrs Ago','Never'))) #Mammogram
        )
      )
  )%>%
    mutate(behave_bucket_40_60 = 'Not Follow') %>%
    as_data_frame()

#AgesAbove60
notFollowAbove60 = mepsPrivate %>% filter(
  (!DUPERSID %in% good$DUPERSID)&
    # 60+ and male
        ( 
          (AGE15X > 60) &
            (SEX == 1) &
            (CHECK53 %in% c('>5 Yrs Ago','Never')) &
            ((PSA53 %in% c('>5 Yrs Ago','Never'))  |
            (CLNTST53 %in% c('>10 Yrs Ago', 'Never')))
        )
      |
        # 60+ female
        (
          (AGE15X > 60) &
            (SEX == 2) &
            (CHECK53 %in% c('>5 Yrs Ago','Never')) &
            ((PAPSMR53 %in% c('>5 Yrs Ago','Never')) |
            (MAMOGR53 %in% c('>5 Yrs Ago','Never')) |
            (CLNTST53 %in% c('>10 Yrs Ago', 'Never')))
        )
  )%>%
  mutate(behave_bucket_above60 = 'Not Follow') %>%
  as_data_frame()

#Under40
  notFollowUnder40 = mepsPrivate %>% filter(
  (!DUPERSID %in% good$DUPERSID)
  &
        #Under 40 Both Sexes
        (
          (AGE15X <= 40) &
            (CHECK53 %in% c('>5 Yrs Ago','Never'))

        )
    ) %>%
  mutate(behave_bucket_under40 = 'Not Follow') %>%
  as_data_frame()

####################################################
####################################################
  
#For 40_60
#Count of participants
#40_60
count(follow40_60)
count(notFollow40_60)
#Percentage
count(notFollow40_60)/count(follow40_60)*100

buckets40_60 = rbind(follow40_60, notFollow40_60)

buckets40_60 %>%
  group_by(behave_bucket_40_60) %>%
  summarize(Frequency = n()) %>%
  ggplot(., aes(x = behave_bucket_40_60, y = Frequency)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  ggtitle("Those Who Follow AMA Guidelines, Age 40-60 (MEPS 2015)")

###################################################################

#For Over60
#Count of participants
count(followAbove60)
count(notFollowAbove60)
#Percentage
count(notFollowAbove60)/count(followAbove60)*100

buckets60 = rbind(followAbove60, notFollowAbove60)

buckets60 %>%
  group_by(behave_bucket_above60) %>%
  summarize(Frequency = n()) %>%
  ggplot(., aes(x = behave_bucket_above60, y = Frequency)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  ggtitle("Those Who Follow AMA Guidelines, Above 60 (MEPS 2015)")

###################################################################

#For Under 40
#Count of participants
#40_60
count(followUnder40)
count(notFollowUnder40)
#Percentage
count(notFollowUnder40)/count(followUnder40)*100

bucketsUnder40 = rbind(followUnder40, notFollowUnder40)

bucketsUnder40 %>%
  group_by(behave_bucket_under40) %>%
  summarize(Frequency = n()) %>%
  ggplot(., aes(x = behave_bucket_under40, y = Frequency)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  ggtitle("Those Who Follow AMA Guidelines, Age Under 40 (MEPS 2015)")
