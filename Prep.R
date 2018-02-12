#' MEPS Data Preparation for 2017 USC Study
#' 
#' IMPORTANT: This script must be run with the ~/repos/research_resources/Infrastructure/MEPS/Lib project active.  
#' This is why this script isn't contained within the main project folder.

source('Env.R')
source('Lib.R')
library(data.table)

# Load in metadata for raw data available
meps.fyc.files <- getRawMEPSMetadata('FYC')
meps.prpl.files <- getRawMEPSMetadata('PRPL')

# Load in expense variable metadata
expense.vars <- getExpenseVariables()

# Configure CPI Adjustments
cpi.base.year <- 2016
cpi.type <- 'none'
#cpi.type <- 'generic'
#cpi.type <- 'medical'


##### Begin Preparation #####

# Retrieve pre-populated CPI adjustment sets based on the given type
cpi <- getCPI(cpi.type)

prepFYCFile <- function(meps.file){
  
  path <- meps.file$path
  year <- meps.file$year
  year.abbr <- substr(as.character(year), 3, 4)
  loginfo('Processing MEPS FYC data for year %s (file = "%s")', year, path)
  
  # Extract the survey response data frame from the rda file for this year
  data <- loadRawData(path, 'FYC', year)
  
  # Initialize resulting list, which will have various fields appended to it
  res <- list(YEAR=data$YEAR)
  
  ###### Survey Design Variables #####
  
  res[['SRV_VARPSU']] <- data[, 'VARPSU']
  res[['SRV_VARSTR']] <- data[, 'VARSTR']
  res[['SRV_PERWTF']] <- getField(data, 'PERWT', year.abbr, suffix='F')
  res[['DUPERSID']] <- as.integer(as.character(data[, 'DUPERSID']))
  
  ##### Demographic Variables #####
  
  # res[['EMPLOYER_SIZE']] <- data[, 'NUMEMP53']
  # res[['EMPLOYER_INDUSTRY']] <- data[, 'INDCAT53']
  res[['DEMO_AGE']] <- getField(data, 'AGE', year.abbr, suffix='X')
  res[['DEMO_FAMINCOME']] <- getField(data, 'FAMINC', year.abbr, is.numeric=F)
  map.sex <- c("1"="MALE", "2"="FEMALE")
  res[['DEMO_GENDER']] <- getMappedField(data, 'SEX', map.sex)
  
  map.regions <- c("-1"="UNKNOWN", "1"="NORTHEAST", "2"="MIDWEST", "3"="SOUTH", "4"="WEST")
  res[['DEMO_REGION']] <- getMappedField(data, 'REGION', map.regions, year.abbr)
  
  map.priv <- c("-1"="Not-Applicable", "1"="Yes", "2"="No")
  res[['PRIV_INSURANCE']] <- getMappedField(data, 'PRIV', map.priv, year.abbr)
  res[['PRIV_INSURANCE_GROUP']] <- getMappedField(data, 'PRIEU', map.priv, year.abbr)
  
  map.ins <- c("1"="Any-Private", "2"="Public Only", "3"="Uninsured")
  res[['INSURANCE_TYPE']] <- getMappedField(data, 'INSCOV', map.ins, year.abbr)
  
  map.dep <- c("-1"="Not-Applicable", "1"="Yes")
  res[['IS_DEPENDENT']] <- getMappedField(data, 'DEPDNT', map.dep, year.abbr)
  
  ##### Preventive Variables #####
  # See: https://meps.ahrq.gov/data_stats/download_data/pufs/h171/h171doc.shtml#PreventVariables
  
  map.time.since <- c('1'='Within Last Yr', '2'='Within Last 2 Yrs', '3'='Within Last 3 Yrs', '4'='Within Last 5 Yrs', '5'='>5 Yrs Ago', '6'='Never')
  map.time.since.ext <- c('1'='Within Last Yr', '2'='Within Last 2 Yrs', '3'='Within Last 3 Yrs', '4'='Within Last 5 Yrs', '5'='Within Last 10 Yrs', '6'='>10 Yrs Ago', '7'='Never')
  map.yes.no <- c('1'='Yes', '2'='No')
  add.pv.field <- function(key, field, value.map){
    if (field %in% names(data)){
      res[[key]] <<- unname(value.map[as.character(getField(data, field))])
    }
  }
  
  # https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=DENTCK53
  map.dental <- c('1'='Twice/Yr+', '2'='Once/Yr', '3'='< Once/Yr', '4'='Never')
  add.pv.field('PVS_DENTCK', 'DENTCK53', map.dental)
  
  # https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=BPCHEK53
  add.pv.field('PVS_BPCHEK', 'BPCHEK53', map.time.since)
  
  # https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=CHOLCK53
  add.pv.field('PVS_CHOLCHK', 'CHOLCK53', map.time.since)
  
  # https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=CHECK53
  add.pv.field('PVS_DRCHK', 'CHECK53', map.time.since)
  
  # https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=NOFAT53
  add.pv.field('PVS_NOFAT', 'NOFAT53', map.yes.no)
  
  # https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=EXRCIS53
  add.pv.field('PVS_EXRCIS', 'EXRCIS53', map.yes.no)
  
  # https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=PHYEXE53
  add.pv.field('PVS_PHYEXE', 'PHYEXE53', map.yes.no)
  
  # https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=PSA53
  add.pv.field('PVS_PSA', 'PSA53', map.time.since)
  
  # https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=PAPSMR53
  add.pv.field('PVS_PAPSMR', 'PAPSMR53', map.time.since)
  
  # https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=BRSTEX53
  add.pv.field('PVS_BRSTEX', 'BRSTEX53', map.time.since)
  
  # https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=MAMOGR53
  add.pv.field('PVS_MAMOGR', 'MAMOGR53', map.time.since)
  
  # https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=CLNTST53
  add.pv.field('PVS_CLNTST', 'CLNTST53', map.time.since.ext)
  
  # https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=SGMTST53
  add.pv.field('PVS_SGMTST', 'SGMTST53', map.time.since.ext)
  
  # https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_codebook.jsp?PUFId=H147&varName=BMINDX53
  res[['PVS_BMI']] <- as.numeric(getField(data, 'BMINDX53'))
  res[['PVS_BMI']] <- ifelse(res[['PVS_BMI']] < 0, NA, res[['PVS_BMI']])
  
  
  ##### Utilization Variables #####
  
  res[['MED_IP_VISITS']] <- getField(data, 'IPDIS', year.abbr)
  res[['MED_IP_ALOS']] <- getField(data, 'IPNGTD', year.abbr)
  res[['MED_OP_VISITS']] <- getField(data, 'OPTOTV', year.abbr)
  res[['MED_DR_VISITS']] <- getField(data, 'OBTOTV', year.abbr)
  res[['MED_ER_VISITS']] <- getField(data, 'ERTOT', year.abbr)
  res[['MED_RX_CT']] <- getField(data, 'RXTOT', year.abbr)
  
  
  ##### Expense Variables #####
  
  res[['EXP_ALL_TOT']] <- getField(data, 'TOTEXP', year.abbr)
  res[['EXP_ALL_OOP']] <- getField(data, 'TOTSLF', year.abbr)
  
  res[['EXP_IP_TOT']] <- getField(data, 'IPTEXP', year.abbr)
  res[['EXP_IP_OOP']] <- getField(data, 'IPTSLF', year.abbr)
  
  res[['EXP_ER_TOT']] <- getField(data, 'ERTEXP', year.abbr)
  res[['EXP_ER_OOP']] <- getField(data, 'ERTSLF', year.abbr)
  
  res[['EXP_DR_TOT']] <- getField(data, 'OBVEXP', year.abbr)
  res[['EXP_DR_OOP']] <- getField(data, 'OBVSLF', year.abbr)
  
  ##### Result Generation #####
  
  for (n in names(res)){
    assert_that(length(res[[n]]) > 0)
  }
  res.df <- data.frame(res)
  
  # Ensure that the resulting data frame has the same number of rows as the original
  assert_that(nrow(res.df) == nrow(data))
  
  res.df
}

# dt <- cleanRawDataFile(meps.file = meps.files[[1]])

# Run above preparation function for all known raw files
d.fyc <- lapply(meps.fyc.files, prepFYCFile)
d.fyc <- data.frame(data.table::rbindlist(d.fyc, fill=T))
d.fyc$YEAR <- as.integer(d.fyc$YEAR)


##### Add PRPL Data ##### 

# The household-component file contains the majority of useful information, but the PRPL file also
# contains some useful data like who the policyholder was for someone's coverage, premiums, deductible 
# levels, etc.  This data will be added here in a way that does not eliminate or alter any HC file records.

prepPRPLFile <- function(meps.file){
  path <- meps.file$path
  year <- meps.file$year
  year.abbr <- substr(as.character(year), 3, 4)
  loginfo('Processing MEPS PRPL data for year %s (file = "%s")', year, path)
  
  # Extract the survey response data frame from the rda file for this year
  data <- loadRawData(path, 'PRPL', year)
  data
}
d.prpl <- lapply(meps.prpl.files, prepPRPLFile)
d.prpl <- data.frame(data.table::rbindlist(d.prpl, fill=T))
d.prpl$YEAR <- as.integer(d.prpl$YEAR)
d.prpl$DUPERSID <- as.integer(d.prpl$DUPERSID)

stopifnot(class(d.fyc$DUPERSID)=='integer')
stopifnot(class(d.fyc$YEAR)=='integer')
stopifnot(class(d.prpl$DUPERSID)=='integer')
stopifnot(class(d.prpl$YEAR)=='integer')

    
# status.cols <- d.prpl %>% select(starts_with('STATUS')) %>% names
# to.bin <- function(x) ifelse(x > 0, 1, 0)
# to.rng <- function(x) ifelse(x == 0, '', 'X')
# d.prpl %>% 
#   # Convert all -1/1 status indicators to 0/1
#   mutate_each(funs(to.bin), one_of(status.cols)) %>%
#   # Group by person and sum up status values
#   group_by(DUPERSID) %>% summarize_each(funs(sum), one_of(status.cols)) %>% ungroup %>%
#   # Convert status sums to blank if 0 and 'X' otherwise
#   mutate_each(funs(to.rng), one_of(status.cols)) %>%
#   # Tally all existing patterns of some type of coverage and order by frequency
#   group_by_(.dots=status.cols) %>% 
#   tally %>% ungroup %>% arrange(desc(n)) %>% View


#### PRPL Data Prep ####
# There are often many PRPL records per person and resolving them is not easy.  Across all PRPL records for an individual
# though, it is common to find all the data relating to coverage for a year however given that each record is specific
# to a collection panel, all the data for an individual in the year (even in the simplest case) is not in one line.
#
# The logic below will work through resolving these in a way that is relevant to this project (and probably this
# project only).
#
# Resources:
# 
# Why processing these is difficult: https://meps.ahrq.gov/data_stats/download_data/pufs/h169/h169doc.shtml#Complex21
# MEPS collection schedule: https://meps.ahrq.gov/survey_comp/hc_data_collection.jsp
# OOPPREMX -> -1 indicates "INAPPLICABLE), 0 indicates "NO PREMIUM CONTRIBUTION"
# https://meps.ahrq.gov/data_stats/download_data_files_codebook.jsp?PUFId=H169&varName=OOPPREMX
# ANNDEDCT -> <0 indicates various reasons for absence, 1=Low, 2=High, 3=No Deductible
# https://meps.ahrq.gov/data_stats/download_data_files_codebook.jsp?PUFId=H169&varName=ANNDEDCT
# PRIVCAT -> 0=Not Medical?, 1=EMPLOYER/UNION, 2-5=Others, 99=DONT KNOW WHAT KIND PRIV COV
# https://meps.ahrq.gov/data_stats/download_data_files_codebook.jsp?PUFId=H145&varName=PRIVCAT

resolvePRPLData <- function(d.prpl){
  status.cols <- d.prpl %>% select(starts_with('STATUS')) %>% names
  to.bin <- function(x) ifelse(x > 0, 1, 0)
  max.na.rm <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA )
  stopifnot(class(d.prpl$ANNDEDCT) == 'character')
  d.prp <- d.prpl %>% 
    # Convert deductible codes to integers (one negative value is a character for some reason)
    dplyr::mutate(ANNDEDCT=as.integer(ANNDEDCT)) %>%
    dplyr::mutate(PHLDRIDX=as.integer(PHLDRIDX)) %>%
    
    # Filter to only records for employer/union insurance
    dplyr::filter(PRIVCAT==1) %>%
    
    # Compute the number of "1" values in status fields by row
    # *Note: There are 24 status fields and most people have a 1 for each after aggregating across rows, but
    # within a row this number is generally in the 6-9 range
    dplyr::mutate_each(funs(to.bin), one_of(status.cols)) %>%
    dplyr::mutate(COVWT=rowSums(.[,status.cols])) %>% 
    
    # Group by year and attach rank of the "COVWT" variable in order to select only the records with
    # the longest applicable coverage period
    dplyr::group_by(YEAR, DUPERSID) %>% 
    dplyr::mutate(COVWTRANK=rank(-COVWT, ties.method = 'min')) %>% ungroup %>%
    dplyr::filter(COVWTRANK == 1) %>%
    
    # Regroup by year and person and aggregate selected properties
    # *Note: This is necessary because even when limiting to records with longest coverage window, there
    # are still many possibilities (for about 15% of people)
    dplyr::group_by(YEAR, DUPERSID) %>% 
    
    # At this point, there may still be records for the same person, year, and number of months covered
    # so resolve them by picking the most favorable information from each available record by that grouping
    dplyr::summarize(
      # If any available premiums, take the max
      PREMIUM=max.na.rm(ifelse(OOPPREMX >= 0, OOPPREMX, NA)),
      # If any available deductible data, take the max
      DEDUCTIBLE=max.na.rm(ifelse(ANNDEDCT >= 1, ANNDEDCT, NA)),
      PHLDRIDX=max(PHLDRIDX),
      NCOVMNTHS=COVWT[1],
      NRECORDS=n()
    ) %>% ungroup
  
  stopifnot(all(!is.na(d.prp$PHLDRIDX)))
  d.prp
}

d.prp <- resolvePRPLData(d.prpl)

#### Merging ####

d.meps <- d.fyc %>% left_join(d.prp, by=c('YEAR', 'DUPERSID'))
stopifnot(nrow(d.meps) == nrow(d.fyc))

# Encode deductible
# See: https://meps.ahrq.gov/data_stats/download_data_files_codebook.jsp?PUFId=H169&varName=ANNDEDCT
stopifnot(all(d.meps$DEDUCTIBLE %in% c(NA,1,2,3)))
d.meps$DEDUCTIBLE <- factor(d.meps$DEDUCTIBLE, levels=c(3, 1, 2), labels=c('No Deductible', 'Low', 'High'))

# Final filtering and preparation
d.meps <- d.meps %>%
  dplyr::filter(!is.na(NRECORDS)) %>%
  dplyr::filter(PRIV_INSURANCE == 'Yes') %>%
  dplyr::filter(PRIV_INSURANCE_GROUP == 'Yes') %>%
  dplyr::filter(INSURANCE_TYPE == 'Any-Private') %>%
  dplyr::select(-one_of('PRIV_INSURANCE', 'PRIV_INSURANCE_GROUP', 'INSURANCE_TYPE', 'IS_DEPENDENT', 'NRECORDS')) %>%
  dplyr::mutate(INS_ISDEP=DUPERSID != PHLDRIDX) %>%
  dplyr::rename(
    INS_MNTHSCVG=NCOVMNTHS,
    INS_PHLDR_DUPERSID=PHLDRIDX,
    INS_DEDUCTIBLE=DEDUCTIBLE,
    INS_PREMIUM=PREMIUM,
    SRV_YEAR=YEAR
  )
d.meps <- d.meps[,sort(names(d.meps))]
stopifnot(all(!is.na(d.meps$INS_PHLDR_DUPERSID)))
stopifnot(all(!is.na(d.meps$DUPERSID)))


summary(d.meps)

table(is.na(d.meps$INS_PREMIUM))
table(is.na(d.meps$INS_DEDUCTIBLE))
table(!is.na(d.meps$INS_DEDUCTIBLE) & !is.na(d.meps$INS_PREMIUM))

# 12.7k w/ premium, 8.9k w/ deductible, 5.6k with both

#library(ggplot2)
d.meps %>% ggplot(aes(x=PVS_DRCHK, y=INS_PREMIUM)) + geom_boxplot()

##### Quick Modeling #####

library(survey)
options(survey.replicates.mse=TRUE)
options(survey.lonely.psu="adjust")
d.model <- d.meps %>% 
  #mutate(OUTCOME=as.integer(PVS_DRCHK == 'Within Last Yr')) %>%
  mutate(OUTCOME=as.integer(PVS_CHOLCHK == 'Within Last Yr')) %>%
  filter(!is.na(INS_DEDUCTIBLE) & !is.na(INS_PREMIUM)) %>%
  filter(SRV_PERWTF > 0) %>%
  mutate(DEMO_FAMINCOME=ifelse(DEMO_FAMINCOME >= 0, DEMO_FAMINCOME, 0))

d.meps.srv <- svydesign(id=~SRV_VARPSU, strata=~SRV_VARSTR, weights=~SRV_PERWTF, data=d.model, nest=TRUE)
table(d.model$OUTCOME)

m.srv <- svyglm(
  formula=OUTCOME ~ INS_DEDUCTIBLE + log10(INS_PREMIUM+1) + DEMO_AGE + log10(DEMO_FAMINCOME+1) + DEMO_GENDER,
  design=d.meps.srv,
  family=quasibinomial()
)
summary(m.srv)


##### Export Results

# Second version after adding 2015 data
EXPORT_PATH_FYC <- '/Users/eczech/repos/research_projects/2017Q3_USCPreventiveServices/Code/R/meps/data/meps-fyc-v1.csv'
write.csv(d.fyc, EXPORT_PATH_FYC, row.names = F)
loginfo('Finished export to "%s"', EXPORT_PATH_FYC)

EXPORT_PATH_PRPL <- '/Users/eczech/repos/research_projects/2017Q3_USCPreventiveServices/Code/R/meps/data/meps-prpl-v1.csv'
write.csv(d.prp, EXPORT_PATH_PRPL, row.names = F)
loginfo('Finished export to "%s"', EXPORT_PATH_PRPL)

EXPORT_PATH <- '/Users/eczech/repos/research_projects/2017Q3_USCPreventiveServices/Code/R/meps/data/meps-v1.csv'
write.csv(d.meps, EXPORT_PATH, row.names = F)
loginfo('Finished export to "%s"', EXPORT_PATH)
