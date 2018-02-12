#' MEPS data importation and aggregation script
#' 
#' This script should be used to download h*.dat and h*.txt files from MEPS to
#' $RESOURCE_RESOURCES_REPO/Infrastructure/MEPS/Data/Raw/$FILE_TYPE and then generate a single
#' .rda file (one for each year) based on merging the .dat and .txt files together (which is slow).
#' 
#' This process is annoyingly complicated, but this is truly the simplest way to get MEPS data into
#' an R-compatible format.  The `importMEPSData` function below does the following:
#' 
#' 1. For a given type of MEPS file (at TOW either full year files or person-round plan files) and year, 
#'    construct a URL for downloading a .dat file
#' 2. Download the .dat file zip archive, unzip it, extract the single file it contains to the same file level
#' 3. Delete the original zip archive
#' 4. Construct a URL for the companion "instruction" file
#' 5. Download the instruction file using the URL
#' 6. Merge the .dat and .txt instruction file using SAScii into an R data frame
#' 7. Save the data frame as an RDA file
#' 7. Delete the .dat and .txt files

source('Env.R')
source('Lib.R')
library(SAScii)


##### Import Configuration #####


# Generated from https://meps.ahrq.gov/data_stats/download_data_files_results.jsp?cboDataYear=All&cboDataTypeY=105%2CPerson+Round+Plan&buttonYearandDataType=Search
prpl.file.mapping <- "File\tYear
HC-179\t2015
HC-169\t2014
HC-161\t2013
HC-153\t2012
HC-145\t2011
HC-136\t2010
HC-127\t2009
HC-119\t2008
HC-111\t2007
HC-103\t2006
HC-095\t2005
HC-088\t2004
HC-076\t2003
HC-066\t2002
HC-057\t2001
"

# Generated from https://meps.ahrq.gov/data_stats/download_data_files_results.jsp?cboDataYear=All&cboDataTypeY=101%2CConsolidated+Data&buttonYearandDataType=Search
fyc.file.mapping <- "File\tYear
HC-181\t2015
HC-171\t2014
HC-163\t2013
HC-155\t2012
HC-147\t2011
HC-138\t2010
HC-129\t2009
HC-121\t2008
HC-113\t2007
HC-105\t2006
HC-097\t2005
HC-089\t2004
HC-079\t2003
HC-070\t2002
HC-060\t2001
"


## Choose importation type by setting the file mapping and file type to be imported

# Parameters when importing "Person-Round-Plan" files
# file.mapping <- prpl.file.mapping
# file.type <- 'PRPL'

# Parameters when importing "Full Year Consolidated" files
file.mapping <- fyc.file.mapping
file.type <- 'FYC'

file.mapping <- read.table(textConnection(file.mapping), sep='\t', header = T)


##### Importation Methods #####

data.dir <- file.path('../Data/Raw', file.type)

mergeMEPSData <- function(dat.file, instr.file, file.type, year){
  
  # For some reason, the 2005 PRPL instruction file doesn't begin on the same line as the others
  beginline = 273
  if (year == 2005 && file.type == 'PRPL')
    beginline <- 266
  
  # loadRawSASData(data.file, instr.file, beginline=beginline)
  read.SAScii(dat.file, instr.file, zipped=F, beginline=beginline)
}


importMEPSData <- function(file.mapping, file.type, year.filter=NULL) {
  loginfo('Beginning import for %s years of %s data', nrow(file.mapping), file.type)
  for (i in 1:nrow(file.mapping)){
    file.name <- file.mapping[i, 'File']
    file.year <- file.mapping[i, 'Year']
    if (!is.null(year.filter) && !(file.year %in% year.filter)){
      loginfo('Skipping year "%s" because it is not in year filter list', file.year)
      next
    }
    file.num <- as.integer(str_split(tolower(file.name), '-')[[1]][2])
    stopifnot(!is.na(file.num))
    
    # Download data file as zip archive
    file.data.url <- sprintf('https://meps.ahrq.gov/data_files/pufs/h%sdat.zip', file.num)
    file.data.path <- file.path(data.dir, sprintf('h%sdat.zip', file.num))
    loginfo('Downloading data file %s to %s', file.data.url, file.data.path)
    download.file(url = file.data.url, destfile = file.data.path, quiet=T)
    
    # Extract zip archive
    loginfo('Extracting zip archive .dat file ...')
    unzip.data.dir <- file.path(data.dir, sprintf('h%sdat', file.num))
    unzip(file.data.path, exdir = unzip.data.dir)
    print(list.files(unzip.data.dir))
    data.file.name <- list.files(unzip.data.dir)[1]
    stopifnot(length(data.file.name) > 0)
    stopifnot(!is.na(data.file.name))
    stopifnot(!is.null(data.file.name))
    data.file <- file.path(unzip.data.dir, list.files(unzip.data.dir)[1])
    file.dat.path <- file.path(data.dir, sprintf('MEPS_%s_%s.dat', file.year, file.type))
    file.rename(data.file, file.dat.path)
    
    # Remove original (and now unnecessary) zip archive and extraction folder
    unlink(unzip.data.dir, recursive=T)
    unlink(file.data.path)
    
    # Download companion SAS instructions for data file
    file.inst.url <- sprintf('https://meps.ahrq.gov/data_stats/download_data/pufs/h%s/h%ssu.txt', file.num, file.num)
    file.inst.path <- file.path(data.dir, sprintf('MEPS_%s_%s.txt', file.year, file.type))
    loginfo('Downloading instruction file %s to %s', file.inst.url, file.inst.path)
    download.file(url = file.inst.url, destfile = file.inst.path, quiet=T)
    
    # Merge data and instruction files
    loginfo('Merging data and instruction files')
    data <- mergeMEPSData(file.dat.path, file.inst.path, file.type, file.year)
    file.rda.path <- file.path(data.dir, sprintf('MEPS_%s_%s.rda', file.year, file.type))
    save(data, file=file.rda.path)
    
    # Remove now unnecessary .dat and .txt instruction files
    unlink(file.dat.path)
    unlink(file.inst.path)
    
    loginfo('Import complete for file type %s and year %s', file.type, file.year)
    
  }
  
  loginfo('All imports Complete')
}


##### Import Invocation #####

#importMEPSData(file.mapping, file.type, year.filter=NULL)
# importMEPSData(file.mapping, file.type, year.filter=c(2014))
importMEPSData(file.mapping, file.type, year.filter=c(2015))

