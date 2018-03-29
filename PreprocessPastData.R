library(foreign)
source("saveSASdata.r")

# Load 2014 FYC MEPS data from online and save to rda
download.file("https://meps.ahrq.gov/mepsweb/data_files/pufs/h171ssp.zip", temp <- tempfile())
unzipped_fyc_2014_file = unzip(temp)
unlink(temp)  # Unlink to delete temporary file
SaveData(unzipped_fyc_2014_file, "fyc_full2014")

# Load 2014 PRP MEPS data from online and save to rda
download.file("https://meps.ahrq.gov/mepsweb/data_files/pufs/h169ssp.zip", temp <- tempfile())
unzipped_prp_2014_file = unzip(temp)
unlink(temp)  # Unlink to delete temporary file
SaveData(unzipped_prp_2014_file, "prp_full2014")

# Load 2013 FYC MEPS data from online and save to rda
download.file("https://meps.ahrq.gov/mepsweb/data_files/pufs/h163ssp.zip", temp <- tempfile())
unzipped_fyc_2013_file = unzip(temp)
unlink(temp)  # Unlink to delete temporary file
SaveData(unzipped_fyc_2013_file, "fyc_full2013")

# Load 2013 PRP MEPS data from online and save to rda
download.file("https://meps.ahrq.gov/mepsweb/data_files/pufs/h161ssp.zip", temp <- tempfile())
unzipped_prp_2013_file = unzip(temp)
unlink(temp)  # Unlink to delete temporary file
SaveData(unzipped_prp_2013_file, "prp_full2013")
