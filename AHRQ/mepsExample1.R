## Example 1: Re-producing estimates from MEPS summary table for 2013 data

# Load packages and set options

install.packages("foreign")  # Only need to run these once
install.packages("survey")

library(foreign) # Run these every time you re-start R
library(survey)

options(survey.lonely.psu='adjust')

# Load MEPS data from internet
download.file("https://meps.ahrq.gov/mepsweb/data_files/pufs/h181ssp.zip", temp <- tempfile())
unzipped_file = unzip(temp)
h181 = read.xport(unzipped_file)
unlink(temp)  # Unlink to delete temporary file

# After downloading MEPS data define the survey object:
mepsdsgn <- svydesign(id = ~VARPSU,
                      strata = ~VARSTR,
                      weights = ~PERWT15F,
                      data = h181,
                      nest = TRUE)

# TOTAL POPULATION
# Standard errors are not applicable to population control totals, so we don't need to use a survey function here.
# The total population is equal to the sum of survey weights (PERWT13F).
sum(h181$PERWT15F)

# TOTAL EXPENSES
# Use the formula notation '~' with specified design object for survey functions
svytotal(~TOTEXP15,design = mepsdsgn)

# PERCENT WITH EXPENSE
# To calculate the percent of people with any expense, first update mepsdsgn with a new indicator variable for persons with an expense:
mepsdsgn <- update(mepsdsgn, any_expense = (TOTEXP15 > 0)*1)

# Then run the 'svymean' function
svymean(~any_expense,design = mepsdsgn)

# MEAN AND MEDIAN EXPENSE, PER PERSON
# To get expenses per person with an expense, use the 'subset' function to limit the dataset to persons that have an expense
# (i.e. any_expense == 1).
svymean(~TOTEXP15, design = subset(mepsdsgn,any_expense==1))
svyquantile(~TOTEXP15, design = subset(mepsdsgn,any_expense==1),quantiles = 0.5)

# DISTRIBUTION BY SOURCE OF PAYMENT
# For percent of total, use the `svyratio` function, and specify the numerator and denominator.
# Use a '+' sign to calculate estimates for multiple variables.
svyratio(~TOTSLF15 + TOTPTR15 + TOTMCR15 + TOTMCD15,
         denominator = ~TOTEXP15,
         design = mepsdsgn)

# Before estimating percentages for 'Other' insurance, we need to adjust this variable to match the online table:
# Other = VA + worker's comp + other sources.
mepsdsgn <- update(mepsdsgn, tototh15 = TOTVA15 + TOTWCP15 + TOTOTH15)
svyratio(~tototh15, denominator = ~TOTEXP15, design = mepsdsgn)
