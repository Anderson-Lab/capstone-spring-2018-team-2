library(dplyr)
prp = rd.p

# Info on how panels and rounds work in MEPS:
# https://meps.ahrq.gov/survey_comp/hc_data_collection.jsp

# 2015 data was collected in panels 19 & 20 
distinct(prp, PANEL)

# survey questions were answered in the following round order...
distinct(prp, RN)

# There are 61,436 rows in prp
nrow(distinct(prp))

# There are 21,103 unique people in this file, which means there are multiple rows for
# the same person. According to MEPS, a DUPERSID will show up X + Y times 
# when 1) they do an interview in X number of rounds and 2) they obtain insurance 
# from Y different establishments (ex: job switch in the middle of a round interview)
nrow(distinct(prp, DUPERSID))


by_dupersid = group_by(prp, DUPERSID)

# this tibble shows records with DUPERSID = 60001103. This person only answered questions 
# in rounds 3, 4, and 5. They were the policyholder (PHOLDER) for their private insurance. They held
# the same job at the same establishment in each round (JOBSIDX). Their source of insurance coverage
# came from the same establishment (ESTBIDX). They obtained the same insurance coverage from this establishment in each round (EPRSIDX).
# And they didn't have a change of name on the plan
by_dupersid %>% 
  select(EPCPIDX, DUPERSID, PHLDRIDX, ESTBIDX, EPRSIDX, PANEL, RN, JOBSIDX, PHOLDER, NAMECHNG) %>%
  filter(DUPERSID == "60001103")

# here is a subest of a tibble where the PHLDRIDX = 60003101. This is an interesting example
# because there are dependents on the policy holder (where PHOLDER == 0 ).
by_dupersid %>% 
  select(EPCPIDX, DUPERSID, PHLDRIDX, ESTBIDX, EPRSIDX, PANEL, RN, JOBSIDX, PHOLDER, NAMECHNG) %>%
  filter(PHLDRIDX == "60003101") 

# here's a query that sums how many times a policyholder changes the name on their insurance coverage
by_dupersid %>% 
  select(NAMECHNG) %>%
  group_by_("NAMECHNG") %>%
  count("NAMECHNG")
