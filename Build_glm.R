library(caret)
source("Join_Data.R")
load('meta.rda')

# Get data
meps <- Join_MEPS()
meps.p <- meps[meps$PHOLDER == 1,]
mepsPublic<-Public_Filter(meps.p)
mepsPrivate<-Private_Filter(meps.p)

# Get vars
mepsPrivate$age.cat <- Age.to.Cat(mepsPrivate, 'AGE15X')
plan.dsn <- c('HOSPINSX','ANNDEDCT', 'HSAACCT', 'PLANMETL')
behaviors <- c('BPCHEK53', 'CHOLCK53', 'NOFAT53', 'CHECK53', 'ASPRIN53', 'PAPSMR53', 
               'BRSTEX53', 'MAMOGR53', 'CLNTST53')
controls <- c('PHOLDER', 'CHBMIX42','BMINDX53','ADGENH42', 'age.cat', 'FAMINC15', 
              'COBRA', 'OOPPREM', 'PREGNT31', 'PREGNT42', 'PREGNT53')
target <- 'IPDIS15'
weights <- 'PERWT15F'
vars <- c(target, plan.dsn, behaviors, controls, weights)
predVars <- c(plan.dsn, behaviors, controls)
factors <- c('IPDIS15', 'HOSPINSX', 'PLANMETL', 'HSAACCT', behaviors, 'PHOLDER',
             'CHBMIX42', 'ADGENH42','COBRA', 'OOPPREM', 'PREGNT31', 'PREGNT42', 'PREGNT53')

#Set target to binary
mepsPrivate$IPDIS15[mepsPrivate$IPDIS15>1] <- 1

#Coerce to fewer factors
mepsPrivate$ANNDEDCT <- as.numeric(mepsPrivate$ANNDEDCT)
for(variable in c(plan.dsn, behaviors)){
  mepsPrivate[mepsPrivate[,variable] < 0, variable] <- "Unknown"
}

for(factor in factors){
  mepsPrivate[,factor] <- as.factor(mepsPrivate[, factor])
}

#split
trainidx <- createDataPartition(mepsPrivate$IPDIS15, p=.8, list = FALSE)
train <- mepsPrivate[trainidx,vars]
y.test <- mepsPrivate[-trainidx,target]
x.test <- mepsPrivate[-trainidx,-which(names(meps) == target)]

ds <- downSample(train, train[,target], list = FALSE)
predVars2 <- predVars[-which(predVars %in% c('PHOLDER', 'OOPPREM'))]
f <- formula(paste(target, paste(predVars2, collapse = '+' ), sep = '~'))
lin.model <- glm(f,family=binomial(link='logit'),data=train[,c(predVars2, target)])

