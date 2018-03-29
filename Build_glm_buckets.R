library(caret)
source("Join_Data.R")
load('mepsBehaviorBucket.rda')

buckets$w <- buckets$IPDIS15
buckets[buckets$w<1, 'w']<- 1

buckets$IPDIS15[buckets$IPDIS15>1] <- 1
buckets$behave_bucket <- as.factor(buckets$behave_bucket)
buckets$ANNDEDCT <- as.numeric(buckets$ANNDEDCT)
buckets$age.cat <- Age.to.Cat(buckets, 'AGE15X')
plan.dsn <- c('HOSPINSX','ANNDEDCT', 'HSAACCT', 'PLANMETL', 'OOPPREM')
controls <- c('BMINDX53','ADGENH42', 'age.cat', 'FAMINC15', 
              'COBRA', 'PREGNT31', 'PREGNT42', 'PREGNT53')
weights <- 'w'
behaviors <- 'behave_bucket'
target <- 'IPDIS15'

vars <- c(target, plan.dsn, behaviors, controls, weights)
predVars <- c(plan.dsn, behaviors, controls)

factors <- c('IPDIS15', 'ANNDEDCT' ,'PLANMETL', 'HSAACCT', 'ADGENH42','COBRA',
             'PREGNT31', 'PREGNT42', 'PREGNT53')
buckets[buckets$OOPPREM < 0, 'OOPPREM'] <- 0


for(variable in c(plan.dsn, controls[-3])){
  buckets[buckets[,variable] < 0, variable] <- 0
}

td <- buckets[,c(predVars, target, weights)]
for(factor in factors){
  td[,factor] <- as.factor(td[, factor])
}

#split
trainidx <- createDataPartition(td$IPDIS15, p=.8, list = FALSE)
train <- td[trainidx,vars]
y.test <- td[-trainidx,target]
x.test <- td[-trainidx,-which(names(td) == target)]

ds <- downSample(train, train[,target], list = FALSE)
f <- formula(paste(target, paste(predVars, collapse = '+' ), sep = '~'))
lin.model <- glm(f,family=binomial(link='logit'),data=train, weights = train$w)

lin.model.ds <- glm(f,family=binomial(link='logit'),data=ds, weights = ds$w)
