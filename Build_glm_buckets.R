library(caret)
library(ranger)
source("Join_Data.R")
load('mepsBehaviorBucket.rda')

buckets <- droplevels(buckets[buckets$AGE15X >= 40,])
buckets$w <- buckets$IPDIS15
buckets[buckets$w<1, 'w']<- .3

buckets$IPDIS15[buckets$IPDIS15>1] <- 1
buckets$behave_bucket <- as.factor(buckets$behave_bucket)
buckets$ANNDEDCT <- as.numeric(buckets$ANNDEDCT)
buckets$age.cat <- Age.to.Cat(buckets, 'AGE15X')
plan.dsn <- c('ANNDEDCT', 'HSAACCT', 'PLANMETL', 'OOPPREM')
controls <- c('age.cat', 'FAMINC15', 'SEX',
              'COBRA')
weights <- 'w'
behaviors <- 'behave_bucket'
target <- 'IPDIS15'

vars <- c(target, plan.dsn, behaviors, controls, weights)
predVars <- c(plan.dsn, behaviors, controls)

factors <- c('IPDIS15', 'ANNDEDCT' ,'PLANMETL', 'HSAACCT', 'COBRA')
buckets[buckets$OOPPREM < 0, 'OOPPREM'] <- 0


for(variable in c(plan.dsn[-c(2,3)], controls[-1])){
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
rf.model <- ranger(formula = f,
                   data = train,
                   case.weights = train$w,
                   num.trees = 2500,
                   importance = 'impurity',
                   min.node.size = 150,
                   probability = TRUE,
                   classification = TRUE,
                   sample.fraction = .7
                   )

lin.model.ds <- glm(f,family=binomial(link='logit'),data=ds, weights = ds$w)
rf.model.ds <- ranger(formula = f,
                   data = ds,
                   case.weights = ds$w,
                   num.trees = 500,
                   importance = 'impurity',
                   min.node.size = 35,
                   probability = TRUE,
                   classification = TRUE,
                   sample.fraction = .7
)


# predict buckets
predVars <- c(plan.dsn, controls)
target <- 'behave_bucket'

ds <- downSample(train, train[,target], list = FALSE)
f <- formula(paste(target, paste(predVars, collapse = '+' ), sep = '~'))
# ONLY RUN GLM CALLS IF 2 BUCKETS
#lin.model.b <- glm(f,family=binomial(link='logit'),data=train, weights = train$w)
rf.model.b <- ranger(formula = f,
                   data = train,
                   case.weights = train$w,
                   num.trees = 1500,
                   importance = 'impurity',
                   min.node.size = 150,
                   probability = TRUE,
                   classification = TRUE,
                   sample.fraction = .7
)

#lin.model.b.ds <- glm(f,family=binomial(link='logit'),data=ds, weights = ds$w)
rf.model.b.ds <- ranger(formula = f,
                     data = ds,
                     case.weights = ds$w,
                     num.trees = 500,
                     importance = 'impurity',
                     min.node.size = 10,
                     probability = TRUE,
                     classification = TRUE,
                     sample.fraction = .7
)


print("Trained models are: lin.model, lin.model.ds, #lin.model.b, #lin.model.b.ds, rf.model, rf.model.ds,rd.model.b, and rf.model.b.ds")
