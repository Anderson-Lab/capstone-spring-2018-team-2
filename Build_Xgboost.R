library(xgboost)
source("Join_Data.R")
library(dplyr)
#  ------------- Initialize variables we are interested in for modeling -------------

plan.dsn <- c('HOSPINSX','ANNDEDCT', 'HSAACCT', 'PLANMETL')
behaviors <- c('BPCHEK53', 'CHOLCK53', 'NOFAT53', 'CHECK53', 'ASPRIN53', 'PAPSMR53', 
               'BRSTEX53', 'MAMOGR53', 'CLNTST53')
controls <- c('PHOLDER', 'CHBMIX42','BMINDX53','ADGENH42', 'age.cat', 'FAMINC15', 
              'COBRA', 'OOPPREM', 'PREGNT31', 'PREGNT42', 'PREGNT53')
target <- 'IPDIS15'
weights <- 'w'
vars <- c(target, plan.dsn, behaviors, controls, weights)
predVars <- c(plan.dsn, behaviors, controls)
ordered <- c('PLANMETL','ADGENH42', 'age.cat', behaviors)
factors <- c('IPDIS15', 'HOSPINSX', 'HSAACCT','COBRA', 'PREGNT53')

# ------------- Gather data for modeling -------------

meps.2015 <- Join_MEPS(2015)

mepsPrivate <- Filter_MEPS_2015(meps.2015, vars = vars)

#Set target to binary
mepsPrivate$IPDIS15[mepsPrivate$IPDIS15>1] <- 1

#Coerce to fewer factors
mepsPrivate$ANNDEDCT <- as.numeric(mepsPrivate$ANNDEDCT)
for(variable in c(plan.dsn, behaviors)){
  mepsPrivate[mepsPrivate[,variable] < 0, variable] <- 0
}

for(factor in factors){
  mepsPrivate[,factor] <- as.factor(mepsPrivate[, factor])
}
for(factor in ordered){
  mepsPrivate[,factor] <- as.ordered(mepsPrivate[, factor])
}

# ------------- age.cat one hot encoding + split data into train, test -------------

ohe_feats = c('age.cat')
dummies <- dummyVars(~ age.cat, data = mepsPrivate)
ohe <- as.data.frame(predict(dummies, newdata = mepsPrivate))
ohe_combined <- cbind(mepsPrivate[,-c(which(colnames(mepsPrivate) %in% ohe_feats))],ohe)

#ds <- downSample(train, train[,target], list = FALSE)

trainidx <- createDataPartition(ohe_combined$IPDIS15, p=.8, list = FALSE)
all_train <- ohe_combined[trainidx,]
ds <- downSample(all_train, all_train[,target], list = FALSE)
train.y.ds <- ds[,target]
train.y.ds <- as.numeric(levels(train.y.ds))[train.y.ds]
train.x.ds <- ds[,-which(names(ds) %in% c(target, 'Class'))]
train.y <- ohe_combined[trainidx, target]
train.y <- as.numeric(levels(train.y))[train.y]
train.x <- ohe_combined[trainidx,-which(names(ohe_combined) == target)]
test.y <- ohe_combined[-trainidx,target]
test.y <- as.numeric(levels(test.y))[test.y]
test.x <- ohe_combined[-trainidx,-which(names(ohe_combined) == target)]

#-------------- xgboost non-downsampled  ---------------------------------

xgb <- xgboost(data = data.matrix(train.x),
                  label = train.y,
                  max.depth = 2,
                  eta = 1,
                  nthread = 2,
                  nround = 2,
                  objective = "binary:logistic",
                  verbose = 2)

preds.test <- predict(xgb, data.matrix(test.x))
test.Results<-data.frame(obs = test.y,
                         preds = ifelse(preds.test < 0.5, 0, 1))

print(paste('RESULTS FOR NON DOWNSAMPLED xgboost: ', target))
print(confusionMatrix(test.Results$preds, test.Results$obs))
print('')
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')

#-------------- xgboost downsampled  ---------------------------------

xgb.ds <- xgboost(data = data.matrix(train.x.ds),
               label = train.y.ds,
               weight = 
               max.depth = 3,
               eta = 1,
               nthread = 2,
               nround = 4,
               objective = "binary:logistic",
               verbose = 2)

preds.test <- predict(xgb.ds, data.matrix(test.x))
test.ds.Results<-data.frame(obs = test.y,
                            preds = ifelse(preds.test < 0.5, 0, 1))

print(paste('RESULTS FOR DOWNSAMPLED xgboost: ', target))
print(confusionMatrix(test.ds.Results$preds, test.ds.Results$obs))
print('')
print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')

