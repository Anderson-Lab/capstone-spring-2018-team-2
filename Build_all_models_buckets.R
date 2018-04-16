library(caret)
library(ranger)
source("Join_Data.R")
load('rda_data/mepsBehaviorBuckets.rda')

unlist <- unlist(buckets) 
buckets<-as.data.frame(buckets)
buckets <- droplevels(buckets[buckets$AGE15X >= 40,])
buckets$w <- buckets$IPDIS15
buckets[buckets$w<1, 'w']<- .3

buckets$IPDIS15[buckets$IPDIS15>1] <- 1
buckets$behave_bucket <- as.factor(buckets$behave_bucket)
buckets$ANNDEDCT <- as.numeric(buckets$ANNDEDCT)
buckets$age.cat <- Age.to.Cat(buckets, 'AGE15X')
plan.dsn <- c('ANNDEDCT', 'HSAACCT', 'PLANMETL', 'OOPPREM')
controls <- c('AGE15X', 'FAMINC15', 'SEX',
              'COBRA')
weights <- 'w'
behaviors <- 'behave_bucket'
target <- 'IPDIS15'

vars <- c(target, plan.dsn, behaviors, controls, weights)
predVars <- c(plan.dsn, behaviors, controls)

factors <- c('IPDIS15', 'ANNDEDCT' ,'PLANMETL', 'HSAACCT', 'COBRA')
buckets[buckets$OOPPREM < 0, 'OOPPREM'] <- 0


for(variable in c("ANNDEDCT","OOPPREM" , "FAMINC15", "SEX", "COBRA")){
  buckets[buckets[,variable] < 0, variable] <- 0
}

td <- buckets[,c(predVars, target, weights)]

for(factor in factors){
  td[,factor] <- as.factor(td[, factor])
}

get_metrics = function(preds, test, cutoff){
  results <- preds
  results$Obs <- test
  results$Pred <- ifelse(preds[,1] > cutoff, 0, 1)
  print(confusionMatrix(results$Pred, results$Obs))
}

#split
trainidx <- createDataPartition(td$IPDIS15, p=.8, list = FALSE)
train <- td[trainidx,vars]
y.test <- td[-trainidx,target]
x.test <- td[-trainidx,-which(names(td) == target)]

ds <- downSample(train, train[,target], list = FALSE)
f <- formula(paste(target, paste(predVars, collapse = '+' ), sep = '~'))
lin.model <- glm(f,family=binomial(),data=train, weights = train$w)
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

lin.preds <- predict(lin.model, x.test, type = 'response')
lin.preds.test <- tibble(NoHosp=1-lin.preds, Hosp=lin.preds)
rf.preds.test <- as_tibble(predict(rf.model, x.test)$predictions) %>% rename("NoHosp" = '0', "Hosp" = '1')
get_metrics(lin.preds.test, y.test, 0.5)
get_metrics(rf.preds.test, y.test, 0.5)


lin.model.ds <- glm(f,family=binomial(),data=ds, weights = ds$w)
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


lin.preds.ds <- predict(lin.model.ds, x.test, type = 'response')
lin.preds.ds.test <- tibble(NoHosp=1-lin.preds.ds, Hosp=lin.preds.ds)
rf.preds.ds.test <- as_tibble(predict(rf.model.ds, x.test)$predictions) %>% rename("NoHosp" = '0', "Hosp" = '1')
get_metrics(lin.preds.ds.test, y.test, 0.5)
get_metrics(rf.preds.ds.test, y.test, 0.5)


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
lin.model.b.preds <- predict(lin.model.b, x.test, type = 'response')
lin.model.b.preds.test <- tibble(NoHosp=1-lin.model.b.preds, Hosp=lin.model.b.preds)
rf.model.b.preds.test <- as_tibble(predict(rf.model.b, x.test)$predictions)
get_metrics(lin.preds.test, y.test, 0.5)
get_metrics(rf.model.b.preds.test, y.test, 0.5)

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

lin.model.b.ds.preds <- predict(lin.model.b.ds, x.test, type = 'response')
lin.model.b.ds.preds.test <- tibble(NoHosp=1-lin.model.b.ds.preds, Hosp=lin.model.b.ds.preds)
rf.model.b.ds.preds.test <- as_tibble(predict(rf.model.b.ds, x.test)$predictions)
get_metrics(lin.preds.test, y.test, 0.5)
get_metrics(rf.model.b.ds.preds.test, y.test, 0.5)

print("Trained models are: lin.model, lin.model.ds, #lin.model.b, #lin.model.b.ds, rf.model, rf.model.ds,rd.model.b, and rf.model.b.ds")
