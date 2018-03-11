library(ranger)
library(caret)
library(dplyr)
library(data.table)
library(ggplot2)
source("Join_Data.R")
#load('meta.rda')

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
weights <- 'PERWT15F'
vars <- c(plan.dsn, behaviors, controls)
predVars <- c(plan.dsn, controls)
factors <- c(plan.dsn, behaviors, 'PHOLDER','CHBMIX42', 'ADGENH42','COBRA', 
             'OOPPREM', 'PREGNT31', 'PREGNT42', 'PREGNT53')

behavior_models = vector("list", length(behaviors))
names(behavior_models) = behaviors


td <- mepsPrivate[,vars]
#Coerce to fewer factors
td$ANNDEDCT <- as.numeric(td$ANNDEDCT)
for(variable in c(plan.dsn, behaviors)){
  td[td[,variable] < 0, variable] <- "Unknown"
}

for(factor in factors){
  td[,factor] <- as.factor(td[, factor])
}
#Create a model for each behavior
trainidx <- createDataPartition(td$BMINDX53, p=.8, list = FALSE)
for(target in behaviors){
  train <- td[trainidx,vars]
  y.test <- td[-trainidx,target]
  x.test <- td[-trainidx,-which(names(td) == target)]
  

  f <- formula(paste(target, paste(predVars, collapse = '+' ), sep = '~'))
  fit <- ranger(formula = f,
                data = train,
                #case.weights = w,
                num.trees = 50,
                importance = 'impurity',
                min.node.size = 30,
                probability = TRUE,
                classification = TRUE,
                sample.fraction = .8)
  
  
  
  # get model performance metrics
  preds.train <- as.data.frame(predict(fit, train[,predVars])$predictions)
  preds.test <- as.data.frame(predict(fit, x.test)$predictions)
  preds.train$PREDICTION <- colnames(preds.train)[max.col(preds.train,ties.method="first")]
  preds.train$PREDICTION <- as.factor(preds.train$PREDICTION)
  preds.test$PREDICTION <- colnames(preds.test)[max.col(preds.test,ties.method="first")]
  preds.test$PREDICTION <- as.factor(preds.test$PREDICTION)
  print(paste('RESULTS FOR', target))
  print(confusionMatrix(preds.train$PREDICTION, train[,target]))
  print(confusionMatrix(preds.test$PREDICTION, y.test))
  print('')
  print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
  
  #Var Imp Plots
  imp <- fit$variable.importance
  
  behavior_models[[target]] = imp
  # for (i in seq_along(imp)){
  #   print(imp[i])
  # }
  #imp <- imp[imp > 50]
  imp.dt<-setDT(as.data.frame(imp), keep.rownames = TRUE)[]
  imp.dt.top <- head(arrange(imp.dt,desc(imp)), n = 10)
  #save(imp.dt, file = "r-shiny/template/data/ranger_imp.rda") # save the data to the r-shiny directory so that var.importance interactivity can be added
  
  print(ggplot(data=imp.dt.top, aes(x=reorder(rn,imp), y=imp)) +
    geom_bar(stat="identity", fill = "dodgerblue3", color="black") + 
    ggtitle(paste('Predicting:',target)) +
    xlab('Variables') +
    ylab('Relative Importance')+
    coord_flip())
  
  
}

# save the data to the r-shiny directory so that var.importance of each behavior interactivity can be added
save(behavior_models, file = "r-shiny/template/data/behavior_models.rda") 


