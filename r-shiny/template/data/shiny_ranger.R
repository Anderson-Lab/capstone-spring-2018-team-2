library(ranger)
library(caret)
library(dplyr)
#  ------------- Initialize variables we are interested in for modeling -------------

buildHospModel <- function(mepsPrivate, train, planVars, behaviorVars, controlVars) {
  
  target <- 'IPDIS15'
  weights <- 'w'
  vars <- c(target, planVars, behaviorVars, controlVars, weights)
  predVars <- c(planVars, behaviorVars, controlVars)
  ordered <- c('PLANMETL','ADGENH42', 'age.cat', behaviors)
  factors <- c('IPDIS15', 'HOSPINSX', 'HSAACCT','COBRA', 'PREGNT53')
  
  #Set target to binary
  mepsPrivate$IPDIS15[mepsPrivate$IPDIS15>1] <- 1
  
  #Coerce to fewer factors
  mepsPrivate$ANNDEDCT <- as.numeric(mepsPrivate$ANNDEDCT)
  
  for(variable in c(planVars, behaviorVars)){
    mepsPrivate[,variable] <- as.numeric(mepsPrivate[,variable])
    mepsPrivate[mepsPrivate[,variable] < 0, variable] <- 0
  }
  
  for(factor in factors){
    mepsPrivate[,factor] <- as.factor(mepsPrivate[, factor])
  }
  for(factor in ordered){
    mepsPrivate[,factor] <- as.ordered(mepsPrivate[, factor])
  }
  
  #ds <- downSample(train, train[,target], list = FALSE)
  f <- formula(paste(target, paste(predVars, collapse = '+' ), sep = '~'))
  fit <- ranger(formula = f,
                data = train,
                case.weights = train$w,
                num.trees = 2500,
                importance = 'impurity',
                min.node.size = 150,
                probability = TRUE,
                classification = TRUE,
                sample.fraction = .7)
  
  return(fit)
  
}

  # get model performance metrics
  


  # Performance Plots

  # library(ROCR)
  # ## find the best cut off
  # pred <- prediction( preds.train[,1],  train[,target])
  # 
  # plot(performance(pred, "sens" , x.measure = "cutoff"), col = 'red', ylab= NULL, main="Optimal Cutoff")
  # par(mar=c(4,4,4,4))
  # par(new=T)
  # plot(performance(pred, "spec" , x.measure = "cutoff"),add = TRUE, col = 'blue', xlab = NULL)
  # axis(side = 4,  at = .5, labels = 'specificity', padj = 1 )
  # legend(.3, .9, legend=c("Sensitivity", "Specificity"),
  #        col=c("red", "blue"), lty=1, cex=0.8)
  # 
  # plot(performance(pred, "tpr" , x.measure = "fpr"), col = 'red', ylab= NULL)
  # abline(0,1)
  # plot(performance(pred, "acc" , x.measure = "cutoff"), col = 'red', ylab= NULL)
  # 
  # performance(pred, "auc")

