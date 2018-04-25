
#  ------------- Initialize variables we are interested in for modeling -------------

buildHospModel <- function(train, planVars, behaviorVars, controlVars, nonHospWt, hospWt) {
  
  target <- 'IPDIS15'
  weights <- 'w'
  vars <- c(target, planVars, behaviorVars, controlVars, weights)
  predVars <- c(planVars, behaviorVars, controlVars)
  ordered <- c('PLANMETL','ADGENH42', 'age.cat', behaviors)
  factors <- c('IPDIS15', 'HOSPINSX', 'HSAACCT','COBRA', 'PREGNT53')

  
  #Coerce to fewer factors
  train$ANNDEDCT <- as.numeric(train$ANNDEDCT)
  
  for(variable in c(planVars, behaviorVars)){
    train[,variable] <- as.numeric(train[,variable])
    train[train[,variable] < 0, variable] <- 0
  }
  
  for(factor in factors){
    train[,factor] <- as.factor(train[, factor])
  }
  for(factor in ordered){
    train[,factor] <- as.ordered(train[, factor])
  }
  
  train$w[train$w_copy < 1] <- nonHospWt
  train$w[train$w_copy > 1] <- hospWt
  
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
