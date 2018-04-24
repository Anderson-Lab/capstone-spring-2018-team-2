
#  ------------- Initialize variables we are interested in for modeling -------------

buildHospModel <- function(mepsPrivate, train, planVars, behaviorVars, controlVars, nonHospWt, hospWt) {
  
  target <- 'IPDIS15'
  weights <- 'w'
  vars <- c(target, planVars, behaviorVars, controlVars, weights)
  predVars <- c(planVars, behaviorVars, controlVars)
  ordered <- c('PLANMETL','ADGENH42', 'age.cat', behaviors)
  factors <- c('IPDIS15', 'HOSPINSX', 'HSAACCT','COBRA', 'PREGNT53')
  
  # mepsPrivate = mepsPrivate %>% 
  #   select(w, IPDIS15) %>%
  #   mutate(w = ifelse(IPDIS15 = 0, nonHospWt, hospWt))
  #Set target to binary
  mepsPrivate$w[mepsPrivate$IPDIS15 == 0] <- nonHospWt
  mepsPrivate$w[mepsPrivate$IPDIS15 > 1 ] <- hospWt
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
