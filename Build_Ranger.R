library(ranger)
library(caret)
library(dplyr)
source("Join_Data.R")
load('meta.rda')

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
meps.2014 <- Join_MEPS(2014)
meps.2013 <- Join_MEPS(2013)

# mepsPrivate <- Combine_MEPS_Years(meps.2015, 
#                          meps.2014, 
#                          meps.2013, 
#                          join_vars = vars[!vars %in% 'PLANMETL'])

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

#split
trainidx <- createDataPartition(mepsPrivate$IPDIS15, p=.8, list = FALSE)
train <- mepsPrivate[trainidx,vars]
y.test <- mepsPrivate[-trainidx,target]
x.test <- mepsPrivate[-trainidx,-which(names(mepsPrivate) == target)]

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

save(fit, file = "r-shiny/template/data/ranger_hosp_fit.rda")


# get model performance metrics
preds.train <- as.data.frame(predict(fit, train[,predVars])$predictions)
preds.test <- as.data.frame(predict(fit, x.test)$predictions)
save(preds.test, file = "r-shiny/template/data/ranger_hosp_preds.rda")
save(y.test, file = "r-shiny/template/data/ranger_hosp_true.rda")
classNames <- c('NoHosp', 'Hosp')
levels(train[,target])<-classNames
levels(y.test)<-classNames

colnames(preds.train)<-classNames
colnames(preds.test)<-classNames
cutOff = .7
train.Results<-data.frame(preds.train, 
                          obs = train[,target],
                          pred = ifelse(preds.train[,classNames[1]] < cutOff, classNames[1], classNames[2]))
test.Results<-data.frame(preds.test, 
                         obs = y.test,
                         pred = ifelse(preds.test[,classNames[1]] < cutOff, classNames[1], classNames[2]))
# test Results
levels(train.Results$pred) <- classNames
levels(test.Results$pred) <- classNames
twoClassSummary(train.Results, lev = classNames)
twoClassSummary(test.Results, lev = classNames) 


# Performance Plots

library(ROCR)
## find the best cut off
pred <- prediction( preds.train[,1],  train[,target])

plot(performance(pred, "sens" , x.measure = "cutoff"), col = 'red', ylab= NULL, main="Optimal Cutoff")
par(mar=c(4,4,4,4))
par(new=T)
plot(performance(pred, "spec" , x.measure = "cutoff"),add = TRUE, col = 'blue', xlab = NULL)
axis(side = 4,  at = .5, labels = 'specificity', padj = 1 )
legend(.3, .9, legend=c("Sensitivity", "Specificity"),
       col=c("red", "blue"), lty=1, cex=0.8)
#x<-locator()



plot(performance(pred, "tpr" , x.measure = "fpr"), col = 'red', ylab= NULL)
abline(0,1)
plot(performance(pred, "acc" , x.measure = "cutoff"), col = 'red', ylab= NULL)

performance(pred, "auc")





#Var Imp Plots

#x<-fit$variable.importance
#x<-x[order(x)]
#par(mar = c(4,10,4,4))
#barplot(x, horiz = TRUE, las = 1)


imp <- fit$variable.importance
#imp <- imp[imp > 50]
library(data.table)
imp.dt<-setDT(as.data.frame(imp), keep.rownames = TRUE)[]
imp.dt.top <- head(arrange(imp.dt,desc(imp)), n = 10)
save(imp.dt, file = "r-shiny/template/data/ranger_imp.rda") # save the data to the r-shiny directory so that var.importance interactivity can be added

library(ggplot2)
ggplot(data=imp.dt.top, aes(x=reorder(rn,imp), y=imp)) +
  geom_bar(stat="identity", fill = "dodgerblue3", color="black") + 
  ggtitle('Variable Importance: Gini Impurity') +
  xlab('Variables') +
  ylab('Relative Importance')+
  coord_flip()

