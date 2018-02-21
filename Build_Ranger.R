library(ranger)
library(caret)
library(dplyr)
load('meta.rda')

# Get data
meps <- Join_MEPS()

mepsPublic<-Public_Filter(meps)

mepsPrivate<-Private_Filter(meps)

# Get vars
plan.dsn <- c('HOSPINSX','ANNDEDCT', 'HSAACCT', 'PLANMETL')
behaviors <- c('BPCHEK53', 'CHOLCK53', 'NOFAT53', 'CHECK53', 'ASPRIN53', 'PAPSMR53', 
               'BRSTEX53', 'MAMOGR53', 'CLNTST53')
controls <- c('PHOLDER', 'CHBMIX42','BMINDX53','ADGENH42', 'AGE15X', 'FAMINC15', 
              'COBRA', 'OOPPREM', 'PREGNT31', 'PREGNT42', 'PREGNT53')
target <- 'IPDIS15'
weights <- 'PERWT15F'
vars <- c(target, plan.dsn, behaviors, controls, weights)
predVars <- c(plan.dsn, behaviors, controls)
factors <- c('IPDIS15', 'HOSPINSX', 'PLANMETL', 'HSAACCT', behaviors, 'PHOLDER',
             'CHBMIX42', 'ADGENH42','COBRA', 'OOPPREM', 'PREGNT31', 'PREGNT42', 'PREGNT53')
#Set target to binary
meps$IPDIS15[meps$IPDIS15>1] <- 1
for(factor in factors){
  meps[,factor] <- as.factor(meps[, factor])
}

#split
trainidx <- createDataPartition(meps$IPDIS15, p=.8, list = FALSE)
train <- meps[trainidx,vars]
y.test <- meps[-trainidx,target]
x.test <- meps[-trainidx,-which(names(meps) == target)]

w<-ifelse(train[, target] == '1', 12,1)
f <- formula(paste(target, paste(predVars, collapse = '+' ), sep = '~'))
fit <- ranger(formula = f,
              data = train,
              case.weights = w,
              num.trees = 50,
              importance = 'impurity',
              min.node.size = 30,
              probability = TRUE,
              classification = TRUE,
              sample.fraction = .8)



# get model performance metrics
preds.train <- as.data.frame(predict(fit, train[,predVars])$predictions)
preds.test <- as.data.frame(predict(fit, x.test)$predictions)
classNames <- c('NoHosp', 'Hosp')
levels(train[,target])<-classNames
levels(y.test)<-classNames

colnames(preds.train)<-classNames
colnames(preds.test)<-classNames
cutOff = .8
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

plot(performance(pred, "sens" , x.measure = "cutoff"), col = 'red', ylab= NULL)
par(mar=c(4,4,4,4))
par(new=T)
plot(performance(pred, "spec" , x.measure = "cutoff"),add = TRUE, col = 'blue', xlab = NULL)
axis(side = 4,  at = .5, labels = 'specificity', padj = 1 )
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
#save(imp, file = "r-shiny/template/data/ranger_imp.rda") # save the data to the r-shiny directory so that var.importance interactivity can be added
#imp <- imp[imp > 50]
library(data.table)
imp.dt<-setDT(as.data.frame(imp), keep.rownames = TRUE)[]
library(ggplot2)
ggplot(data=imp.dt, aes(x=reorder(rn,imp), y=imp)) +
  geom_bar(stat="identity", fill = "dodgerblue3", color="black") + 
  ggtitle('Variable Importance: Gini Impurity') +
  xlab('Variables') +
  ylab('Relative Importance')+
  coord_flip()

