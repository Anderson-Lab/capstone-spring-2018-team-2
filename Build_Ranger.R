library(ranger)
library(caret)
load('data.rda')
load('meta.rda')

# Get data
meps <- Join_MEPS()

# Get vars
plan.dsn <- c('HOSPINSX','ANNDEDCT', 'HSAACCT', 'PLANMETL')
behaviors <- c('BPCHEK53', 'CHOLCK53', 'NOFAT53', 'CHECK53', 'ASPRIN53', 'PAPSMR53', 
               'BRSTEX53', 'MAMOGR53', 'CLNTST53')
controls <- c('PHOLDER', 'CHBMIX42','BMINDX53','ADGENH42', 'AGE15X', 'FAMINC15', 
              'COBRA', 'OOPPREM', 'PREGNT31', 'PREGNT42', 'PREGNT53')
target <- 'IPDIS15'

weights <- 'PERWT15F'
vars <- c(target, plan.dsn, behaviors, controls, weights)


#split
trainidx <- createDataPartition(meps$ERTOT15, p=.8, list = FALSE)
train <- meps[trainidx,]
y.test <- as.data.frame(meps[-trainidx,target])
x.test <- meps[-trainidx,-which(names(meps) == target)]


f <- formula(paste(target, paste(names(rd.p[,names(rd.p) != target]), collapse = '+' ), sep = '~'))
fit <- ranger(formula = f, 
              data = train, 
              num.trees = 50,
              importance = 'impurity',
              min.node.size = 30,
              sample.fraction = .8)

preds <- as.data.frame(predict(fit, x.test)$predictions)
imp <- fit$variable.importance
#save(imp, file = "r-shiny/template/data/ranger_imp.rda") # save the data to the r-shiny directory so that var.importance interactivity can be added
imp <- imp[imp > 50]
library(data.table)
imp.dt<-setDT(as.data.frame(imp), keep.rownames = TRUE)[]
library(ggplot)
ggplot(data=imp.dt, aes(x=rn, y=imp)) +
  geom_bar(stat="identity", fill = "dodgerblue3", color="black") + 
  ggtitle('Variable Importance: Gini Impurity') +
  xlab('Variables') +
  ylab('Relative Importance')+
  coord_flip()

