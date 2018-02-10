library(ranger)
library(caret)
load('data.rda')
load('meta.rda')

target <- 'ERTOT15'
descriptions[target]

#split
trainidx <- createDataPartition(rd.p$ERTOT15, p=.8, list = FALSE)
train <- rd.p[trainidx,]
y.test <- as.data.frame(rd.p[-trainidx,target])
x.test <- rd.p[-trainidx,-which(names(rd.p) == target)]


f <- formula(paste(target, paste(names(rd.p[,names(rd.p) != target]), collapse = '+' ), sep = '~'))
fit <- ranger(formula = f, 
              data = train, 
              num.trees = 50,
              importance = 'impurity',
              min.node.size = 30,
              sample.fraction = .8)

preds <- as.data.frame(predict(fit, x.test)$predictions)
varImpPlot(fit)
imp <- fit$variable.importance
save(imp, file = "r-shiny/template/data/ranger_imp.rda") # save the data to the r-shiny directory so that var.importance interactivity can be added
imp <- imp[imp > 50]
barplot(imp, horiz=TRUE, las= 1)
