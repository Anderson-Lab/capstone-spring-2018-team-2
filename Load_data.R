library(foreign)
library(caret)
rd <- read.xport('h181.ssp')

pre <- preProcess(rd, method= c('zv', 'nzv'), na.remove = TRUE)

rd.p <- predict(pre, rd)