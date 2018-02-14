SaveData <- function(currentFileName, saveToFileName){
  library(foreign)
  library(caret)
  #have to join two files: person round plan file and this one
  rd <- read.xport(currentFileName)

  pre <- preProcess(rd, method= c('zv', 'nzv'), na.remove = TRUE)

  rd.p <- predict(pre, rd)

  save(rd.p, file = paste(saveToFileName,".rda", sep=''))
  }