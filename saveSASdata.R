SaveData <- function(currentFileName, saveToFileName){
  library(foreign)
  library(caret)
  #have to join two files: person round plan file and this one
  rd <- read.xport(currentFileName)

  save(rd, file = paste(saveToFileName,".rda", sep=''))
}

