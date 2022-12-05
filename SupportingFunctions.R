#This file will contain supporting files
makeCSV <- function(filename,delimeter,fileOutname){
  tempTable <- read.table(file=filename,sep=delimeter)
  write.csv(tempTable,file=paste(fileOutname,".csv",sep=""))
}