#This file will contain supporting files
makeCSV <- function(filename,delimeter,fileOutname){
  tempTable <- read.table(file=filename,sep=delimeter)
  write.csv(tempTable,file=paste(fileOutname,".csv",sep=""))
}
combineCSVs <- function(filename,outputObj,country,day){
  tempDF <- read.csv(file=filename,header=T)
  dayVect <- rep(day,nrow(tempDF))
  countryVect <- rep(country,nrow(tempDF))
  finaltempDF <- cbind(tempDF,countryVect,dayVect)
  #print(finaltempDF)
  outputObj <- rbind(outputObj,finaltempDF)
  return(outputObj)
}