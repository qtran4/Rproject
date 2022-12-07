#This file will contain supporting files
makeCSV <- function(filename,delimeter,fileOutname){
  tempTable <- read.table(file=filename,sep=delimeter)
  write.csv(tempTable,file=paste(fileOutname,".csv",sep=""),row.names = F)
}
combineCSVs <- function(directoryname,country,na_handling="none"){
  #making list of all .csv files in directory using grep to search for .csv
  csv.files.list <- dir(directoryname)[grep(".csv",dir(directoryname))] 
  #initiating matrix for final results
  bigMatrix <- matrix(data=NA,nrow=0,ncol=14)
  for(csv.file in csv.files.list){
    #initiating temp data obj for raw data from csv
    tempDF <- read.csv(file=paste(directoryname,csv.file,sep=""),header=T)
    day <- gsub("[^0-9]*","",csv.file)
    day = gsub("[_]","",day) # using char replace to extract the day from file name
    day.vect <- rep(day,n=nrow(tempDF)) # making vector of day name to bind to tempDF
    country.vect <- rep(country,n=nrow(tempDF)) # making vector of country to bind to tempDF
    expanded.tempDF <- cbind(tempDF,country.vect,day.vect) # binding day and country col to tempDF
    bigMatrix <- rbind(bigMatrix,expanded.tempDF) #binding the new completed DF to existing large one
  }
  #renaming columns of the final df
  colsList <- list("gender","age","marker01","marker02","marker03","marker04",
                   "marker05","marker06","marker07","marker08","marker09","marker10",
                   "country","dayofYear")
  colnames(bigMatrix) = colsList
  #code allowing user to request option removal of or warning about NA's present in data
  if(na_handling == "remove"){
    bigMatrix=na.omit(bigMatrix)
    print("Any NA Values in data removed")
    }
  if(na_handling == "warn" ){
    if(any(is.na.data.frame(bigMatrix))){print("Warning : Your Data Contains NA Values")}else{
      print("No NA Values Present")
    }
    }
  return(bigMatrix)
}
  
  