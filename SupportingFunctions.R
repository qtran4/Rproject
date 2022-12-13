#This file will contain supporting files
makeCSV <- function(filename,delimeter,fileOutname){
  tempTable <- read.table(file=filename,sep=delimeter)
  write.csv(tempTable,file=paste(fileOutname,".csv",sep=""),row.names = F)
}

#This function combines all the data from a directory into a big csv file and also
# returns this aggregated data set as a data frame
combineCSVs <- function(directoryname,country,na_handling="none",FileOutName){
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
  write.csv(bigMatrix,file = paste(as.character(FileOutName),".csv",sep=""))
  return(bigMatrix)

}
# This function summarizes the Data Set for a variety of aspects
SummarizeCompiledTests <- function(dfName){
  screen_number <- nrow(dfName)
  running_infected_total <- 0
  #markers.list <- c("marker01","marker02","marker03","marker04","marker05",
  #                  "marker06","marker07","marker08","marker09","marker10")
  for (i in 1:screen_number){
    if(sum(as.numeric(dfName[i,3:12] == "1"))){
      running_infected_total <- running_infected_total +1
      }
  }
  percentage.male <- sum(dfName[,"gender"] == "male") / screen_number * 100
  percentage.female <- 100 - percentage.male
  percentage.juvenile <- sum(dfName[,"age"] < 18) / screen_number * 100
  percentage.adult <- (sum(dfName[,"age"] < 65) / screen_number *100 ) - percentage.juvenile
  percentage.senior <- sum( dfName[,"age"] >= 65) / screen_number *100
  infected <- running_infected_total / screen_number *100
  print(c("Data Summary:",
          paste("There were ",screen_number," screening tests run",sep=""),
          #percentage of males out of all screened
          paste(round(percentage.male,1),"% of patients were male",sep=""), 
          #percentage of females out of all screened
          paste(round(percentage.female,1),"% of patients were female",sep=""),
          #percentage of juveniles out of all screened
          paste(round(percentage.juvenile,1),"% of patients were juveniles (under 18)",sep=""),  
          #percentage of adults out of all screened
          paste(round(percentage.adult,1),"% of patients were adults (18-65)",sep=""),
          #percentage of senior citizens out of all screened
          paste(round(percentage.senior,1),"% of patients were seniors (over 65)",sep=""))
          )
}
  