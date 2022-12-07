#this R file will be used to analyze the patient data
# with the help of the functions in SupportingFunctions.R

#setting current working directory; Change for your system
setwd("/Users/kevinbuck/Desktop/Biocomputing/Exercises/Rproject")
source("SupportingFunctions.R")

#turning all the txt files for Country Y into CSV files
for (i in 120:175){
  makeCSV(filename = paste("countryY/screen_",i,".txt",sep=""),delimeter = " ",
          fileOutname = paste("countryY/screen_",i,sep=""))
}
colsList <- list("gender","age","marker01","marker02","marker03","marker04",
                 "marker05","marker06","marker07","marker08","marker09","marker10",
                 "country","dayofYear")
bigDF <- matrix(data=NA,nrow=0,ncol=14)
colnames(bigDF) = colsList
for(i in 120:175){
  bigDF <- combineCSVs(filename=paste("countryX/screen_",i,".csv",sep=""),outputObj=bigDF,
          country="x",day=i)
  bigDF <- combineCSVs(filename=paste("countryY/screen_",i,".csv",sep=""),outputObj=bigDF,
          country="y",day=i)
}


