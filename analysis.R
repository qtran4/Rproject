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

#making giant data frame of screening data from both countries
countryX.df <- combineCSVs(directoryname = "countryX/", country="X",na_handling = "remove")
countryY.df <- combineCSVs(directoryname = "countryY/", country="Y",na_handling = "remove")

allData.df <- rbind(countryX.df,countryY.df)


