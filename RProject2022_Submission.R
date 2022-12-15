#Questions (can filter out data that doesn't make sense)
#1 In which country (X or Y) did the disease outbreak likely begin?
#2 If Country Y develops a vaccine for the disease, 
#is it likely to work for citizens of Country X?
##Coding
#Script 1: custom functions, "supportingFunctions.R"
#.txt -> .csv substitute

setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/countryY")
dir.create ("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/csvlist_Y/")
filelist = list.files(pattern = ".txt")
txt2csv <- function(filelist){
  for (i in 1:length(filelist)){
    input<-filelist[i]
    output<-gsub("txt","csv",input)
    data = read.table(input, header = TRUE)   
    setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/csvlist_Y/")
    write.csv(data, file=output, row.names=FALSE)
    setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/countryY")
    
  } 
}

# compile data from all .csv files in a directory into a single .csv file
#same columns, also add "country" and "dayofYear" columns.
#user should be able to remove NA rows, include NA rows but be warned, 
#or include NAs without warning

#Path directory
setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/")

#Compile
compileData <- function(directory,country,naOption){
  #setting directory variable while testing , delete when functions works
  setwd(directory)
  csvlist <- list.files(path=directory, pattern = ".csv")
  input<-read.csv(csvlist[1])
  input$country <-  country
  input$dayofYear <- as.numeric(substr(csvlist[1], 8, 10))
  
  for (i in 2:length(csvlist)){
    inputloop<-read.csv(csvlist[i])
    inputloop$country <- country 
    inputloop$dayofYear <- as.numeric(substr(csvlist[i], 8, 10)) 
    input=rbind(input,inputloop)
  } 
  
  if (naOption == "remove") {
    # Remove rows with NA's in any columns
    input <- na.omit(input)
  } else if (naOption == "warn") {
    # Check for NA's in the data and warn the user if they are present
    if (any(is.na(input))) {
      warning("Data contains NA values")
    }
  } else if (naOption == "include") {
    # Do nothing - include NA's in the data without warning the user
  } else {
    # Invalid option - raise an error
    stop("Invalid value for naOption parameter")
  }
  write.csv(input, paste("country",country,"_alldata.csv"), row.names = F)
  
}