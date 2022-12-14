getwd()
setwd("/Users/pujasharma/Documents/Phd_data/Rproject/CountryY")
list.files() #just to see the files present in this directory
#list files from any path or folder like country X and Y
#sf means separated by.

#list all .txt file and convert it to csv file 
#
txtTOcsv <- function(path, sf){
  FILES <- list.files(path,pattern = ".txt") 
  for(i in 1:length(FILES)){
    tem <- paste(path,FILES[i], sep = "/") 
    csvfile <- read.table(file = tem, sep = sf, header = T)
    csvOutput <- paste(path,gsub("txt","csv",FILES[i]),sep = "/") 
  }
}

#Function to combine all csv files from countryX and countryY. The combined csv
#files should have original 12 columns along with country and day of year column 
#There should be an option, if there is any NAs in any of the column and you want
#to include NAs, warning should be there and if you want to remove rows with NAs
#in any column without a warning. 


#first argument includes the path or directory that contains all the files
#needed to compile. Second argument would be for NAs
#using gsub function to grab the number that are in each file name. 
compile_csv <- function(path, warnNA){
  FILES <- list.files(path,pattern = ".csv") 
  comFile <- data.frame(matrix(NA,0,14))
  
  for(i in 1:length(FILES)){
    tem <- paste(path,FILES[i], sep = "/") 
    #grab all files and reiterate it by creating for loop
    csvfile <- read.csv(tem)
    country <- gsub("country","",path)
    #using gsub function grab the number that are in each file name. 
    day <- gsub("[^0-9]","",FILES[i]) 
    csvfile <- cbind(csvfile,country,day) 
   #two columns are added "country" and day of year" along with old 12 columns
    colnames(csvfile)[13] <- "country"
    colnames(csvfile)[14] <- "dayofYear"
    comFile <- rbind(comFile, csvfile) 
  }

  availableNAs<- sum(is.na(comFile)) 

  if(warnNA == "Delete"){
    if(availableNAs>0){
      comFile <- na.omit(comFile)
      print("Be cautious, rows with NAs are removed!!!")
    }
  }else if(warnNA == "Warning"){
    if(availableNAs>0){
      print("Be cautious, there is NAs in the data.")
    }else{
      print("There is no any NAs in the data")
    }
  }else if(warnNA != "Delete"&warnNA != "NoWarn"&warnNA != "Warning"){
    print("Something wrong in second variable (related to NAs)")
  }
  return(comFile)
}

# Write a function to summarize the compiled data set in terms of number of screens
# run, percent of patients screened that were infected, male vs. female patients,
# and the age distribution of patients.

sumdata <- function(file){
  
  print("Number of screens run")
  print(nrow(file))
  
  subPatient <- data.frame(matrix(NA,0,14))
  for(i in 1:nrow(file)){
    if(sum(file[i,3:12])>0){
      #sum will be taken from marker1 to marker10 , the one with greater than 
      #zero value will be infected
      subPatient <- rbind(subPatient, file[i,]) 
    }
  }
  
  print("Patients diagnosed as infected is(%): ")
  pata <- nrow(subPatient)/nrow(file)
  print(pata)
  print("Percentage of male vs. female patients")
  patb <- nrow(subPatient[which(subPatient$gender=="male"),])/nrow(subPatient[which(subPatient$gender=="female"),])
  print(patb)
  print("Age distribution of patients")
  print(summary(subPatient$age))
  return(subPatient) 
}

# Includes age distribution of the patient with the year range of 1 to 423 year. 
# Seems a bit weird to me.
