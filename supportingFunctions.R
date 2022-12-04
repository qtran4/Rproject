##R Final Project
setwd("/Users/bethoceguera/Documents/PhD Year 1/First Semester/Introduction to Biocomputing/Rproject")

##FUNCTION 1
# Function that converts all files in a directory with space- or 
#   tab-delimited data (.txt) into comma-separated value files.

csvConvert <- function(dir){
  setwd(dir)
  filesToConvert <- list.files(pattern = ".txt") 
  for (i in 1:length(filesToConvert)){
  input <- filesToConvert[i]  
  output <- paste0(gsub("\\.txt$", "", input), ".csv")
  data <- read.table(input, sep="",header=TRUE)
  write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
  file.remove(input)  
  }
}

#USAGE 
# Define and run variable "dir" as the directory path that holds the files to be converted
# Run "csvConvert(dir)"

#EXAMPLE: convert and remove .txt files in "countryY" directory
dir <- "/Users/bethoceguera/Documents/PhD Year 1/First Semester/Introduction to Biocomputing/Rproject/countryY" 
csvConvert(dir)


##FUNCTION 2
# Function that compiles data from all .csv files in a directory 
# into a single .csv file. The compiled data has the original twelve columns 
# from daily data sheets, but also country and dayofYear columns. 
# The user should be able to choose whether they want to remove rows with NA’s 
# in any columns, include NAs in the compiled data but be warned of their presence, or include NAs in the compiled data without a warning

csvMerge<-function(dir,naChoice){
  myPath <- list.files(path = dir, recursive = TRUE, pattern = "*.csv", full.names = TRUE)
  dirName <-dirname(path= myPath)
  dayofYear <-'dayofYear'
  country<-'country'
  header<-read.csv(myPath[1],header = TRUE, stringsAsFactors = FALSE) 
  for (i in seq_along(myPath)){
    if (naChoice=="1"){
      data<-read.csv(myPath[i],header = TRUE, stringsAsFactors = FALSE)
      noNAs <-na.omit(data)
      noNAs[,dayofYear]<- gsub(".+([0-9]{3}).*", "\\1", myPath[i])
      noNAs[,country]<-gsub(".+([A-Z]{1}).*", "\\1", dirName[i])
      header<-rbindlist(list(header,noNAs),fill = TRUE)  
    }
    if (naChoice=="2") {
      print("WARNING: NAs present within the data have not been ommitted!")
      data<-read.csv(myPath[i],header = TRUE, stringsAsFactors = FALSE)
      data[,dayofYear]<- gsub(".+([0-9]{3}).*", "\\1", myPath[i])
      data[,country]<-gsub(".+([A-Z]{1}).*", "\\1", dirName[i])
      header<-rbindlist(list(header,data),fill = TRUE)
    }
    if (naChoice=="3"){
      data<-read.csv(myPath[i],header = TRUE, stringsAsFactors = FALSE)
      data[,dayofYear]<- gsub(".+([0-9]{3}).*", "\\1", myPath[i])
      data[,country]<-gsub(".+([A-Z]{1}).*", "\\1", dirName[i])
      header<-rbindlist(list(header,data),fill = TRUE)
    }
  }
  header<-(header[-1,])  
  write.csv(header,file = "allDataTest.csv")
}

#USAGE 
# Define and run variable "dir" as the directory path that holds the files to be converted
# Define and run variable "naChoice":
#     naChoice <- 1 = Remove rows with NAs in any columns
#     naChoice <- 2 = Include NAs in the compiled data but be warned of their presence
#     naChoice <- 3 = Include NAs in the compiled data without a warning
# Run "csvMerge(dir,naChoice)"

#EXAMPLE: Append all .csv files within the RProject directory (Recursive)
dir <- "/Users/bethoceguera/Documents/PhD Year 1/First Semester/Introduction to Biocomputing/Rproject"
naChoice <- 1
csvMerge(dir,naChoice)


##FUNCTION 3
# Function to summarize the compiled data set in terms of 
#   3a. Number of screens run
#   3b. Percent of patients screened that were infected
#   3c. Male vs. female patients
#   3d. Age distribution of patients





  
 
  
  
  


