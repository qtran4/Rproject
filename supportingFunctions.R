# Introduction to Biocomputing - R Project
# Written by Xiyuan Guan
# 2022-12-2
# Supporting Functions

workpath <- "C:/Users/guanx/Documents/N.ND/BIOS 60318 Biocomputing/test/Rproject/Rproject/"
setwd(workpath)
list.files()


# Write a function that converts all files in a directory with space- or tab-delimited data (.txt) into comma-separated value files.
# Usage: trans.csv($1,$2), $1 is the directory that contains the files to be transfered into csv format.
# $2 is the separation in txt files.
# Example: trans.csv("countryY"," ")
trans.csv <- function(direct, sepf){
  #list.files(direct) 
  files <- list.files(direct,pattern = ".txt") # get all .txt files
  for(i in 1:length(files)){
    f <- paste(direct,files[i], sep = "/") # generate file path
    csvf <- read.table(file = f, sep = sepf, header = T)
    csvname <- paste(direct,gsub("txt","csv",files[i]),sep = "/") # generate output file path
    write.csv(csvf, file = csvname, row.names = F)
  }
}


# Write a function to compile data from all .csv files in a directory into a single .csv file.
# The user should be able to choose whether they want to remove rows with NA’s in any columns, 
# include NAs in the compiled data but be warned of their presence, 
# or include NAs in the compiled data without a warning

# Usage: combine.csv($1,$2), $1 is the directory that contains the files to be combined.
# $2 is the treatment of NA, could be choose from Delete (delete rows with NA), Warn (include NAs but warn) and NoWarn (include NAs w/o warn).
# Example: sumtable1 <- combine.csv("countryY","Warn")

combine.csv <- function(direct, NAWarn){
  files <- list.files(direct,pattern = ".csv") # get all .csv files in the directory
  cFile <- data.frame(matrix(NA,0,14)) # normalize the data.frame
  
  for(i in 1:length(files)){
    f <- paste(direct,files[i], sep = "/") # get file path
    csvf <- read.csv(f)
    country <- gsub("country","",direct)
    day <- gsub("[^0-9]","",files[i]) # get the number in the file name
    csvf <- cbind(csvf,country,day) # generate extra columns
    colnames(csvf)[13] <- "country"
    colnames(csvf)[14] <- "dayofYear"
    cFile <- rbind(cFile, csvf) 
  }
  nNAs<- sum(is.na(cFile)) # num of NAs in the compiled dataset
  if(NAWarn == "Delete"){
    if(nNAs>0){
      cFile <- na.omit(cFile)
      print("rows with NA are removed")
    }
  }else if(NAWarn == "Warn"){
    if(nNAs>0){
      print("Warning, NAs in the dataset.")
    }else{
      print("No NAs in the dataset")
    }
  }else if(NAWarn != "Delete"&NAWarn != "NoWarn"&NAWarn != "Warn"){
    # Increase Robustness
    print("Wrong input for the second variable")
  }
  return(cFile) # return the complied data of the directory
}

# Write a function to summarize the compiled data set
# Usage: subset <- sumdata(file)
# Example: file <- read.csv("allData.csv")
# sub.pat <- sumdata("allData.csv")

sumdata <- function(file){
  # number of screens run
  
  print("The number of screens run is:")
  print(nrow(file))
  
  sub.pat <- data.frame(matrix(NA,0,14))
  for(i in 1:nrow(file)){
    if(sum(file[i,3:12])>0){
      sub.pat <- rbind(sub.pat, file[i,]) # generate subset of patients
    }
  }
  # percent of patients screened that were infected
  print("Percent of patients screened that were infected is:")
  p1 <- nrow(sub.pat)/nrow(file)
  print(p1)
  # male vs. female patients
  print("Percent of male vs. female patients is:")
  p2 <- nrow(sub.pat[which(sub.pat$gender=="male"),])/nrow(sub.pat[which(sub.pat$gender=="female"),])
  print(p2)
  # the age distribution of patients
  print("the age distribution of patients is:")
  print(summary(sub.pat$age))
  # ????? 423 yrs old patient
  return(sub.pat) # return the subset of patients
}
