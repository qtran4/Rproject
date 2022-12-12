#Questions (can filter out data that doesn't make sense)
#1 In which country (X or Y) did the disease outbreak likely begin?
#2 If Country Y develops a vaccine for the disease, 
#is it likely to work for citizens of Country X?

##Coding
#Script 1: custom functions, "supportingFunctions.R"


#.txt -> .csv substitute
dir.create ("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/csvlist_Y/")
setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/countryY")
filelist = list.files(pattern = ".txt")
for (i in 1:length(filelist)){
  input<-filelist[i]
  output<-gsub("txt","csv",input)
  data = read.table(input, header = TRUE)   
  setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/csvlist_Y/")
  write.csv(data, file=output, row.names=FALSE)
  setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/countryY")
  
}


# compile data from all .csv files in a directory into a single .csv file
#same columns, also add "country" and "dayofYear" columns.
#user should be able to remove NA rows, include NA rows but be warned, 
#or include NAs without warning

### countryX for loop
setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/countryX/")

# list the files in country X directory and assign to csvlist_X vector
csvlist_X <- list.files(pattern = ".csv")

# add two columns to first csv file 
inputX<-read.csv(csvlist_X[1])
inputX$country <- "X" 
inputX$dayofYear <- as.numeric(substr(csvlist_X[1], 8, 10))

# do a for loop for the rest of the files and combine with rbind
for (i in 2:length(csvlist_X)){
  input<-read.csv(csvlist_X[i])
  input$country <- "X" 
  input$dayofYear <- as.numeric(substr(csvlist_X[i], 8, 10)) 
  inputX<-rbind(inputX, input)
}  

###countryY for loop
setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/csvlist_Y/")

# list the files in country X directory and assign to csvlist_X vector
csvlistY <- list.files(pattern = ".csv")

# add two columns to first csv file 
inputY<-read.csv(csvlistY[1])
inputY$country <- "Y" 
inputY$dayofYear <- as.numeric(substr(csvlistY[1], 8, 10))

# do a for loop for the rest of the files and combine with rbind
for (i in 2:length(csvlistY)){
  Yinput<-read.csv(csvlistY[i])
  Yinput$country <- "Y" 
  Yinput$dayofYear <- as.numeric(substr(csvlistY[i], 8, 10)) 
  inputY<-rbind(inputY, Yinput)
}  

# (check this) Allowing user to remove or include NA columns, with or without warnings
if (naOption == "remove") {
  # Remove rows with NA's in any columns
  data <- data[complete.cases(data),]
} else if (naOption == "warn") {
  # Check for NA's in the data and warn the user if they are present
  if (any(is.na(data))) {
    warning("Data contains NA values")
  }
} else if (naOption == "include") {
  # Do nothing - include NA's in the data without warning the user
} else {
  # Invalid option - raise an error
  stop("Invalid value for naOption parameter")
}

# Return the compiled data frame
return(data)

# how user uses function
# compiledData <- compileData("path/to/directory", "remove")
  # for second argument, include "remove," "warn," or "include"
    # "remove" for removing rows with NA's
    # "warn" for including NA's but showing a warning message
    # "include" for including NA's without a warning

### Combine x and y csv lists
allData.csv<-rbind(inputY, inputX)
setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/")
write.csv(allData.csv, "allData.csv", row.names = F)

#Script 2: analyzing script, source("supportingFunctions.R")

  # Write a function to summarize the compiled data set in terms of
  # number of screens run, percent of patients screened that were infected, 
  # male vs. female patients, and the age distribution of patients.

import allData.csv

def summarize_data(allData.csv)
  num_screens = 0
num_infected = 0
num_male = 0
num_female = 0
age_distribution = {}

with open(allData.csv, 'r') as csvfile:
  reader = csv.reader(allData.csv)
# Skip the header row
next(reader)
for row in reader:
  num_screens += 1
if row[1] == 'Yes':
  num_infected += 1
if row[2] == 'Male':
  num_male += 1
elif row[2] == 'Female':
  num_female += 1

# Count the number of patients in each age group
age = int(row[3])
if age not in age_distribution:
  age_distribution[age] = 0
age_distribution[age] += 1

print('Number of screens run:', num_screens)
print('Percent of patients screened that were infected:', 100 * num_infected / num_screens)
print('Number of male patients:', num_male)
print('Number of female patients:', num_female)
print('Age distribution of patients:', age_distribution)





#summarize: graph or table distribution (just hit all reqs)
#source("supportingFunctions.R")
#will give allthe function I made 