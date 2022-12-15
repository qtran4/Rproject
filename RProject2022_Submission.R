#Questions (can filter out data that doesn't make sense)
#1 In which country (X or Y) did the disease outbreak likely begin?
#2 If Country Y develops a vaccine for the disease, 
#is it likely to work for citizens of Country X?

##Coding
#Script 1: custom functions, "supportingFunctions.R"



#.txt -> .csv substitute

setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/countryY")
dir.create ("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/csvlist_Y/")

txt2csv <- function(filelist){
  directory=args$arg1
  mkdir=args$arg2
  filelist = list.files(pattern = ".txt")
  setwd(directory)
  dir.create (mkdir)
  
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


































#Questions (can filter out data that doesn't make sense)
#1 In which country (X or Y) did the disease outbreak likely begin?
#2 If Country Y develops a vaccine for the disease, 
#is it likely to work for citizens of Country X?

##Coding
#Script 1: custom functions, "supportingFunctions.R"


#.txt -> .csv substitute

setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/countryY")
dir.create ("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/csvlist_Y/")
txt2csv <- function(filelist){
  filelist = list.files(pattern = ".txt")
  for (i in 1:length(filelist)){
    input<-filelist[i]
    output<-gsub("txt","csv",input)
    data = read.table(input, header = TRUE)   
    setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/csvlist_Y/")
    write.csv(data, file=output, row.names=FALSE)
    setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/countryY")
    
  } 
}
  

#####txt2csv(directory="/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/countryY", input)

# compile data from all .csv files in a directory into a single .csv file
#same columns, also add "country" and "dayofYear" columns.
#user should be able to remove NA rows, include NA rows but be warned, 
#or include NAs without warning

#Path directory
setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/")

compileData(directory = "/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/",country = X)
compileData(directory=, country=Y)
#Compile
compileData <- function(directory,country,naOption){
  #setting directory variable while testing , delete when functions works
  directory= setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/countryX")
  country="X"
  csvlist <- list.files(path=directory, pattern = ".csv")
  inputX<-read.csv(csvlist[1])
  inputX$country <-  country
  inputX$dayofYear <- as.numeric(substr(csvlist[1], 8, 10))
  
  for (i in 2:length(csvlist)){
    inputloopX<-read.csv(csvlist[i])
    inputloopX$country <- country 
    inputloopX$dayofYear <- as.numeric(substr(csvlist[i], 8, 10)) 
    inputX=rbind(inputX,inputloopX)
  }  
  #setting directory variable while testing , delete when functions works
  directory= setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/csvlist_Y")
  country="Y"
  csvlistY <- list.files(path=directory, pattern = ".csv")
  inputY<-read.csv(csvlistY[1])
  inputY$country <-  country
  inputY$dayofYear <- as.numeric(substr(csvlist[1], 8, 10))
  
  for (i in 2:length(csvlist)){
    inputloopY<-read.csv(csvlist[i])
    inputloopY$country <- country 
    inputloopY$dayofYear <- as.numeric(substr(csvlist[i], 8, 10)) 
    inputY=rbind(inputY,inputloopY)
  }  
  input=rbind(inputY,inputX)
  
  if (naOption == "remove") {
    # Remove rows with NA's in any columns
    input <- input[-is.na(input),]
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
  write.csv(input, paste0("country",country,"_alldata.csv"), row.names = F)
  
}


# how user uses function
# compiledData <- compileData("path/to/directory", "remove")
  # for second argument, include "remove," "warn," or "include"
    # "remove" for removing rows with NA's
    # "warn" for including NA's but showing a warning message
    # "include" for including NA's without a warning



#Script 2: analyzing script, source("supportingFunctions.R")

  # Write a function to summarize the compiled data set in terms of
  # number of screens run, percent of patients screened that were infected, 
  # male vs. female patients, and the age distribution of patients.


#percentage male, percentage female, percentage positive, percentage negative, age distribution
summarydata<-read.csv("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/countryX/_alldata.csv")
summarizedCompileData(data=summarydata)
summarizedCompileData <- function(data){
#directions
  
  # find the number of rows in the data frame
  nrow(summarydata)
  
  #know number of male and female patients
  sum(summarydata$gender == "male")
  #percentage
  sum(summarydata$gender == "female")
  #percentage
 
   #number of infected patients
      #sum each row, loop through and see if 1 or greater than 1, then positive
  

  # Use the apply function to sum the values in each row
  sums <- apply(summarydata, 1, data)
  
  # Print the resulting vector of row sums
  print(sums)
  
  # Use a for loop to iterate over the vector of row sums
  for (s in sums) {
    # Check whether the sum of the values in the row is 1 or greater than 1
    if (s >= 1) {
      # Print a message if the sum is 1 or greater than 1
      print("Infected")
  #??how to assign this to a variable I can use for the percentage?
    }
  }
 #### 
  # function to summarize a data set in terms of percentage of patients screened that were infected
  summarize_percent_infected <- function(data) {
    # calculate the total number of patients screened
    total_patients <- nrow(data)
    
    # calculate the number of infected patients
    infected_patients <- sum(data$infected)
    
    # calculate the percentage of patients that were infected
    percent_infected <- (infected_patients / total_patients) * 100
    
    # return the percentage of infected patients
    return(percent_infected)
  }
  ####
  
  
  
 #Age distribution histogram of patients (this goes is supportingFunctions.R script)
  #Reading in data
  allData<-read.csv(file = "allData.csv", header = TRUE)
  
  #The data had strangely high ages that were likely errors; this corrects them
  removed_strange_ages<-allData[allData$age <120,]
  
  #finding entries with a marker present
  sick<-subset(removed_strange_ages, marker01!=0 | marker02!=0 |marker03!=0 |marker04!=0 |marker05!=0 |marker06!=0 |marker07!=0 |marker08!=0 | marker09!=0 |marker10!=0)
  
  #visualize age distribution 
  
  # Load the ggplot2 package
  library(ggplot2)
  ggplot()
  
  # Create the histogram using the ggplot function and the geom_histogram function
  ggplot(sick, aes(x = sick$age)) +
    geom_histogram(binwidth = 10, center = 5) +
    xlab("Age Groups") +
    ylab("Number of People Infected") +
    xlim(0,120) +
    scale_x_continuous(breaks =seq(0,120,10)) +
    scale_y_continuous(breaks =seq(0,15000,1250)) 
  
  #analysis.R script
  
  #create source path to supportingFunctions.R script
  source("C:/Users/natal/Desktop/shell-lesson-data/shell-lesson-data/RProject/RProject2022_Submission.R")
  
  #Load functions in supportingFunctions.R file
  source("RProject2022_Submission.R")
  
  #Compile all data into single CSV by calling function
  compiledData()
  
  
  #Process data 
  
  
  
  
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
=======
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

txt2csv <- function(filelist)
for (i in 1:length(filelist)){
  input<-filelist[i]
  output<-gsub("txt","csv",input)
  data = read.table(input, header = TRUE)   
  setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/csvlist_Y/")
  write.csv(data, file=output, row.names=FALSE)
  setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/countryY")
  
}
txt2csv(directory="/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/countryY")

# compile data from all .csv files in a directory into a single .csv file
#same columns, also add "country" and "dayofYear" columns.
#user should be able to remove NA rows, include NA rows but be warned, 
#or include NAs without warning

#Path directory
setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/")

compileData(directory = ,country = X)
compileData(directory=, country=Y)
#Compile
compileData <- function(directory,country,naOption){
  #setting directory variable while testing , delete when functions works
  directory= setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/countryX")
  #??how do I put the country y in the country x file
  country="X"
  csvlist <- list.files(path=directory, pattern = ".csv")
  csvlist
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
    input <- input[-is.na(input),]
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
  write.csv(input, paste0("country",country,"_alldata.csv"), row.names = F)
  
}


# how user uses function
# compiledData <- compileData("path/to/directory", "remove")
  # for second argument, include "remove," "warn," or "include"
    # "remove" for removing rows with NA's
    # "warn" for including NA's but showing a warning message
    # "include" for including NA's without a warning



#Script 2: analyzing script, source("supportingFunctions.R")

  # Write a function to summarize the compiled data set in terms of
  # number of screens run, percent of patients screened that were infected, 
  # male vs. female patients, and the age distribution of patients.


#percentage male, percentage female, percentage positive, percentage negative, age distribution
summarydata<-read.csv("./alldata.csv")
summarizedCompileData(data=summarydata)
summarizedCompileData <- function(data){
#directions
  
  # find the number of rows in the data frame
  nrow(summarydata)
  
  #know number of male and female patients
  sum(summarydata$gender == "male")
  #percentage
  sum(summarydata$gender == "female")
  #percentage
 
   #number of infected patients
      #sum each row, loop through and see if 1 or greater than 1, then positive
  

  # Use the apply function to sum the values in each row
  sums <- apply(summarydata, 1, data)
  
  # Print the resulting vector of row sums
  print(sums)
  
  # Use a for loop to iterate over the vector of row sums
  for (s in sums) {
    # Check whether the sum of the values in the row is 1 or greater than 1
    if (s >= 1) {
      # Print a message if the sum is 1 or greater than 1
      print("Infected")
  #??how to assign this to a variable I can use for the percentage?
    }
  }
 #### 
  # function to summarize a data set in terms of percentage of patients screened that were infected
  summarize_percent_infected <- function(data) {
    # calculate the total number of patients screened
    total_patients <- nrow(data)
    
    # calculate the number of infected patients
    infected_patients <- sum(data$infected)
    
    # calculate the percentage of patients that were infected
    percent_infected <- (infected_patients / total_patients) * 100
    
    # return the percentage of infected patients
    return(percent_infected)
  }
  ####
  
  
#visualize age distribution 
  # Load the ggplot2 package
  library(ggplot2)
  
  # Load the data from the CSV file
  dataforAge <- read.csv("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/countryX/_alldata.csv")
  
  # Create the histogram using the ggplot function and the geom_histogram function
  ggplot(dataforAge, aes(x = input$age)) +
    geom_histogram(binwidth = 1) +
    xlab("Age") +
    ylab("Number of people")
  
  
  
  
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
#---
#my attempts to messwith it
#Script 2: analyzing script, source("supportingFunctions.R")
# Write a function to summarize the compiled data set in terms of
# number of screens run, percent of patients screened that were infected, 
# male vs. female patients, and the age distribution of patients.

#percentage male, percentage female, percentage positive, percentage negative, age distribution
summarydata<-read.csv("./alldata.csv")
removed_strange_ages<-data[data$age <110,]
summarizedCompileData <- function(data){
  cat("Summarized Data")
  
  #number of screens run
  cat("\nNumber of Screens Run:",nrow(data))
  
  #number of infected or healthy patients; removed ages that were likely errors
  removed_strange_ages<-data[data$age <110,]
  sick<-subset(removed_strange_ages, marker01!=0 | marker02!=0 |marker03!=0 |marker04!=0 |marker05!=0 |marker06!=0 |marker07!=0 |marker08!=0 | marker09!=0 |marker10!=0)
  healthy<-subset(removed_strange_ages, marker01!=1 & marker02!=1 & marker03!=1 & marker04!=1 & marker05!=1 & marker06!=1 & marker07!=1 & marker08!=1 & marker09!=1 & marker10!=1)
  cat("\nNumber of Positive Screenings:",nrow(sick))
   
  #percentages
  cat("\n\nPercentage of screenings that were positive:",(nrow(sick)/nrow(removed_strange_ages))*100,"%")
  cat("\nPercentage of positive screens that were male:",((nrow(subset(sick, gender=="male")))/nrow(sick)*100),"%")
  cat("\n\nPercentage of positive screens that were female:",((nrow(subset(sick, gender=="female")))/nrow(sick)*100),"%")
  cat("\nPercentage of screenings that were negative:",(nrow(healthy)/nrow(removed_strange_ages))*100,"%")
 
  #visualize age distribution 
  # Load the ggplot2 package
  library(ggplot2)
  # Create the histogram using the ggplot function and the geom_histogram function
  ggplot(sick, aes(x =age)) +
  geom_histogram(binwidth = 10, center = 5, color="white") +
    xlab("Age Groups") +
    ylab("Number of People Infected") +
    xlim(0,120) +
    ggtitle("Age Distribution by Sex of Patients")+
    facet_grid(.~gender)+
    scale_x_continuous(breaks =seq(0,120,10)) +
    theme_bw()+
    scale_y_continuous(breaks =seq(0,15000,1250))
}
summarizedCompileData(data=summarydata)
  #### 
  # function to summarize a data set in terms of percentage of patients screened that were infected
  summarize_percent_infected <- function(data) {
    # calculate the total number of patients screened
    total_patients <- nrow(data)
    
    # calculate the number of infected patients
    infected_patients <- sum(data$infected)
    
    # calculate the percentage of patients that were infected
    percent_infected <- (infected_patients / total_patients) * 100
    
    # return the percentage of infected patients
    return(percent_infected)
  }
  ####
  
  
  #visualize age distribution 
  # Load the ggplot2 package
  library(ggplot2)
  
  # Load the data from the CSV file
  dataforAge <- read.csv("./alldata.csv")
  
  # Create the histogram using the ggplot function and the geom_histogram function
  ggplot(dataforAge, aes(x = input$age)) +
  geom_histogram(binwidth = 10, center = 5) +
    xlab("Age Groups") +
    ylab("Number of People Infected") +
    xlim(0,120) +
    scale_x_continuous(breaks =seq(0,120,10)) +
    scale_y_continuous(breaks =seq(0,15000,1250))
  
  
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
#------------



#summarize: graph or table distribution (just hit all reqs)
#source("supportingFunctions.R")
>>>>>>> 3c70a618603cc640a499da187dec40d8f77c902f
#will give allthe function I made 