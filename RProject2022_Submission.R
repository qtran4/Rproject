#Questions (can filter out data that doesn't make sense)
#1 In which country (X or Y) did the disease outbreak likely begin?
#2 If Country Y develops a vaccine for the disease, 
#is it likely to work for citizens of Country X?

##Coding
#Script 1: custom functions, "supportingFunctions.R"


#.txt -> .csv substitute
dir.create ("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/csvlist_Y/")

setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/countryY")
#??OR
txt2csv(directory="/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/countryY")

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





#summarize: graph or table distribution (just hit all reqs)
#source("supportingFunctions.R")
#will give allthe function I made 