#Elias Issa Rproject

#Set Working Directory
setwd("/Users/elias/Downloads/Rproject")
#Initialize ggplot2
library(ggplot2)

#Converts all .txt files in a directory to csv files
convertDelimiter<- function(directory){
    #Object with all .txt file names in given directory
    filedir <- dir(path = directory, pattern = ".txt")
    #Loops through all .txt files, reads the content to data and writes a .csv file with the same name, then removes the .txt file
    for(i in filedir){
      data <- read.table(paste(directory,i, sep ="/" ), header = T, sep = "")
      write.csv(data, file = paste(directory, sub(".txt",".csv", i), sep ="/"), quote=F, row.names=F, sep = ",")
      file.remove(paste(directory,i, sep ="/" ))
      }
    }
#Compiles all .csv files in a single directory
#The user has 3 options in NA choice, remove rows with NAs through removeNA, include NAs but be warned of their presence through warnNA, or include NAs without warning through ignoreNA 
compileCSV<- function(directory, choice){
  #Deletes the output file in the event that it already exists
  file.remove(paste(directory, "allData.csv", sep = "/"))
  #Used to define the first addition to a file which will be later changed to FALSE
  first = TRUE
  #Object with all .csv files in the directory
  filedir <- dir(path = directory, pattern = ".csv")
  if(choice == "warnNA"){ print("Warning this summary data may contain NAs")}
  #Loops through all .csv files in the directory
  for(i in filedir){
    #Stores the data in .csv files as temporary value
    data2 <- read.csv(paste(directory,i, sep = "/"))
    #Remove NAs if choice is removeNA
    if(choice=="removeNA"){ data <- na.omit(data2)}
  
    #Coverts directory name(ex. countryX) to indenity (ex. X)
    country <- gsub("country","", directory)
    #Replicates country identity for the number of rows in the directory 
    country <- rep(country, nrow(data2))
    #Adds a column with country identity to data
    data2 <- cbind(data2, country)
    #Removes all non-numerical characters from file name
    dayofYear <- gsub("[^0-9]", "", i)
    #Replicates date for the number of rows in the directory
    dayofYear <- rep(dayofYear, nrow(data2))
    #Adds a column with date to data
    data2 <- cbind(data2, dayofYear)
    #If this isn't the first entry then column names will be excluded
    if(first==FALSE){
      write.table(data2, file = paste(directory,"allData.csv", sep ="/"), append =T, row.names = F, col.names = F, sep = ",")
    }
    #If this is the first entry then column names will be included
    else{
      first = FALSE
      write.table(data2, file = paste(directory,"allData.csv", sep ="/"), append = F, row.names=F, col.names = T, sep = ",")
      }
  }
}

#This function summarizes the data of a file
#Provides total number of screens as well as the percent of patients infected
#As well as the number of patients infected by sex and percent of patients infected by sex
#In addition a graph of the patient age distribution
summarizeData<- function(file){
    data <- read.csv(file)
    #Removes unrealistic data of patients older than 122, as some patients were reported to be over 400 years old
    all <- subset(data, age<122)
    #Total number of rows is equal to the number of screens
    numscreen <- nrow(all)
    #Subsets the data for patients with a single marker present in any of the markers
    infected <- subset(all, marker01 > 0 | marker02 > 0 | marker03 > 0 | marker04 > 0 | marker05 > 0 | marker06 > 0 | marker07 > 0 | marker08 > 0 | marker09 > 0 | marker10 > 0)   
    #Total number of rows is equal to the number of infected
    numinfected <- nrow(infected)
    
    #Number screened by gender
    nummalescreen <-nrow(subset(all, gender == "male"))
    numfemalescreen <-nrow(subset(all, gender == "female"))
    
    #Number of  infected by gender
    nummaleinfected <- nrow(subset(infected, gender == "male"))
    numfemaleinfected <- nrow(subset(infected, gender == "female"))
    
    #Percent infected
    percentinfected = numinfected/numscreen
    malepercentinfected = nummaleinfected/nummalescreen
    femalepercentinfected = numfemaleinfected/numfemalescreen

    #Print functions
    print(paste("Percent of patients screened", numscreen, "with an infection rate of", percentinfected))
    print(paste("Percent of male patients screened", nummalescreen, "with an infection rate of", malepercentinfected))
    print(paste("Percent of female patients screened", numfemalescreen, "with an infection rate of", femalepercentinfected))
    
    #Histogram of infected patients by age
    ggplot() + geom_histogram(data=infected, aes(x=age,color="red"), binwidth = 1) +theme_classic()

}

#Provides the number of markers present by marker for a given country from summary data
markerDistribution <- function(file){
  data <- read.csv(file)
  all <- subset(data, age<122)
  #Creates a dataframe with the markers 1,10 and corresponding total marker count
  df <- data.frame(marker = c(seq.int(1,10,by =1)),
                   count = c(sum(all$marker01),sum(all$marker02),sum(all$marker03),sum(all$marker04),sum(all$marker05),sum(all$marker06),sum(all$marker07),sum(all$marker08),sum(all$marker09),sum(all$marker10)))
  #bar chart of marker distribution
   ggplot(data = df, aes(x=marker, y = count)) + geom_bar(stat= "identity")+theme_classic()
}

#Determines the date of first infection for a given country from summary data
firstInfection <- function(file){
  data <- read.csv(file)
  #Subsets the data for patients with a single marker present in any of the markers
  infected <- subset(data, marker01 > 0 | marker02 > 0 | marker03 > 0 | marker04 > 0 | marker05 > 0 | marker06 > 0 | marker07 > 0 | marker08 > 0 | marker09 > 0 | marker10 > 0)   
 #prints the minimum value of day of year to represent the first infection
   print(min(infected$dayofYear))
}
  

