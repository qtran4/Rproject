#Analysis Script for R Project
#Max Zupfer, Roman Fresquez, Rey Ortiz Bautista
#Introduction to Biocomputing


#Set the working directory on personal computer
setwd("/Users/maxwellzupfer/Desktop/Biocomputing/Rproject")

#Load Plots
library(ggplot2)
library(cowplot)

#Source the supportingFunctions.R script
source('supportingFunctions.R')

#Change all files from .txt to .csv for country, use function from source: supportingFunctions.R
TxtToCSV(directory)
#To run on personal computer:
TxtToCsv("/Users/maxwellzupfer/Desktop/biocomputing/Rproject/countryY/")


#Make the Large CSV for both countries 1 and 2, use function from source: supportingFunctions.R
#This code allows the user to decide what to do with NA's
Combined_CSV(directory1, directory2, directory3)
#To run on personal computer:
Combined_CSV("/Users/maxwellzupfer/Desktop/Biocomputing/Rproject/countryX", "/Users/maxwellzupfer/Desktop/Biocomputing/Rproject/countryY", "/Users/maxwellzupfer/Desktop/Biocomputing/Rproject")
  
#Create a summary of the data from both countries, use function from source: supportingFunctions.R
Data_Summary(file)
#To run on personal computer:
Data_Summary(All_Data_Both)

#Question 1: In which country (X or Y) did the disease outbreak likely begin?
# We determined that the disease originated in country X. 
#This conclusion was drawn from the analysis of the graph created by the code that follows.
#This graph visualizes the cumulative number of infected patients from each nation over time. 
#For the first twenty days of testing, Country Y screenings returned no positive tests. 
#However, Country X had already reported ~4000 cases of the disease by this time. 
#This leads us to believe that the disease outbreak took place in Country X.

#Code to make the graph that demonstrates the answer to question 1
#Make a data frame to add the data to
outbreak<- data.frame(matrix(NA,0,3))
#Make the column names for the data frame
Column_Names=c("dayofyear","Patients","Country")
#Add the column names to the data frame
colnames(outbreak)=Column_Names
#Make a list of all the unique day numbers for country X and Y
daysX<-unique(All_Data1$dayofyear[All_Data1$country=="X"])
daysY<-unique(All_Data1$dayofyear[All_Data1$country=="Y"])
#For loop that Counts the number of patients from country X that were infected with each marker and adds it to the outbreak file
for(i in 1:length(daysX)){
  #Look at each day of the year
  outbreak[i,1]<-daysX[i] 
  #Sub set the country and the day of the year
  countryX<-All_Data1[(All_Data1$dayofyear==daysX[i]&All_Data1$country=="X"),]
  #Sub set the patients
  Patientsx<-countryX[(rowSums(countryX[,3:12])>0),]
  #Number of patients for country X
  outbreak[i,2]<-nrow(Patientsx)
  outbreak[i,3]<-"X"
}


#For loop that counts the number of patients from country that were infected with each marker and adds it to the outbreak file
for(i in 1:length(daysY)){
  #Day of the year for country Y
  outbreak[i+56,1]<-daysY[i]
  #Sub set the country and the day of the year
  countryY<-All_Data1[(All_Data1$dayofyear==daysY[i]&All_Data1$country=="Y"),]
  #Sub set the patients
  PatientsY<-countryY[(rowSums(countryY[,3:12])>0),]
  #Number of patients for country Y
  outbreak[i+56,2]<-nrow(PatientsY)
  outbreak[i+56,3]<-"Y"
}
# generate dataset with day, total number of patients per day, country
outbreak$totalPatients[1:56] <- cumsum(outbreak$Patients[1:56]) # X
outbreak$totalPatients[57:112] <- cumsum(outbreak$Patients[57:112]) #Y

# generating ggplot
ggplot(data = outbreak, aes(x=dayofyear, y=totalPatients, group=Country,color=Country))+
  geom_line(linewidth=0.5)+theme_classic()+ggtitle("Cumulative Infections Over Time")+xlab("Day Number")+ylab("Total Infected Patients")
#Plot is saved under Infections Per Country Graph.pdf


  
#Question 2: If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?
#Use function from source: supportingFunctions.R
# We have reason to believe that if a vaccine was manufactured for Country Y, it would not be effective in country X. 
#This assumption was made following our analysis of the graph created by the function; Vaccine_Question. 
#This graph illustrates how a majority of the infected people in Country X contain markers 1-5 in their samples while most in Country Y contain markers 6-10. 
#This provides evidence that there are different proteins in the disease-causing agents between the two countries. 
#For this reason, the vaccine in country Y would work to provide an immune response to proteins that aren’t as prevalent in the disease-causing agent in Country X.
#Use function: Vaccine Question
Vaccine_Question(file)
#Run Function To Create Graph on personal computer
Vaccine_Question(All_Data_Both)
#The graphical support for this question is VaccineGraph.pdf, which demonstrates markers 01-05 are dominated by country X outbreak, and 06-10 are dominated by the country Y outbreak

  
  
    
