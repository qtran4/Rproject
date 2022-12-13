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