#github was very uncooperative, we all did pretty equal work though 
#1)our figures provide good evidence based on the cases per day the the disease originated in country X. 
#2)We also determined that a vaccine developed in country Y would have limited uses for country X. 
#The disparity in markers present between the two countries means that the little overlap that doesoccur would not confer good immunity to country X people. 

#Analysis R script---------------------

#create source path to RProject2022_Submission.R script
setwd(C:/Users/natal/Desktop/shell-lesson-data/shell-lesson-data/Rproject)
source("C:/Users/natal/Desktop/shell-lesson-data/shell-lesson-data/Rproject/RProject2022_Submission.R")

#Load functions in RProject2022_Submission.R file
source(RProject2022_Submission.R)
#-------------------------
#Compile all data into single CSV by calling function
# compile data from all .csv files in a directory into a single .csv file
#same columns, also add "country" and "dayofYear" columns.
#user should be able to remove NA rows, include NA rows but be warned, 
#or include NAs without warning

#Path directory
setwd("/Users/avivalund/Desktop/Biocomputing/FinalProject/RProject/")

#Compile; couldnt get source to work so here is the code that would compile the csvs
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
#------------------------------------------------------------------------------------------------------------------
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



#Graphing------------------------------
#Reading in data
allData<-read.csv(file = "allData.csv", header = TRUE)
library(ggplot2)
ggplot()

#The data had strangely high ages that were likely errors; this corrects them
removed_strange_ages<-allData[allData$age <110,]

#finding entries with a marker present; removes any entries that weren't infected
sick<-subset(removed_strange_ages, marker01!=0 | marker02!=0 |marker03!=0 |marker04!=0 |marker05!=0 |marker06!=0 |marker07!=0 |marker08!=0 | marker09!=0 |marker10!=0)

#ggplotting cases as a function of time
#sub-setting and adjusting data
Xdata<- sick[sick$country=="X",]
Ydata<-sick[sick$country=="Y",]

Yno<-seq_along(Ydata$dayofYear)
Xno<-seq_along(Xdata$country)
sick$Casenum<-append(Xno,Yno, after = length(Xno))

#Plot Cumulative cases in each country against time
ggplot(data = sick,
       aes(x=sick$dayofYear, y=sick$Casenum, colour = country))+
  geom_smooth()+
  xlab("Day of the Year") +
  ylab("Running Case Total")+
  theme_classic()

#Plot new cases against time
ggplot(data = sick,
       aes(x=dayofYear))+
  geom_bar(position="dodge", aes( fill = country), show.legend = FALSE)+
  xlab("Day of the Year") +
  ylab("New Cases Per Day")+
  facet_grid(.~country)+
  theme_classic()

#Marker comparison by country
#subset data
XmarkerData<-data.frame( MarkerNum = colnames(Xdata)[3:12],
                         MarkerSum = colSums(Xdata[,3:12]),
                         Country = Xdata[1:10,13])
rownames(XmarkerData)<-1:nrow(XmarkerData)
YmarkerData<-data.frame( MarkerNum = colnames(Ydata)[3:12],
                         MarkerSum = colSums(Ydata[,3:12]),
                         Country = Ydata[1:10,13])
rownames(YmarkerData)<-1:nrow(YmarkerData)
MarkerforPlot<- rbind(XmarkerData,YmarkerData)
#plot
ggplot(data = MarkerforPlot,
       aes(x=Country, y=MarkerSum, fill=MarkerNum))+
  geom_bar(stat="identity", position="dodge")+
  xlab("Country") +
  ylab("Marker Abundance")+
  theme_minimal()