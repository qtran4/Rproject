####R project
####Analysis
####Shannon Culbert and Camden Arnold
####All code assumes working directory is set to RProject directory

################################################################################

source("supportingFunctions.R")

##convert all .txt files to .csv files
convert_to_csv("countryY")

##combine all .csv files from both countries into one large .csv file
combine_data("countryX","countryY")

##summarize important data
summary_data("allData.csv")

################################################################################

###Question 1: In which country did the disease outbreak likely begin?
###Answer: The outbreak likely began in country X because infected 
###individuals for Country X were detected on day 120, whereas the 
###first infected individuals for Country Y were detected on day 139

##This reasoning can best be understood by viewing a line graph
##that compares the day of infection to the number of infected individuals

Day.v.Infections <- function(file){
  allData <- read.csv("allData.csv")
  #create separate Country X and Y dataframes to hold information
  XDayInfect.df <- data.frame(matrix(NA,0,3))
  YDayInfect.df <- data.frame(matrix(NA,0,3))
  colnames(XDayInfect.df) <- c("country","dayofYear", "numberofInfections")
  colnames(YDayInfect.df) <- c("country","dayofYear", "numberofInfections")
  
  XDayofYear <- unique(allData$dayofYear[allData$country=="X"])
  YDayofYear <- unique(allData$dayofYear[allData$country=="Y"])
  
  #use a ForLoop to determine Country X's infected individuals at each dayofYear 
  for(i in 1:length(XDayofYear)){
    XDayInfect.df[i,1] <- "X"
    XDayInfect.df[i,2] <- XDayofYear[i]
    XDay <- allData[allData$dayofYear==XDayofYear[i],]
    XPatients <- XDay[XDay$country=="X",]
    XPat.num <- XPatients[(rowSums(XPatients[,3:12])>0),]
    XDayInfect.df[i,3] <- nrow(XPat.num)
  }
  
  #use a ForLoop to determine Country Y's infected individuals at each dayofYear
  for(i in 1:length(YDayofYear)){
    YDayInfect.df[i,1] <- "Y"
    YDayInfect.df[i,2] <- YDayofYear[i]
    YDay <- allData[allData$dayofYear==YDayofYear[i],]
    YPatients <- YDay[YDay$country=="Y",]
    YPat.num <- YPatients[(rowSums(YPatients[,3:12])>0),]
    YDayInfect.df[i,3] <- nrow(YPat.num)
  }
  
  #combine Country X and Country Y dataframes together
  allInfections <- rbind(XDayInfect.df, YDayInfect.df)
  
  #create the line graph to compare the number of infections on each day for each country 
  library(ggplot2)
  ggplot(data = allInfections, aes(x = dayofYear, y = numberofInfections, group = country, color = country))+
    geom_line() +
    geom_point() +
    theme_bw() +
    xlab("Day of the Year") +
    ylab("Number of Infections")
  #CountryX infections are labeled in red
  #CountryY infections are labeled in blue
}

Day.v.Infections("allData.csv")
#Since CountryX had an earlier exposure to disease and a greater number of infections,
#we can infer that the disease outbreak most likely began in CountryX 

################################################################################

###Question 2: If Country Y develops a vaccine for the disease, 
###is it likely to work for citizens in Country X?
###Answer: No, because the disease that affects individuals in Country Y 
###have different markers. If a vaccine is developed in Country Y, then the vaccine will
###target the markers that are representative of Country Y (such as markers #05-10). 
###Since Country Y's vaccine targets markers 05-10, which are not readily present 
###in the disease that affects individuals in Country X, 
###the vaccine will not work for the citizens of Country X. 

##This reasoning can best be understood by viewing a barplot
##that compares the markers that are most abundant in each country

Marker.Bar <- function(file){
  allData<-read.csv("allData.csv")
  XallData <- allData[allData$country=="X",]
  YallData <- allData[allData$country=="Y",]
  
  #create a count for each marker
  Xmarker01_count<-0
  Xmarker02_count<-0
  Xmarker03_count<-0
  Xmarker04_count<-0
  Xmarker05_count<-0
  Xmarker06_count<-0
  Xmarker07_count<-0
  Xmarker08_count<-0
  Xmarker09_count<-0
  Xmarker10_count<-0
  
  Ymarker01_count<-0
  Ymarker02_count<-0
  Ymarker03_count<-0
  Ymarker04_count<-0
  Ymarker05_count<-0
  Ymarker06_count<-0
  Ymarker07_count<-0
  Ymarker08_count<-0
  Ymarker09_count<-0
  Ymarker10_count<-0
  
  #Use a ForLoop with If/Else statements to count each marker
  for (row in 1:nrow(XallData)){
    if (XallData$marker01[row] == 1){
      Xmarker01_count<-Xmarker01_count + 1 
    }
    if (XallData$marker02[row] == 1){
      Xmarker02_count<-Xmarker02_count + 1 
    }
    if (XallData$marker03[row] == 1){
      Xmarker03_count<-Xmarker03_count + 1 
    }
    if (XallData$marker04[row] == 1){
      Xmarker04_count<-Xmarker04_count + 1 
    }
    if (XallData$marker05[row] == 1){
      Xmarker05_count<-Xmarker05_count + 1 
    }
    if (XallData$marker06[row] == 1){
      Xmarker06_count<-Xmarker06_count + 1 
    }
    if (XallData$marker07[row] == 1){
      Xmarker07_count<-Xmarker07_count + 1
    }
    if (XallData$marker08[row] == 1){
      Xmarker08_count<-Xmarker08_count + 1 
    }
    if (XallData$marker09[row] == 1){
      Xmarker09_count<-Xmarker09_count + 1 
    }
    if (XallData$marker10[row] == 1){
      Xmarker10_count<-Xmarker10_count + 1 
    }}
  
  for (row in 1:nrow(YallData)){
    if (YallData$marker01[row] == 1){
      Ymarker01_count<-Ymarker01_count + 1 
    }
    if (YallData$marker02[row] == 1){
      Ymarker02_count<-Ymarker02_count + 1 
    }
    if (YallData$marker03[row] == 1){
      Ymarker03_count<-Ymarker03_count + 1 
    }
    if (YallData$marker04[row] == 1){
      Ymarker04_count<-Ymarker04_count + 1 
    }
    if (YallData$marker05[row] == 1){
      Ymarker05_count<-Ymarker05_count + 1 
    }
    if (YallData$marker06[row] == 1){
      Ymarker06_count<-Ymarker06_count + 1 
    }
    if (YallData$marker07[row] == 1){
      Ymarker07_count<-Ymarker07_count + 1 
    }
    if (YallData$marker08[row] == 1){
      Ymarker08_count<-Ymarker08_count + 1 
    }
    if (YallData$marker09[row] == 1){
      Ymarker09_count<-Ymarker09_count + 1
    }
    if (YallData$marker10[row] == 1){
      Ymarker10_count<-Ymarker10_count + 1 
    }}
  
  #create dataframes for marker information
  Xmarker.df <- data.frame(matrix(NA,10,3))
  colnames(Xmarker.df) <- c("country", "marker","occurrence")
  Xmarker.df[,1] <- "X"
  Xmarker.df[,2] <- c("marker01","marker02","marker03","marker04","marker05","marker06","marker07","marker08","marker09","marker10")
  Xmarker.df[,3] <- c(Xmarker01_count,Xmarker02_count,Xmarker03_count,Xmarker04_count,Xmarker05_count,Xmarker06_count,Xmarker07_count,Xmarker08_count,Xmarker09_count,Xmarker10_count)
  
  Ymarker.df <- data.frame(matrix(NA,10,3))
  colnames(Ymarker.df) <- c("country", "marker","occurrence")
  Ymarker.df[,1] <- "Y"
  Ymarker.df[,2] <- c("marker01","marker02","marker03","marker04","marker05","marker06","marker07","marker08","marker09","marker10")
  Ymarker.df[,3] <- c(Ymarker01_count,Ymarker02_count,Ymarker03_count,Ymarker04_count,Ymarker05_count,Ymarker06_count,Ymarker07_count,Ymarker08_count,Ymarker09_count,Ymarker10_count)
  
  #combine X Y marker dataframes
  allMarkers <- rbind(Xmarker.df, Ymarker.df)
  
  #create the bargraph to compare marker occurrence for each country
  library(ggplot2)
  
  ggplot(allMarkers, aes(x = marker, y = occurrence, fill = country)) +
    geom_bar(stat="identity", position = position_dodge(), alpha = 0.75) +
    theme_classic() +
    xlab("Marker Number") +
    ylab("Occurrence") +
    theme(axis.text.x = element_text(angle=65, vjust=0.6))
  #CountryX markers are labeled in red
  #CountryY markers are labeled in blue
}

Marker.Bar("allData.csv")
#Since each country has different representative markers 
#(CountryX is predominantly markers 01-05, and 
#CountryY is predominantly markers 06-10), a vaccine developed
#in CountryY would not likely work for CountryX because each country 
#will have different immunological responses.
#If a vaccine is developed in CountryY, it will most likely target markers 06-10,
#and very little people in CountryX have markers 06-10.