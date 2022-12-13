#this R file will be used to analyze the patient data
# with the help of the functions in SupportingFunctions.R

#setting current working directory; Change for your system
setwd("/Users/kevinbuck/Desktop/Biocomputing/Exercises/Rproject")
source("SupportingFunctions.R")
####Initial Data Setup ####
#turning all the txt files for Country Y into CSV files
for (i in 120:175){
  makeCSV(filename = paste("countryY/screen_",i,".txt",sep=""),delimeter = " ",
          fileOutname = paste("countryY/screen_",i,sep=""))
}

#making giant data frame of screening data from both countries
countryX.df <- combineCSVs(directoryname = "countryX/", country="X",na_handling = "remove",
                           FileOutName = "CountryX")
countryY.df <- combineCSVs(directoryname = "countryY/", country="Y",na_handling = "remove",
                           FileOutName = "CountryY")

allData.df <- rbind(countryX.df,countryY.df)

#summarize data 
SummarizeCompiledTests(allData.df)


#making function to check if the screened patient is infected
check_infected <- function(row_index,df){
  return(sum(as.numeric(df[row_index,3:12] == "1")) != 0)
}

####Tracking Cases Over Time####

day.list <- as.character(seq(120,175))

#initiating data frame to store info
Daily_Total_Cases <- data.frame(Day=as.numeric(day.list),CountryX.Total=numeric(length(day.list)),
                                  CountryY.Total=numeric(length(day.list)))
row.names(Daily_Total_Cases) = day.list
#this big loop checks if a screened row has any markers present (is patient infected?)
# and then it will add 1 to the total for cases in the appropriate day as stored in the dataframe
for(day_num in day.list){
  tempDF <- allData.df[allData.df[,"dayofYear"] == day_num,]
  for(i in 1:nrow(tempDF)){
    if( (check_infected(i,tempDF) == TRUE) & (tempDF[i,'country'] == "X")){
      Daily_Total_Cases[day_num,"CountryX.Total"] = 1 + Daily_Total_Cases[day_num,"CountryX.Total"]
    }
    if((check_infected(i,tempDF) == TRUE) & (tempDF[i,'country'] == "Y")){
      Daily_Total_Cases[day_num,"CountryY.Total"] = 1 + Daily_Total_Cases[day_num,"CountryY.Total"]
    }
  }
}

#####Plotting Daily Cases by Day#####

#Notice how Country X has cases starting at day 120, and the country Y
# cases line is at 0 cases until day 138ish. This means the outbreak began in country X

library(ggplot2)
ggplot()+geom_line(data=Daily_Total_Cases,mapping=aes(x=Day,y=CountryX.Total,color="CountryX")) +
  geom_line(data=Daily_Total_Cases,mapping=aes(x=Day,y=CountryY.Total,color="CountryY")) +
  ylab("Daily Cases") + xlab("Day Number") + ggtitle("Infections Over Time")+ 
  scale_color_manual(name = "Country", values = c("CountryX" = "blue", "CountryY" = "red")) 


#Figuring out which markers cause the disease
total.cases.X <- 0
for (i in 1:nrow(countryX.df)){
  if(check_infected(i,countryX.df) == T){ 
    total.cases.X <- total.cases.X +1
    }
}
total.cases.Y <- 0
for (i in 1:nrow(countryY.df)){
  if(check_infected(i,countryY.df) == T){ 
    total.cases.Y <- total.cases.Y +1
  }
}
x.markers.pct <- numeric(10)
y.markers.pct <- numeric(10)
x.markers.pct[1] <- sum(countryX.df[,"marker01"] == "1") / total.cases.X
y.markers.pct[1] <- sum(countryY.df[,"marker01"] == "1") / total.cases.Y
x.markers.pct[2] <- sum(countryX.df[,"marker02"] == "1") / total.cases.X
y.markers.pct[2] <- sum(countryY.df[,"marker02"] == "1") / total.cases.Y
x.markers.pct[3] <- sum(countryX.df[,"marker03"] == "1") / total.cases.X
y.markers.pct[3] <- sum(countryY.df[,"marker03"] == "1") / total.cases.Y
x.markers.pct[4] <- sum(countryX.df[,"marker04"] == "1") / total.cases.X
y.markers.pct[4] <- sum(countryY.df[,"marker04"] == "1") / total.cases.Y
x.markers.pct[5] <- sum(countryX.df[,"marker05"] == "1") / total.cases.X
y.markers.pct[5] <- sum(countryY.df[,"marker05"] == "1") / total.cases.Y
x.markers.pct[6] <- sum(countryX.df[,"marker06"] == "1") / total.cases.X
y.markers.pct[6] <- sum(countryY.df[,"marker06"] == "1") / total.cases.Y
x.markers.pct[7] <- sum(countryX.df[,"marker07"] == "1") / total.cases.X
y.markers.pct[7] <- sum(countryY.df[,"marker07"] == "1") / total.cases.Y
x.markers.pct[8] <- sum(countryX.df[,"marker08"] == "1") / total.cases.X
y.markers.pct[8] <- sum(countryY.df[,"marker08"] == "1") / total.cases.Y
x.markers.pct[9] <- sum(countryX.df[,"marker09"] == "1") / total.cases.X
y.markers.pct[9] <- sum(countryY.df[,"marker09"] == "1") / total.cases.Y
x.markers.pct[10] <- sum(countryX.df[,"marker10"] == "1") / total.cases.X
y.markers.pct[10] <- sum(countryY.df[,"marker10"] == "1") / total.cases.Y
markers.list <- as.character(seq(1,10))
markers.sum.table <- matrix(ncol = 3,nrow=20)
colnames(markers.sum.table) <- c("country","marker","freq")
markers.sum.table <- as.data.frame(markers.sum.table)
for (i in 1:10){
  tempvect01 <- c("X",as.character(i),x.markers.pct[i])
  markers.sum.table[i,] <- tempvect01
  tempvect02 <- c("Y",as.character(i),y.markers.pct[i])
  markers.sum.table[10+i,] <- tempvect02
}
#plotting the markers that cause disease
# Note how the distribution of markers for country X is skewed towards Markers 1-5,
# Whereas Country Y has a relatively even distribution. This means that a vaccine targeted
# At patients in country Y might be based upon solving a problem caused by a micro satellite the patient in 
# Country X might not have present. Thus, vaccines developed by country Y MIGHT work for 
# Country X, but it is not certain.
ggplot(data=markers.sum.table,aes(x=marker,y=freq,fill=country)) + 
  geom_bar(stat="identity",position = "dodge",alpha=.75) + 
  ggtitle("Relative Frequency of MicroSatellites")
  



