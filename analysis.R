#this R file will be used to analyze the patient data
# with the help of the functions in SupportingFunctions.R

#setting current working directory; Change for your system
setwd("/Users/kevinbuck/Desktop/Biocomputing/Exercises/Rproject")
source("SupportingFunctions.R")

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

#Tracking Cases Over Time
day.list <- as.character(seq(120,175))
Daily_Total_Cases <- data.frame(Day=as.numeric(day.list),CountryX.Total=numeric(length(day.list)),
                                  CountryY.Total=numeric(length(day.list)))
row.names(Daily_Total_Cases) = day.list
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

#Plotting

library(ggplot2)
ggplot()+geom_line(data=Daily_Total_Cases,mapping=aes(x=Day,y=CountryX.Total,color="CountryX")) +
  geom_line(data=Daily_Total_Cases,mapping=aes(x=Day,y=CountryY.Total,color="CountryY")) +
  ylab("Daily Cases") + xlab("Day Number") + ggtitle("Infections Over Time")+ 
  scale_color_manual(name = "Country", values = c("CountryX" = "blue", "CountryY" = "red")) 

  


plot(Daily_Total_Cases$CountryX.Total~seq(120,175),type="lines",col="blue")
lines(Daily_Total_Cases$CountryY.Total~seq(120,175),add=T,type="lines",col="red")
numeric.day.list <- seq(120,175)
