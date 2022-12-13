#load packages and custom functions
library(ggplot2)
library(tidyverse)
source("supportingFunctions.R")

#set work directory
#please set your own work directory before use
setwd("D:/Notre Dame/Class/2022 Fall/Introduction to Biocomputing/Tutorial/Rproject-main/Rproject-main")

#convert txt files into csv files
txt_convert("./countryY")

#compile all data
all_data <- compile(c("X","Y"), NA_remove = FALSE, NA_warning = FALSE)
write.csv(all_data, file = "allData.csv", row.names=F)

#answer the questions
#1.In which country (X or Y) did the disease outbreak likely begin?
#The function "find_first_positive" is defined in supportingFunctions.R
print(find_first_positive("X"))
print(find_first_positive("Y"))
#Answer: From the outputs above, we know that the first patient in country X was found earlier than that of country Y, so the disease began in country X.
#We can also get this conclusion through a graph.
#load the data in allData.csv
allData <- read.csv('allData.csv')
allData$infectedSums <- rowSums(allData[,3:12] )
allData$infected <- ifelse(allData$infectedSums>0, 1, 0)
infectedData <- allData[allData$infected==1,]
ggplot(infectedData, aes(x=dayofYear, fill=country))+geom_bar(position = 'dodge')

#2.If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?
#generate plots
allData_long <- allData %>% pivot_longer(cols = marker01:marker10, names_to="marker",values_to="value")
allData_long <- allData_long[allData_long$value==1,]
ggplot(allData_long, aes(x=marker, fill=country))+geom_bar(position = 'dodge')
#Answer: Markers detected in country A and country B are not highly consistent, so vaccine for country Y is not likely to work in country X.

#summarize all data
#The function "summarize" is defined in supportingFunctions.R
summarize(allData)
