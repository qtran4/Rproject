
#Set Working Directory
setwd("/Users/elias/Downloads/Rproject")
#Initialize ggplot2
library(ggplot2)

#Accesses supporting functions
source("supportingFunctions.R")

#covnerts all .txt files into comma delimited .csv files for countryY
convertDelimiter("countryY")


#Compiles the CSV data for each country into a file allData.csv for future analysis with NAs removed
compileCSV("countryY", "removeNA")
compileCSV("countryX", "removeNA")

#In which country (X or Y) did the disease outbreak likely begin?
#The first infection for country Y is 139
firstInfection("countryY/allData.csv")
#The first infection for country X is 120
firstInfection("countryX/allData.csv")
#As Country X had infections at day 120, 19 days prior to country Y, the infection likely began in country X

#If Country Y develops a vaccine for the disease, it it likely to work for Country X?
#Country Y has high levels of markers 6 & 7, with slightly lower levels of markers 7-10
#and moderate levels of markers 4&5 with nearly no levels of markers 1-3
markerDistribution("countryY/allData.csv")
#Country X has extremely high levels of markers 1-5
markerDistribution("countryX/allData.csv")

#As country Y is developed the vaccine they would likely develop it for markers 5-10
#These markers are present at low levels in country X
#As such the country Y vaccine would likely not work for country X

#Summarizes data for both countries form the file allData.csv
summarizeData("allData.csv")

#The disease appears to infect both sexes fairly equally and appears to mostly infect younger patients under 20 years old
