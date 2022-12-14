
getwd()
source("supportingFunction.R")
setwd("/Users/pujasharma/Documents/Phd_data/Rproject")


# converts all .txt files in "countryY" into .csv format.####
txtTOcsv("countryY", "")


# compile data from all .csv files in "countryX" and "countryY"
countryXSum <- compile_csv("countryX","Warn")
countryYSum <- compile_csv("countryY","Warn")
# compile all data
finalcompiledCSV <- rbind(countryXSum,countryYSum)
write.csv(finalcompiledCSV,"compiled_sharma_version.csv", row.names = F)

# summarize the compiled data set
compileFile <- read.csv("compiled_sharma_version.csv")
subPatient <- sumdata(compileFile) # sum and subset 
# the summary of the age distribution of patients is shown in console. 
write.csv(subPatient,"subPatient.csv",row.names = F) 
# it takes time to load those data, so to make things easy we write the csv file
# and save it on directory


dnp <- data.frame(matrix(NA,0,3))
colnames(dnp) <- c("dayofYear","patientNumber","country")
countryXdays<- unique(compileFile$dayofYear[which(compileFile$country=="X")]) # days with patients in X country
countryYdays <- unique(compileFile$dayofYear[which(compileFile$country=="Y")])
setdiff(countryXdays,countryYdays) 
# subset country and day once and patients another time
for(i in 1:length(countryXdays)){
  dnp[i,1] <- countryXdays[i] 
  countryXPatient <- compileFile[which(compileFile$dayofYear==countryXdays[i]&compileFile$country=="X"),]
  numcountryXPatient<- countryXPatient[which(rowSums(countryXPatient[,3:12])>0),] 
  dnp[i,2] <- nrow(numcountryXPatient)
  dnp[i,3] <- "X"
}

nrow(dnp) 
#subset country and day once and patients another time
for(i in 1:length(countryYdays)){
  dnp[i+56,1] <- countryYdays[i] 
  countryYPatient <- compileFile[which(compileFile$dayofYear==countryYdays[i]&compileFile$country=="Y"),] 
  numcountryYPatient<- countryYPatient[which(rowSums(countryYPatient[,3:12])>0),] 
  dnp[i+56,2] <- nrow(numcountryYPatient) 
  dnp[i+56,3] <- "Y"
}


#First plot: Plot1
library(ggplot2)
Plot1 <- ggplot(data=dnp, aes(x = dayofYear, y = patientNumber, group = country, color = country, shape = country))+
  geom_line(linewidth = 0.1)+
  xlab("Day of the year")+
  ylab("No. of patient")+
  geom_point()+
 theme_light()
Plot1
ggsave("Plot1.pdf")

# #dnp include data set with day, number of patient (cumulative sum with column 
# name totalPatient) and country.
#added with cumsum function
dnp$totalPatient[1:56] <- cumsum(dnp$patientNumber[1:56]) 
dnp$totalPatient[57:112] <- cumsum(dnp$patientNumber[57:112]) 

# Second Plot: Plot2
Plot2 <- ggplot(data=dnp, aes(x = dayofYear, y = totalPatient, group = country, color = country, shape = country))+
  geom_line(size=1)+
  geom_point(size=2)+
  theme_light()+
  xlab("Day of Year")+
  ylab("Total number of patient")
Plot2
ggsave("Plot2.pdf")
# 1. In which country (X or Y) did the disease outbreak likely begin?
#ANSWER: 
# Based on above plots, the diseases has started earlier in the country X (approx. 
# from the 122 days), whereas the diseases has diagnosed a bit later on country Y
# (approx. after 138 days). Thus, the disease outbrerak likely to begin in country X.


Markers <- data.frame(matrix(NA,20,3))
colnames(Markers) <- c("marker","no.ofPatient","country")
Markers[,1] <- rep(colnames(compileFile[,3:12]),2) 
Markers[1:10,3] <- "countryX"
Markers[11:20,3] <- "countryY"
for(i in 3:12){
  Markers[i-2,2] <- sum(compileFile[which(compileFile$country=="X"),i])
  Markers[i+8,2] <- sum(compileFile[which(compileFile$country=="Y"),i])
}

#Third plot: Plot3 
Plot3 <- ggplot(data = Markers, aes(x = marker, y = no.ofPatient, fill = country))+
  geom_bar(stat = "identity", position = "stack")+
  ylab("Number of Patients")+
  xlab("Markers")+
  scale_y_continuous(expand = c(0,0))
Plot3
ggsave("Plot3.pdf")


# 2. If Country Y develops a vaccine for the disease, is it likely to work for
# citizens of Country X?
# ANSWER: From the above graph, we can see that marker 1-marker4 are mostly present
# in country X, whereas marker 6 to 10 are present in abundance in country Y. Not same
# vaccine will work for both countries if we need to eradicated the disease completely.
# If both countries had same marker, a vaccine develoved by country Y could have eradicated
# the disease even in country X.



