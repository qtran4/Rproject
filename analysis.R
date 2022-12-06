# Introduction to Biocomputing - R Project
# Written by Xiyuan Guan
# 2022-12-1
# analysis
library(ggplot2)

setwd("C:/Users/guanx/Documents/N.ND/BIOS 60318 Biocomputing/test/Rproject/Rproject/")
source("supportingFunctions.R")

# converts all .txt files in "countryY" into .csv format.####
trans.csv("countryY"," ")


# compile data from all .csv files in "countryX" and "countryY"
sumtable1 <- combine.csv("countryX","Warn")
sumtable2 <- combine.csv("countryY","Warn")
# compile all data
allcsv <- rbind(sumtable1,sumtable2)
write.csv(allcsv,"allData_myVersion.csv", row.names = F)

# summarize the compiled data set
file <- read.csv("allData.csv")
sub.pat <- sumdata(file) # this step takes time
# the summary of the age distribution of patients is shown in console. 
write.csv(sub.pat,"subset_of_patients.csv",row.names = F) # for better load

# beginning of answers####

# Q1: In which country (X or Y) did the disease outbreak likely begin?####
# generate dataset with day, number of patients and country
day_num <- data.frame(matrix(NA,0,3))
colnames(day_num) <- c("dayofYear","num_patient","country")
Xdays<- unique(file$dayofYear[which(file$country=="X")]) # days with patients in X country
Ydays <- unique(file$dayofYear[which(file$country=="Y")])
setdiff(Xdays,Ydays) # same
for(i in 1:length(Xdays)){
  day_num[i,1] <- Xdays[i] # dayofYear
  XPat <- file[which(file$dayofYear==Xdays[i]&file$country=="X"),]# subset country and day
  nXPat<- XPat[which(rowSums(XPat[,3:12])>0),] # subset patients
  day_num[i,2] <- nrow(nXPat) # num of patient
  day_num[i,3] <- "X"
}
nrow(day_num) #56
for(i in 1:length(Ydays)){
  day_num[i+56,1] <- Ydays[i] # dayofYear
  YPat <- file[which(file$dayofYear==Ydays[i]&file$country=="Y"),] # subset country and day
  nYPat<- YPat[which(rowSums(YPat[,3:12])>0),] # subset patients
  day_num[i+56,2] <- nrow(nYPat) # num of patient
  day_num[i+56,3] <- "Y"
}

head(day_num)

# sum(day_num[,2]) # test data accuracy = nrow(sub.pat)

# save as plot_1.png
ggplot(data=day_num, aes(x = dayofYear, y = num_patient, group = country, color = country, shape = country))+
  geom_line(linewidth = 0.1)+
  geom_point()+
  theme_bw()
  
# accumulated num of patient could be better
# generate dataset with day, accumulated number of patients, country
day_num$acc_num[1:56] <- cumsum(day_num$num_patient[1:56]) # 
day_num$acc_num[57:112] <- cumsum(day_num$num_patient[57:112]) #Y

# save as plot_2.png
ggplot(data=day_num, aes(x = dayofYear, y = acc_num, group = country, color = country, shape = country))+
  geom_line(linewidth = 0.5)+
  geom_point(size=1)+
  theme_bw()

# A1: According to plot_1 and plot_2, the disease started to break in country X before the 120th day of the year,
# which is the start day of screening. However, the disease started to break in country y on around the 139th day
# of the year, so the disease outbreak likely begin in country X.

# Q2: If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?####

mark_num <- data.frame(matrix(NA,20,3))
colnames(mark_num) <- c("marker","num_of_patient","country")
mark_num[,1] <- rep(colnames(file[,3:12]),2) # 10 markers
mark_num[1:10,3] <- "X"
mark_num[11:20,3] <- "Y"
for(i in 3:12){
  mark_num[i-2,2] <- sum(file[which(file$country=="X"),i])
  mark_num[i+8,2] <- sum(file[which(file$country=="Y"),i])
}

#generate plot_3
ggplot(data = mark_num, aes(x = marker, y = num_of_patient, fill = country))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_y_continuous(expand = c(0,0))+
  xlab("")

# A2: According to plot_3, in country X, marker01-05 are mainly present in patients' sample,
# while in country Y, marker04-10 are mainly present, which indicates different proteins
# shown to form the immunological response by the patients, so indicates differences in the 
# protein in the disease. So the vaccine developed by Country Y may work on the strain that
# has marker04-10 as the response to disease-causing agents, and may not work on the strain 
# in Country X with marker01-05 as the response.