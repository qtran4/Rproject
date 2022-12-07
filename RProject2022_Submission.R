#Questions (can filter out data that doesn't make sense)
#1 In which country (X or Y) did the disease outbreak likely begin?
#2 If Country Y develops a vaccine for the disease, 
#is it likely to work for citizens of Country X?

##Coding
#Script 1: custom functions, "supportingFunctions.R"


#.txt -> .csv substitute
dir.create ("/Users/avivalund/Desktop/Biocomputing/Final Project/RProject/done/")
setwd("/Users/avivalund/Desktop/Biocomputing/Final Project/RProject/countryY")
filelist = list.files(pattern = ".txt")
for (i in 1:length(filelist)){
  input<-filelist[i]
  output<-gsub("txt","csv",input)
  data = read.delim(input, header = TRUE)   
  setwd("/Users/avivalund/Desktop/Biocomputing/Final Project/RProject/done/")
  write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
  setwd("/Users/avivalund/Desktop/Biocomputing/Final Project/RProject/countryY")
  
}
#compile once or twice
#country x: for loop!
x-> <matrix of 1-8>
y-> <another matrix of 10-17>
z->rbind(x,y)
z-> <1 through 17
# compile data from all .csv files in a directory into a single .csv file
#same columns, also add "country" and "dayofYear" columns.
#user should be able to remove NA rows, include NA rows but be warned, 
#or include NAs without warning

$# countryX for loop 
csvlistX <- list.files(pattern = ".csv")
(for i in 1:length(csvlistX)){
  rbind()
}




<-rbind()
countryY <-
#for loop
compiledcsvfiles <- rbind(countryX, countryY)
          lapply(list.files(path = "/Users/avivalund/Desktop/Biocomputing/Final 
                            Project/RProject/done/"), read.csv))



#Script 2: analyzing script, source("supportingFunctions.R")






#summarize: graph or table distribution (just hit all reqs)
#source("supportingFunctions.R")
#will give allthe function I made 