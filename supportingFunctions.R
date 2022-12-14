##R Final Project
# Written by Isaiah Murrell-Thomas and Bethany Oceguera 

## IMPORTANT INFORMATION
# Before running this code, please make sure that you are in the directory containing all relevant files 

##FUNCTION 1
# Function that converts all files in a directory with space- or 
#   tab-delimited data (.txt) into comma-separated value files.

csvConvert <- function(dir){
  setwd(dir)
  filesToConvert <- list.files(pattern = ".txt") 
  for (i in 1:length(filesToConvert)){
    input <- filesToConvert[i]  
    output <- paste0(gsub("\\.txt$", "", input), ".csv")
    data <- read.table(input, sep="",header=TRUE)
    write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
    file.remove(input)  
  }
  print("Files have been converted to .csv")
}

#USAGE 
# Define and run variable "dir" as the directory path that holds the files to be converted
# Run "csvConvert(dir)"

#EXAMPLE: convert and remove .txt files in "countryY" directory
# dir <- "/Users/bethoceguera/Documents/PhD Year 1/First Semester/Introduction to Biocomputing/Rproject/countryY" 
# csvConvert(dir)


##FUNCTION 2
# Function that compiles data from all .csv files in a directory 
# into a single .csv file. The compiled data has the original twelve columns 
# from daily data sheets, but also country and dayofYear columns. 
# The user should be able to choose whether they want to remove rows with NA’s 
# in any columns, include NAs in the compiled data but be warned of their presence, or include NAs in the compiled data without a warning

csvMerge<-function(dir,naChoice){
  myPath <- list.files(path = dir, recursive = TRUE, pattern = "*.csv", full.names = TRUE)
  dirName <-dirname(path= myPath)
  dayofYear <-'dayofYear'
  country<-'country'
  header<-read.csv(myPath[1],header = TRUE, stringsAsFactors = FALSE) 
  for (i in seq_along(myPath)){
    if (naChoice=="1"){
      data<-read.csv(myPath[i],header = TRUE, stringsAsFactors = FALSE)
      noNAs <-na.omit(data)
      noNAs[,dayofYear]<- gsub(".+([0-9]{3}).*", "\\1", myPath[i])
      noNAs[,country]<-gsub(".+([A-Z]{1}).*", "\\1", dirName[i])
      header<-rbindlist(list(header,noNAs),fill = TRUE)  
    }
    if (naChoice=="2") {
      print("WARNING: NAs present within the data have not been ommitted!")
      data<-read.csv(myPath[i],header = TRUE, stringsAsFactors = FALSE)
      data[,dayofYear]<- gsub(".+([0-9]{3}).*", "\\1", myPath[i])
      data[,country]<-gsub(".+([A-Z]{1}).*", "\\1", dirName[i])
      header<-rbindlist(list(header,data),fill = TRUE)
    }
    if (naChoice=="3"){
      data<-read.csv(myPath[i],header = TRUE, stringsAsFactors = FALSE)
      data[,dayofYear]<- gsub(".+([0-9]{3}).*", "\\1", myPath[i])
      data[,country]<-gsub(".+([A-Z]{1}).*", "\\1", dirName[i])
      header<-rbindlist(list(header,data),fill = TRUE)
    }
  }
  header<-(header[-1,])  
  write.csv(header,file = "allDataTest.csv")
  print("Country X and Y files have been combined to a master file called allDataTest.csv")
}

#For the rest of the funcitons, we used "allData.csv" 
#USAGE 
# Define and run variable "dir" as the directory path that holds the files to be converted
# Define and run variable "naChoice":
#     naChoice <- 1 = Remove rows with NAs in any columns
#     naChoice <- 2 = Include NAs in the compiled data but be warned of their presence
#     naChoice <- 3 = Include NAs in the compiled data without a warning
# Run "csvMerge(dir,naChoice)"

#EXAMPLE: Append all .csv files within the RProject directory (Recursive)
# dir <- "/Users/bethoceguera/Documents/PhD Year 1/First Semester/Introduction to Biocomputing/Rproject"
# naChoice <- 1
# csvMerge(dir,naChoice)


##FUNCTION 3
# Function to summarize the compiled data set in terms of 
#   3a. Number of screens run
#   3b. Male vs. female patients
#   3c. Age distribution of patients
#   3d. Percent of patients screened that were infected

csvAnalysis <-  function(dir,filename){
  ## set working directory
  setwd(dir)
  
  ## load compiled data
  AllData = read.csv(filename)
  
  ## Number of screens run
  screenstotal = nrow(AllData) # The number of screens is equal to the number of rows in the compiled data
  Answer_1a = "The number of screens is equal to"
  Answer_1 = paste(Answer_1a,screenstotal, collapse = ":")
  
  ## Male vs Female
  malecount = nrow(AllData[AllData$gender == "male",]) # Finding the number of males that were screened
  femalecount = nrow(AllData[AllData$gender == "female",]) # Finding the number of females that were screened
  Answer_2a = "The number of males screened was"
  Answer_2b = "and the number of females screened was"
  Answer_2 = paste(Answer_2a,malecount,Answer_2b,femalecount, sep = " ")
  
  ## Age distribution
  # A histogram is a good way to look at age distribution for both countries 
  Age = AllData$age
  h=hist(Age, breaks = 50, xlim = c(0,100), ylim = c(0,26000), main = "Hisogram of Age Distribution From Screened Population")
  text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
  Answer_3a = "Any age over 100 is not represented in this graph, but they make up 178 data points." 
  Answer_3b = "However, based on this graph, it appears that the infection is effecting mostly people between the ages of 0-10"
  Answer_3 = paste(Answer_3a, Answer_3b)
  ## Percent of people infected
  infectedcounter = 0
  for(i in 1:nrow(AllData)){
    for(j in 1:ncol(AllData)){
      if(AllData[i,j] == 1){
        infectedcounter = infectedcounter + 1
        break
      } 
    }
  }
  percentinfected = (infectedcounter/screenstotal)*100
  Answer_4a = "The percentage of infected people was"
  Answer_4b = "percent"
  Answer_4 = paste(Answer_4a, percentinfected, Answer_4b)
  Answer_list = list(Answer_1, Answer_2, Answer_3, Answer_4)
  #Summary Table
  summaryTable <- data.frame(Parameter=c("Total Screen Runs",
                                         "Percent Positive","Male Count",
                                         "Female Count","Age 0-10","Age 11-20", 
                                         "Age 21-30","Age 31-40","Age 41-50",
                                         "Age 51-60","Age 61-70","Age 71-80",
                                         "Age 80+"),
                             Value=c(screenstotal,percentinfected,malecount,
                                     femalecount,sum(AllData$age%in%(0:10)),
                                     sum(AllData$age%in%(11:20)),
                                     sum(AllData$age%in%(21:30)),
                                     sum(AllData$age%in%(31:40)),
                                     sum(AllData$age%in%(41:50)),
                                     sum(AllData$age%in%(51:60)),
                                     sum(AllData$age%in%(61:70)),
                                     sum(AllData$age%in%(71:80)),
                                     nrow(AllData[AllData$age>=81,])))
  return(summaryTable)
  return(Answer_list)
}

#USAGE 
# Define and run variable "dir" as the directory path that holds the files to be converted
# Define and run variable "filename" as the file within dir to be 
# EXAMPLE: Run an analysis on the compiled data after having run the CSVMerge function
# dir <-  ":C/User/ikemu/Downloads/Rproject"
# filename <-  "allDataTest.csv"
# Run "csvAnalysis(dir,filename)"


#USAGE 
# Define and run variable "dir" as the directory path that holds the files to be converted
# Define and run variable "filename" as the file within dir to be 
# EXAMPLE: Run an analysis on the compiled data after having run the CSVMerge function
# dir <-  ":C/User/ikemu/Downloads/Rproject"
# filename <-  "allDataTest.csv"
# Run "csvAnalysis(dir,filename)"


##FUNCTION 4
# Function to create a time graph of infection for country X and country Y

csvAnalysis2 = function(dir, filename){
  # Set working directory
  setwd(dir)
  
  # Load Data and subset into country specific data
  AllData = read.csv(filename)
  file_x = subset(AllData, country == "X")
  file_y = subset(AllData, country == "Y")
  
  # Subset country data into infected files
  infectedXfile = file_x[which(file_x$marker01 == 1 | file_x$marker02 == 1 | file_x$marker03 == 1 | file_x$marker04 == 1 | file_x$marker05 == 1 | file_x$marker06 == 1 | file_x$marker07 == 1 | file_x$marker08 == 1 | file_x$marker09 == 1 | file_x$marker10 == 1),]
  infectedyfile = file_y[which(file_y$marker01 == 1 | file_y$marker02 == 1 | file_y$marker03 == 1 | file_y$marker04 == 1 | file_y$marker05 == 1 | file_y$marker06 == 1 | file_y$marker07 == 1 | file_y$marker08 == 1 | file_y$marker09 == 1 | file_y$marker10 == 1),]
  
  # Create vectors to be iterated over
  ## Time Vector
  LargestTime = max(AllData$dayofYear)
  SmallestTime = min(AllData$dayofYear)
  DiffTime = LargestTime - SmallestTime
  time_vec = 1:(DiffTime +1)
  
  ## Country X vector
  x_vec = 1:(DiffTime +1)
  
  ## Country Y vector 
  y_vec = 1:(DiffTime +1)
  
  # For loops for vectors to be plotted
  ## Time Vector
  time_vec[1] = SmallestTime
  for(i in 2:length(time_vec)){
    time_vec[i] = time_vec[i-1] + 1
  }
  
  ## Country X for loop
  dayX = 120
  for(i in 1:length(x_vec)){
    x_vec[i] = nrow(infectedXfile[infectedXfile$dayofYear == dayX,])
    dayX = dayX + 1
  }
  
  ## Country Y for loop
  dayY = 120
  for(i in 1:length(y_vec)){
    y_vec[i] = nrow(infectedyfile[infectedyfile$dayofYear == dayY,])
    dayY = dayY + 1
  }
  
  # Making data frame 
  combined_data = data.frame(time_vec, x_vec, y_vec)
  
  # Plotting data to show infection over time for country x and country y
  ## loading ggplot2
  library(ggplot2)
  
  ## Plot
  ggplot(combined_data, aes(time_vec)) + geom_line(aes(y=x_vec), color = "black") + geom_line(aes(y=y_vec), color = "blue") + xlab("Day of Year") +ylab("Number of Infections") + xlim(120,181) + ggtitle("Infections Per Day for Country X (Black) and Country Y (Blue)")
  print("The black line is country x infections per day and the blue line is country y infections per day")
}


#USAGE
# Define and run variable "dir" as the directory path that holds the file to run analysis on
# Define and run variable "filename" as the file within dir to have analysis ran on
# EXAMPLE: Run an analysis on the compiled data after having run the csvMerge function
# dir = ":C/User/ikemu/Downloads/Rproject"
# filename = "allDataTest.csv"
# Run "csvAnalysis2(dir,filename)"



## FUNCTION 5
# If Country Y develops a vaccine for the disease, is it likely to work for 
# citizens of Country X?
# This function compiles two bar graphs for country X and Y to show variation 
# in Markers 1 - 10

markerVariation <- function(file){
  
  setwd(dir)
  
  library(ggplot2)
  library(cowplot) 
  
  data <- read.csv(file)
  dfX <- data[data$country == "X",]
  dfY <- data[data$country == "Y",]
  df1 <- data.frame(Marker=c("1","2","3","4","5","6","7","8","9","10"),
                    XFrequency=c(nrow(dfX[dfX$marker01==1,]),
                                 nrow(dfX[dfX$marker02==1,]),
                                 nrow(dfX[dfX$marker03==1,]),
                                 nrow(dfX[dfX$marker04==1,]),
                                 nrow(dfX[dfX$marker05==1,]),
                                 nrow(dfX[dfX$marker06==1,]),
                                 nrow(dfX[dfX$marker07==1,]),
                                 nrow(dfX[dfX$marker08==1,]),
                                 nrow(dfX[dfX$marker09==1,]),
                                 nrow(dfX[dfX$marker10==1,])),
                    YFrequency=c(nrow(dfY[dfY$marker01==1,]),
                                 nrow(dfY[dfY$marker02==1,]),
                                 nrow(dfY[dfY$marker03==1,]),
                                 nrow(dfY[dfY$marker04==1,]),
                                 nrow(dfY[dfY$marker05==1,]),
                                 nrow(dfY[dfY$marker06==1,]),
                                 nrow(dfY[dfY$marker07==1,]),
                                 nrow(dfY[dfY$marker08==1,]),
                                 nrow(dfY[dfY$marker09==1,]),
                                 nrow(dfY[dfY$marker10==1,])))
  
  XFrequency <- 
    ggplot(df1, aes(x=Marker, y=XFrequency)) + geom_bar(stat = "identity")
  YFrequency <- 
    ggplot(df1, aes(x=Marker, y=YFrequency)) + geom_bar(stat = "identity")
  
  SummaryPlot <- 
    plot_grid(XFrequency,YFrequency,labels=c("a","b"),rel_widths=c(1,1),ncol=2,nrow=1)
  return(SummaryPlot)
}

#USAGE 
# Define and run variable "dir" as the directory path that holds the files to be converted
# Define and run variable "file" as the file that holds all data for both countries (i.e. allData.csv)
# EXAMPLE: Compile two barcharts that show the Marker distribution for countries X and Y
# dir <- "/Users/bethoceguera/Documents/PhD Year 1/First Semester/Introduction to Biocomputing/Rproject"
# file <- "allData.csv"
# Run "markerVariation("allData.csv")"

