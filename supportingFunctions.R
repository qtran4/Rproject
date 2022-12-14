####R Project
####Supporting Functions
####Shannon Culbert and Camden Arnold
####All code assumes working directory is set to RProject directory

################################################################################

###Part 1: Converting all .txt files into .csv files
#Convert all .txt files to .csv files using for loop

convert_to_csv <- function(dir) {
  txt_files <- list.files(dir,pattern=".txt") #lists .txt files that need converted
  for (i in txt_files) {
    file_path <- file.path(dir,i)
    newfile_name <- sub(".txt",".csv",i)
    file_path2 <- file.path(dir,newfile_name)
    data <- read.table(file=file_path, header=TRUE, sep="") #reads .txt file
    write.csv(data,file=file_path2,row.names=FALSE,col.names=FALSE) #converts to .csv
  }
}
#Country X has .csv files already
#Country Y needs .txt files to be converted to .csv files
#run function to convert Country Y's .txt files to .csv files
#usage: convert_to_csv("countryY")

################################################################################

###Part 2: Converting all .csv files into a single .csv file

#create path to add all of the information into "allData.csv"
#includes adding columns for Country and DayOfYear
combine_data <- function(dir1,dir2) { #creates allData.csv to which data will be added
  path_init <- file.path("countryX","screen_120.csv")
  data_init <- read.csv(path_init,header=TRUE)
  data_init$country = "X"
  data_init$dayofYear = "120"
  write.csv(head(data_init,n=0), file = "allData.csv",col.names=FALSE, row.names=FALSE)
  
  #First country will be added into the "allData.csv"
  files_1 <- list.files(dir1,pattern=".csv")
  for (i in files_1) {
    file_path <- file.path(dir1,i)
    day_number <- substr(i,8,10)
    country_letter <- substr(dir1,8,8)
    data <- read.csv(file=file_path, header=TRUE)
    data$country = country_letter
    data$dayofYear = day_number
    write.table(data, file = "allData.csv", append=TRUE, sep=",", col.names=FALSE, row.names=FALSE)
  }
  
  #Second country will be added into the "allData.csv"
  files_2 <- list.files(dir2,pattern=".csv")
  for (i in files_2) {
    file_path <- file.path(dir2,i)
    day_number <- substr(i,8,10)
    country_letter <- substr(dir2,8,8)
    data <- read.csv(file=file_path, header=TRUE)
    data$country = country_letter
    data$dayofYear = day_number
    write.table(data, file = "allData.csv", append=TRUE, sep=",", col.names=FALSE, row.names=FALSE)
  }  
}

#run function for countryX and countryY
#usage: combine_data("countryX","countryY")

################################################################################

###Part 3: Summarizing data sets 

summary_data <- function(file){
  allData <- read.table(file("allData.csv"), header = TRUE, sep = ",")
  ##Part 3A: Number of Screens Run 
  #Number of screens run will be equal to the number of rows in the allData file
  print("The total number of screens run is:")
  print(nrow(allData))
  
  ##Part 3B: Percent of Patients Screened that were Infected
  infected_count = 0
  for(i in 1:nrow(allData)){
    if (sum(allData[i,3:12]) > 0) {
      infected_count = infected_count + 1}
  }
  percent.patients.infected <- (infected_count/nrow(allData))*100
  print("The percentage of screened patient's who are infected is:")
  print(percent.patients.infected)
  
  ##Part 3C: Male vs. Female Patients
  males <- allData[allData$gender=="male",]
  print("The number of all male patients is:")
  print(nrow(males))
  
  females <- allData[allData$gender=="female",]
  print("The number of all female patients is:")
  print(nrow(females))
  
  #if only looking at infected Male vs. Female Patients
  Males <- allData[allData$gender=="male",]
  Females <- allData[allData$gender=="female",]
  
  Males.infected_count = 0
  for(i in 1:nrow(Males)){
    if (sum(Males[i,3:12]) > 0) {
      Males.infected_count = Males.infected_count + 1}
  }
  print("The number of male patients who are infected is:")
  print(Males.infected_count)
  
  Females.infected_count = 0
  for(i in 1:nrow(Females)){
    if (sum(Females[i,3:12]) > 0) {
      Females.infected_count = Females.infected_count + 1}
  }
  print("The number of female patients who are infected is:")
  print(Females.infected_count)
  
  ##Part 3D: Age Distribution of Patients 
  print("The statistical summary of age distribution of all patients is:")
  print(summary(allData$age))
  
  #if only looking at age distribution of infected individuals
  allData$markerSum=allData$marker01+allData$marker02+allData$marker03+allData$marker04+allData$marker05+allData$marker06+allData$marker07+allData$marker08+allData$marker09+allData$marker10
  allData.infected <- allData[allData$markerSum>0,]
  print("The statistical summary of age distribution of all infected patients is:")
  print(summary(allData.infected$age))
  #recording error by study: maximum age is 423???
}  

#run function to view the answers for each part:
#usage: summary_data("allData.csv")