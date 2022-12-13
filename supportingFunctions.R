##supporting functions for analysis

#Text file format conversion function
#This function converts all txt files in a given directory to csv files
#usage: txt_convert("./countryX")
#first define a function to convert a single txt file
single_txt_convert <- function(file_name){
  data <- read.table(file_name, sep = " ", header = TRUE)
  new_file_name <- gsub('txt', 'csv', file_name)
  write.csv(data, file = new_file_name, row.names=F)
}
#then use the function above to define the function for converting multiple files
txt_convert <- function(file_path){
  file_name_list <- list.files(path = file_path, pattern = ".txt")
  for(file in file_name_list){
    name_including_path <- paste(file_path, file, sep = "/")
    single_txt_convert(name_including_path)
  }
}

#Compilation function
#This function compiles all data of a given list of countries. Users should input a vector containing the name(s) of one or multiple countries.
#usage: all_data <- compile(c("X","Y"), NA_remove = FALSE, NA_warning = FALSE)
#requirements: the folders containing data from different countries should be put in the working directory, and they should be named like "countryX"
#define a function to add columns of country and date to a dataframe
add_col <- function(dataframe, country, date){
  country_col <- rep(country, times = nrow(dataframe))
  date_col <- rep(date, times = nrow(dataframe))
  country_col <- data.frame(country_col)
  date_col <- data.frame(date_col)
  data_to_add <- cbind(country_col, date_col)
  colnames(data_to_add) <- c("country","dayofYear")
  dataframe <- cbind(dataframe, data_to_add)
  return(dataframe)
}
#define a function to compile files of a single country
compile_single_country <- function(country_name){
  #create a void dataframe
  single_country_data <- data.frame(matrix(ncol = 14, nrow = 0))
  colnames(single_country_data) <- c("gender",	"age", "marker01", "marker02", 
                                     "marker03", "marker04", "marker05", "marker06", 
                                     "marker07", "marker08", "marker09", "marker10", 
                                     "country", "dayofYear")
  #get file names
  file_path <- paste("./country", country_name, sep = "")
  file_name_list <- list.files(path = file_path, pattern = ".csv")
  #compile files
  for(file in file_name_list){
    file_name_including_path <- paste(file_path, file, sep = "/")
    single_file_data <- read.csv(file_name_including_path, sep = ",", header = TRUE)
    date <- substr(file, 8, 10)
    single_file_data <- add_col(single_file_data, country_name, date)
    single_country_data <- rbind(single_country_data, single_file_data)
  }
  return(single_country_data)
}
#define a function to compile all files
compile <- function(country_name_list, NA_remove = FALSE, NA_warning = FALSE){
  #create a void dataframe
  all_data <- data.frame(matrix(ncol = 14, nrow = 0))
  colnames(all_data) <- c("gender",	"age", "marker01", "marker02", 
                                     "marker03", "marker04", "marker05", "marker06", 
                                     "marker07", "marker08", "marker09", "marker10", 
                                     "country", "dayofYear")
  #compile files
  for(country in country_name_list){
    all_data <- rbind(all_data, compile_single_country(country))
  }
  #deal with NAs
  if(NA_warning == TRUE){
    nNA <- sum(is.na(all_data))
    output <- paste(nNA, "NA(s) found in", nrow(all_data)*ncol(all_data), "data", sep = " ")
    print(output)
  }
  if(NA_remove == TRUE){
    all_data <- na.omit(all_data)
  }
  #return the final result
  return(all_data)
}

#function to find out the earliest date that at least one patient was found in a given country
find_first_positive <- function(country_name){
  file_path <- paste("./country", country_name, sep = "")
  file_name_list <- list.files(path = file_path, pattern = ".csv")
  for(file in file_name_list){
    file_name_including_path <- paste(file_path, file, sep = "/")
    data_to_check <- read.csv(file_name_including_path, sep = ",", header = TRUE)
    date <- substr(file, 8, 10)
    for(i in 1:nrow(data_to_check)){
      if(rowSums(data_to_check[i,3:12]) != 0){
        return(date)
      }
    }
  }
}

#summarize function
summarize <- function(data){
  allData <- data[data$age<100,]
  screened<-nrow(allData)
  allData$infectedSums <- rowSums(allData[,3:12] )
  allData$infected <- ifelse(allData$infectedSums>0, 1, 0)
  infectedData <- allData[allData$infected==1,]
  percentInf<- nrow(infectedData)/nrow(allData)
  males <- nrow(allData[allData$gender =="male",])
  infectedMales <- nrow(infectedData[infectedData$gender =="male",])
  percentMalesInfected <- infectedMales/males
  females <- nrow(allData[allData$gender =="female",])
  infectedFemales <- nrow(infectedData[infectedData$gender =="female",])
  percentFemalesInfected <- infectedFemales/females
  ageHist<-ggplot(data=allData, aes(x=age), fill='green', alpha = 0.2) +
    geom_histogram() +
    geom_histogram(data=infectedData, aes(x=age), fill='red', alpha=0.2)
  returnString <- cat("Number Screened: ", screened , "\nPercentage Infected: " , percentInf , "\nNumber of Males: " , males , "\nPercentage of Males Infected: " , percentMalesInfected , "\nNumber of Females: " , females , "\nPercentage of Females Infected: ", percentFemalesInfected , "\nAge Distribution Graph (grey is total, red is infected): ")
  print(returnString)
  print(ageHist)
}