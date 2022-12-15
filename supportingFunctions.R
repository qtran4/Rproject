### Hayden Gallo
### Biocomputing R Project
### Functions Script

delim_convert <- function(directory_name){

  filenames <- list.files(path = paste0('./',directory_name), pattern = "\\.txt$") #grab all file names with .txt
  count = 120
  for (i in filenames){
    sample <- read.table(paste0('./',directory_name,'/',i), header = TRUE) #loop through each of the .txt files grabbing one at a time
    write.csv(sample, paste0('./',directory_name,'/screen_',count,'.csv'), row.names = FALSE) #writing each .txt file to .csv
    count = count + 1 #iterating count for file naming scheme
  }

}

compile_csv <- function(directory_name, NA_preference){ #three options for NA_preference are 'remove', 'warn', or putting nothing does nothing
  filenames <- list.files(path = paste0('./',directory_name), pattern = "\\.csv$") #grab all file names with .csv
  count = 120
  for (i in filenames){ #iteratre over all .csvs in directory
    if (!exists('compiled_data')){ #if original compile file isn't created create it
      compiled_data <- read.csv(file = paste0('./',directory_name,'/',i), header = TRUE) #load first file
      compiled_data['country'] <- directory_name #add country name column
      compiled_data['dayofYear'] <- count #add day of year column
    }
    
    if (exists('compiled_data')){ #go here if compile data file is already created
      
      temp <- read.csv(paste0('./',directory_name,'/',i)) #read each of files one by one
      temp['country'] <- directory_name # add country name column
      temp['dayofYear'] <- count # add day of year column
      compiled_data <- rbind(compiled_data, temp) #merge temp file with compile data file
    }
    count = count + 1 #iterate count for name and day of year
  }
  if(NA_preference == 'remove'){ #if user wants NAs removed this loop does it
    compiled_data <- na.omit(compiled_data)
  }
  if(NA_preference == 'warn'){ # if users want to warn about NAs this loop does it 
    if(anyNA(countryX_data) == 'FALSE'){
      print('There are no NA values')} else{print("There are NA values")}
  }# no loop for if user wants no warning of NAs
  write.csv(compiled_data, paste0('./',directory_name,'/compiled_data_',directory_name,'.csv'), row.names = FALSE) #write final compiled dataframe to a .csv
  return(compiled_data)
}


summarize_compiled <- function(file_name, specific_country){ #include file name and if it is specific country data or all data this is done with simple 'yes' or 'no' for second argument
  
  if (specific_country == 'yes'){
    country_name <- file_name
    compiled_data <- read.csv(paste0('./',country_name,'/compiled_data_',country_name,'.csv'))
  }else {compiled_data <- read.csv(paste0('./',file_name))} #opens csv of all compiled data that was provided
  num_screen_runs <- nrow(compiled_data) # number of rows of data is number of screens
  age_distribution <- table(compiled_data[,2]) #table function shows age distribution
  compiled_data['sum'] <- rowSums(compiled_data[,c(3:12)]) #sum across columns to see who was infections
  num_infected_percent <- (num_screen_runs - colSums(compiled_data['sum'] == 0))/num_screen_runs #total runs minus columns containing 0 divided by runs shows total infected
  male_v_female <- table(compiled_data[,1]) #table again for male vs female
  stuff_to_return <- list('num_screen_runs' = num_screen_runs, 'age_dist' = age_distribution, 'inf_percent' = num_infected_percent, 'm_v_f' = male_v_female) # return the 4 different values in one list
  return(stuff_to_return) #return list of values to return
}

