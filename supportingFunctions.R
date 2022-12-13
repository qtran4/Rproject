### Hayden Gallo
### Biocomputing R Project
### Functions Script

delim_convert <- function(directory_name){

  filenames <- list.files(path = paste0('./',directory_name), pattern = "\\.txt$") 
  count = 120
  for (i in filenames){
    sample <- read.table(paste0('./',directory_name,'/',i), header = TRUE)
    write.csv(sample, paste0('./',directory_name,'/screen_',count,'.csv'), row.names = FALSE)
    count = count + 1 
  }

}

compile_csv <- function(directory_name, NA_preference){
  filenames <- list.files(path = paste0('./',directory_name), pattern = "\\.csv$")
  count = 120
  for (i in filenames){
    if (!exists('compiled_data')){
      compiled_data <- read.csv(file = paste0('./',directory_name,'/',i), header = TRUE)
      compiled_data['country'] <- directory_name
      compiled_data['dayofYear'] <- count
    }
    
    if (exists('compiled_data')){
      
      temp <- read.csv(paste0('./',directory_name,'/',i))
      temp['country'] <- directory_name
      temp['dayofYear'] <- count
      compiled_data <- rbind(compiled_data, temp)
    }
    count = count + 1
  }
  if(NA_preference == 'remove'){
    compiled_data <- na.omit(compiled_data)
  }
  if(NA_preference == 'warn'){
    print('There are NA values')
  }
  write.csv(compiled_data, paste0('./',directory_name,'/compiled_data_',directory_name,'.csv'), row.names = FALSE)
  
}


summarize_compiled <- function(country_name){
  
  
  compiled_data <- read.csv(paste0('./',country_name,'/compiled_data_',country_name,'.csv'))
  num_screen_runs <- nrow(compiled_data)
  age_distribution <- table(compiled_data[,2])
  compiled_data['sum'] <- rowSums(compiled_data[,c(3:12)])
  num_infected_percent <- (num_screen_runs - colSums(compiled_data['sum'] == 0))/num_screen_runs
  male_v_female <- table(compiled_data[,1])
  stuff_to_return <- list('num_screen_runs' = num_screen_runs, 'age_dist' = age_distribution, 'inf_percent' = num_infected_percent, 'm_v_f' = male_v_female)
  return(stuff_to_return)
}


