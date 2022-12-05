##Below, the five functions created for this project are run via "source()"
source("supportingFunctions.R")

##FUNCTION 1
# Function that converts all files in a directory with space- or 
#   tab-delimited data (.txt) into comma-separated value files.
dir <- 
  "~/Documents/PhD Year 1/First Semester/Introduction to Biocomputing/Rproject/countryY" 
csvConvert(dir)

##FUNCTION 2
# Function that compiles data from all .csv files in a directory 
# into a single .csv file. The compiled data has the original twelve columns 
# from daily data sheets, but also country and dayofYear columns. 
# The user should be able to choose whether they want to remove rows with NA’s 
# in any columns, include NAs in the compiled data but be warned of their presence, or include NAs in the compiled data without a warning
dir <- 
  "~/Documents/PhD Year 1/First Semester/Introduction to Biocomputing/Rproject"
naChoice <- 1
csvMerge(dir,naChoice)

##FUNCTION 3
# Function to summarize the compiled data set in terms of 
#   3a. Number of screens run
#   3b. Male vs. female patients
#   3c. Age distribution of patients
#   3d. Percent of patients screened that were infected
dir <- 
  "~/Documents/PhD Year 1/First Semester/Introduction to Biocomputing/Rproject"
filename <-  "allData.csv"
csvAnalysis(dir,filename) #This will return a summary table of 3a - 3d

## FUNCTION 4



## FUNCTION 5 (Question 2)
# This function compiles two bar graphs for two countries to show variation 
# in Markers 1 - 10
dir <- 
  "~/Documents/PhD Year 1/First Semester/Introduction to Biocomputing/Rproject"
file <- "allData.csv"
markerVariation("allData.csv") #This will return the graph for both countries

#Explanation
#   If Country Y develops a vaccine for the disease, it is likely it would not
# work for country X. From the graph- SummaryPlot- one can see that those in 
# country X test positive for mostly markers 1 - 5. On the other hand, country
# Y tests positive for mostly markers 6 - 10. As vaccines often target the 
# proteins of a pathogen, it is likely that one vaccine may not offer protection 
# to both countries.
