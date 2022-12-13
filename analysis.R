### Hayden Gallo
### Biocomputing R Project
### Analysis Script
library(ggplot2)

source('./supportingFunctions.R')


#1. In which country (X or Y) did the disease outbreak likely begin?

delim_convert('countryY')

# compile all country data
# 3 choices for NA reporting with compile_csv
# 1) 'remove', this argument will remove NA values from the compiled data
# 2) 'warn', warns about NA values, but does not remove them
# 3)  supplying no argument does nothing

compile_csv('countryY', 'remove')
compile_csv('countryX', 'remove')

countryY_summary <- summarize_compiled('countryY')
countryX_summary <- summarize_compiled('countryX')

countryX_data <- read.csv('./countryX/compiled_data_countryX.csv', header = TRUE)
countryY_data <- read.csv('./countryY/compiled_data_countryY.csv', header = TRUE)


countryX_data['sum'] <- rowSums(countryX_data[,c(3:12)])
countryY_data['sum'] <- rowSums(countryY_data[,c(3:12)])

countryX_data['infected'] <- ifelse(countryX_data['sum'] >= 1,1,0)
countryY_data['infected'] <- ifelse(countryY_data['sum'] >= 1,1,0)

countryX_infections_day <- aggregate(countryX_data$infected~countryX_data$dayofYear, countryX_data, sum)
countryY_infections_day <- aggregate(countryY_data$infected~countryY_data$dayofYear, countryY_data, sum)

ggplot() + 
  geom_line(data = countryX_infections_day, aes(x = `countryX_data$dayofYear`, y = sum, color = 'Country X')) + 
  geom_line(data = countryY_infections_day, aes(x = `countryY_data$dayofYear`, y = sum, color = 'Country Y')) +
  xlab('Day of Year') + 
  ylab('Number of Infections Per Day') + 
  ggtitle('Infections Per Day by Country')

# Based on the data and the line plot of infections per day, it seems as if Country X experienced
# the first outbreak of the disease. Looking at the graph it can be seen that Country X saw its first
# infections on day 120 of the year whereas, infections didn't begin in Country Y until around day 137 according
# to the line plot. This graph seems to confirm that the disease outbreak began in Country X.


#2. If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?

# plot distributions of markers 

marker_sum_country_X <- data.frame(colSums(countryX_data[,c(3:12)]))
marker_sum_country_Y <- data.frame(colSums(countryY_data[,c(3:12)]))



marker_sum_country_X <- cbind(rownames(marker_sum_country_X), data.frame(marker_sum_country_X, row.names=NULL))
marker_sum_country_Y <- cbind(rownames(marker_sum_country_Y), data.frame(marker_sum_country_Y, row.names=NULL))

colnames(marker_sum_country_X) <- c('marker','instances')
colnames(marker_sum_country_Y) <- c('marker','instances')

marker_sum_country_X$instances <- marker_sum_country_X$instances/sum(marker_sum_country_X$instances)
marker_sum_country_Y$instances <- marker_sum_country_Y$instances/sum(marker_sum_country_Y$instances)

ggplot() + 
  geom_bar(marker_sum_country_X, stat = 'identity', position = 'dodge', mapping = aes(x=marker, y=instances, fill = 'Country X'), alpha = 0.8) +
  geom_bar(marker_sum_country_Y, stat = 'identity', position = 'dodge', mapping = aes(x=marker, y=instances, fill = 'Country Y'), alpha = 0.8) +
  xlab('Marker') + 
  ylab('Proportion of Infections by Marker') +
  ggtitle('Distribution of Infections by Marker For Each Country')

# Based on the comparison of the distributions of positive markers in each of the two countries
# it seems as if Country Y developed an effective vaccine for its populous, then the vaccine
# would most likely not be effective for the citizens of Country X. This is because most of the infections
# in Country X show positive presence of Markers 1-5 and almost never show presence of markers 6-10.
# However, in Country Y, the most prevalent markers are markers 6-10 with just a little prevalence of 
# markers 1-5. So based on the countries having different distributions of the prevalence of markers,
# it would seem to be that a vaccine developed for Country Y would not be very effective for the citizens
# of Country X.


