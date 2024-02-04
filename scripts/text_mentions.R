rm(list = ls(all.names = TRUE))

library(ggplot2)
library(ggbeeswarm)

covid_data <- read.table(file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/COVID_data_filt.csv.gz',
                         header = TRUE,
                         sep = ',',
                         stringsAsFactors = FALSE)

# Check the distribution of different Governorates 
table(covid_data$Governorate)
(table(covid_data$Governorate) / length(covid_data$Governorate)) * 100

# Check % blood group 
(table(covid_data$blood.group) / length(covid_data$blood.group)) * 100

 
 
 