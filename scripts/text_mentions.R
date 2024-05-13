rm(list = ls(all.names = TRUE))

library(ggplot2)
library(ggbeeswarm)

covid_data <- read.table(file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/COVID_data_filt.csv.gz',
                         header = TRUE,
                         sep = ',',
                         stringsAsFactors = FALSE)

# Check the distribution of different Governorates and gender, irrespective of positivity.
table(covid_data$Governorate)
(table(covid_data$Governorate) / length(covid_data$Governorate)) * 100

table(covid_data$Gender)
(table(covid_data$Gender) / length(covid_data$Gender)) * 100

# Mean age
mean(covid_data$Age)

# Then check some factors by positivity.
covid_data_male <- covid_data[which(covid_data$Gender == 1), ]
covid_data_female <- covid_data[which(covid_data$Gender == 2), ]

(table(covid_data_male$PCR.result.clean) / length(covid_data_male$PCR.result.clean)) * 100
(table(covid_data_female$PCR.result.clean) / length(covid_data_female$PCR.result.clean)) * 100

gender_positivity_tab <- matrix(c(length(which(covid_data_male$PCR.result.clean == "Positive")),
                                  length(which(covid_data_female$PCR.result.clean == "Positive")),
                                  length(which(covid_data_male$PCR.result.clean == "Negative")),
                                  length(which(covid_data_female$PCR.result.clean == "Negative"))),
                                nrow = 2, ncol = 2)

# Check % blood group 
covid_data_positive <- covid_data[which(covid_data$PCR.result.clean == "Positive"), ]
covid_data_negative <- covid_data[which(covid_data$PCR.result.clean == "Negative"), ]

blood_group_tab <- matrix(c(as.integer(table(covid_data_positive$blood.group)) / sum(as.integer(table(covid_data_positive$blood.group))),
                            as.integer(table(covid_data_negative$blood.group)) / sum(as.integer(table(covid_data_negative$blood.group)))),
                          nrow = 9, ncol = 2)

blood_group_counts <- matrix(c(as.integer(table(covid_data_positive$blood.group)),
                            as.integer(table(covid_data_negative$blood.group))),
                          nrow = 9, ncol = 2)

(table(covid_data$blood.group) / length(covid_data$blood.group)) * 100
