rm(list = ls(all.names = TRUE))

# Filter dataset to final set of participants to analyze.
# Also clean up certain columns for downstream analyses.

library(ggplot2)
library(ggbeeswarm)

covid_data <- read.table(file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/COVID_data.csv.gz',
                         header = TRUE,
                         sep = ',',
                         stringsAsFactors = FALSE)

# Keep only one response of respondents with duplicate serial numbers.
length(which(duplicated(covid_data$Serial.Number)))
covid_data <- covid_data[-which(duplicated(covid_data$Serial.Number)), ]

# Ignore ages below 5 and above 95 (note that 999 indicates missing data).
length(which(covid_data$Age == 999))
length(which(covid_data$Age < 5))
length(which(covid_data$Age > 95 & covid_data$Age < 999))
covid_data <- covid_data[which(covid_data$Age >= 5), ]
covid_data <- covid_data[which(covid_data$Age <= 95), ]

# Remove the 999 values (missing data), and set to be NA.
covid_data[covid_data == 999] <- NA
covid_data[covid_data == "999"] <- NA

# Get clean column of PCR result.
covid_data$PCR.result.clean <- NA
covid_data$PCR.result.clean[which(covid_data$PCR.RESULT == 0)] <- "Negative"
covid_data$PCR.result.clean[which(covid_data$PCR.RESULT == 1 | covid_data$PCR.RESULT == 2)] <- "Positive"
covid_data$PCR.result.clean[which(covid_data$PCR.RESULT == 3)] <- "Unclear"

# Re-name governorates as needed to match shapefiles (and fix lowercase typo 'aakaar')
covid_data[which(covid_data$Governorate == 'aakkar'), 'Governorate'] <- 'Akkar'
covid_data[which(covid_data$Governorate == 'Aakkar'), 'Governorate'] <- 'Akkar'
covid_data[which(covid_data$Governorate == 'Baalbek_El-Hermel'), 'Governorate'] <- 'Baalbek-El Hermel'
covid_data[which(covid_data$Governorate == 'Mount-Lebanon'), 'Governorate'] <- 'Mount Lebanon'
covid_data[which(covid_data$Governorate == 'Beqaa'), 'Governorate'] <- 'Bekaa'
covid_data[which(covid_data$Governorate == 'Nabatieh'), 'Governorate'] <- 'El Nabatieh'

# Similarly, re-name Cazas
covid_data[which(covid_data$Caza == 'Batroun'), 'Caza'] <- "El Batroun"
covid_data[which(covid_data$Caza == 'El-Koura'), 'Caza'] <- "El Koura"
covid_data[which(covid_data$Caza == 'Keserwan'), 'Caza'] <- "Kesrwane"
covid_data[which(covid_data$Caza == 'El-Metn'), 'Caza'] <- "El Meten"
covid_data[which(covid_data$Caza == 'Aaley'), 'Caza'] <- "Aley"
covid_data[which(covid_data$Caza == 'El-Chouf'), 'Caza'] <- "Chouf"
covid_data[which(covid_data$Caza == 'Minieh-Danniyeh'), 'Caza'] <- "El Minieh-Dennie"
covid_data[which(covid_data$Caza == 'Rachaiya'), 'Caza'] <- "Rachaya"
covid_data[which(covid_data$Caza == 'Aakkar'), 'Caza'] <- "Akkar"
covid_data[which(covid_data$Caza == 'West Beqaa'), 'Caza'] <- "West Bekaa"
covid_data[which(covid_data$Caza == 'El-Hermel'), 'Caza'] <- "El Hermel"
covid_data[which(covid_data$Caza == 'Bent-Jbeil'), 'Caza'] <- "Bent Jbeil"
covid_data[which(covid_data$Caza == 'Sidon'), 'Caza'] <- "Saida"
covid_data[which(covid_data$Caza == 'Aakkar el-Chamali'), 'Caza'] <- "Akkar  El Chamali"

write.table(x = covid_data,
            file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/COVID_data_filt.csv',
            col.names = TRUE,
            row.names = FALSE,
            quote = TRUE,
            sep = ',')

nrow(covid_data)
