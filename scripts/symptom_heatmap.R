rm(list = ls(all.names = TRUE))

library(ComplexHeatmap)
#library(magick)
library(stringr)
library(circlize)

covid_data <- read.table(file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/COVID_data_filt.csv.gz',
                         header = TRUE,
                         sep = ',',
                         stringsAsFactors = FALSE)

rownames(covid_data) <- covid_data$Serial.Number

covid_data <- covid_data[which(covid_data$PCR.result.clean != 'Unclear'), ]

symptoms <- c("Smell.Loss", "Taste.Loss", "Rhinorrhea", "Dry.Cough",
              "Productive.Cough", "Cold", "Diarrhea", "Sore.throat",
              "Myalgia", "Headache", "Fever", "Shortness.breath", "Fatigue",
              "Abdominal.Pain", "Extremities.Redness.Pain", "Pink.Eye")

covid_data_symptoms <- covid_data[, symptoms]

covid_data_symptoms_noNA <- covid_data_symptoms[-which(is.na(covid_data_symptoms$Smell.Loss)), ]
covid_data_symptoms_noNA <- covid_data_symptoms_noNA[-which(is.na(covid_data_symptoms_noNA$Extremities.Redness.Pain)), ]
covid_data_symptoms_noNA <- covid_data_symptoms_noNA[-which(is.na(covid_data_symptoms_noNA$Pink.Eye)), ]


colnames(covid_data_symptoms_noNA) <- gsub("\\.", " ", colnames(covid_data_symptoms_noNA))
colnames(covid_data_symptoms_noNA) <- str_to_sentence(colnames(covid_data_symptoms_noNA))

# Ignore all asymptomatic samples (after getting overall positivity per symptom).
symptom_prevalence <- (colSums(covid_data_symptoms_noNA) / nrow(covid_data_symptoms_noNA)) * 100
covid_data_symptoms_noNA <- covid_data_symptoms_noNA[which(rowSums(covid_data_symptoms_noNA) > 0), ]

covid_data_symptoms_noNA_char <- covid_data_symptoms_noNA
covid_data_symptoms_noNA_char[covid_data_symptoms_noNA_char == 0] <- 'Not reported'
covid_data_symptoms_noNA_char[covid_data_symptoms_noNA_char == '1'] <- 'Reported'

symptom_heatmap <- Heatmap(matrix = t(covid_data_symptoms_noNA_char),
                           name = 'Symptom',
                           col = c('grey90', 'cornflowerblue'),
                           use_raster = FALSE,
                           raster_by_magick = FALSE,
                           cluster_rows = hclust(dist(t(covid_data_symptoms_noNA), 'binary')),
                           cluster_columns = hclust(dist(covid_data_symptoms_noNA, 'binary')),
                           top_annotation = HeatmapAnnotation("PCR result" = covid_data[rownames(covid_data_symptoms_noNA), 'PCR.result.clean'],
                                                              col = list(`PCR result` = c("Negative" = "grey99", "Positive" = "red"))),
                           right_annotation = rowAnnotation('Prev.' = symptom_prevalence, col = list(Prev. = colorRamp2(c(0, 30), c("white", "orange")))),
                           show_column_names = FALSE,
                           show_column_dend = FALSE)

ggsave(filename = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/display_items/symptom_heatmap.pdf',
       plot = grid.grabExpr(draw(symptom_heatmap)),
       device = 'pdf',
       width = 10,
       height = 8,
       units = 'in')

# Write out overall symptom prevalance as well.
symptom_prev_clean <- data.frame(symptom=names(symptom_prevalence), prevalence=symptom_prevalence)

write.table(file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/display_data/overall_symptom_prevalence.tsv',
            x = symptom_prev_clean,
            col.names = TRUE,
            row.names = FALSE,
            sep = '\t',
            quote = FALSE)
