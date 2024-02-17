rm(list = ls(all.names = TRUE))

# Compute PCoA and calculate % variation in symptoms explained by positivity.
library(vegan)
library(ggplot2)
library(rlang)

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

symptoms_at_least_two <- covid_data_symptoms_noNA[which(rowSums(covid_data_symptoms_noNA) >= 2), ]

symptoms_at_least_two_dist <- dist(symptoms_at_least_two, method = 'binary')

symptoms_at_least_two_pcoa <- cmdscale(symptoms_at_least_two_dist, eig = TRUE)

pcoa_data <- data.frame(symptoms_at_least_two_pcoa$points)

pcoa_data$Result <- covid_data[rownames(pcoa_data), 'PCR.result.clean']

eig_values <- symptoms_at_least_two_pcoa$eig
positive_eigenvalues <- eig_values[eig_values > 0]
total_variance <- sum(positive_eigenvalues)
percent_variance_explained <- (positive_eigenvalues / total_variance) * 100

# Run PERMANOVA to test for how positivity explains variation in symptoms across samples.
result_vec <- covid_data[rownames(symptoms_at_least_two), 'PCR.result.clean']
result_vec <- factor(result_vec)
adonis_output <- vegan::adonis2(formula = symptoms_at_least_two_dist ~ result_vec)

adonis_label <- paste('PERMANOVA\n',
                      expression("R"^2*"="),
                      as.character(round(adonis_output$R2[1], 4)), '\n',
                      'P=',as.character(round(adonis_output$`Pr(>F)`[1], 4)),
                      sep = '')


clean_R2 <- sprintf("%.4f", adonis_output$R2[1])
clean_P <- sprintf("%.4f", adonis_output$`Pr(>F)`[1])

colnames(pcoa_data) <- c('PC1', 'PC2', 'Result')

symptom_pcoa_scatterplot <- ggplot(data = pcoa_data, aes(x = PC1, y = PC2, colour = Result)) +
  geom_point() +
  theme_bw() +
  scale_colour_manual(values = c('grey80', 'red')) +
  xlab(paste('Principal Coordinate 1 (', as.character(round(percent_variance_explained[1], 1)), '% variance explained)', sep = '')) +
  ylab(paste('Principal Coordinate 2 (', as.character(round(percent_variance_explained[2], 1)), '% variance explained)', sep = '')) +
  annotate('text', x = 0.27, y = -0.5, label = 'PERMANOVA') +
  annotate('text', x = 0.27, y = -0.55, label = paste("R^2 == ", clean_R2), parse = TRUE) +
  annotate('text', x = 0.27, y = -0.6, label = paste("P = ", clean_P, sep = '')) +
  ggtitle('Sample similarity based on symptom overlap\n(restricted to individuals with at least two symptoms)') +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/display_items/symptom_pcoa.pdf',
       plot =symptom_pcoa_scatterplot,
       device = 'pdf',
       width = 6,
       height = 5,
       units = 'in')

# Write out PCoA data.
write.table(file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/display_data/symptom_PCoA_point.tsv',
            x = pcoa_data,
            col.names = TRUE,
            row.names = FALSE,
            sep = '\t',
            quote = FALSE)
