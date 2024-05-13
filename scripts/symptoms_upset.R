rm(list = ls(all.names = TRUE))

library(ComplexUpset)
library(ggplot2)
library(stringr)
library(patchwork)
library(dplyr)

covid_data <- read.table(file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/COVID_data_filt.csv.gz',
                         header = TRUE,
                         sep = ',',
                         stringsAsFactors = FALSE)

symptoms_orig <- c("Smell.Loss", "Taste.Loss", "Rhinorrhea", "Dry.Cough",
                   "Productive.Cough", "Cold", "Diarrhea", "Sore.throat",
                   "Myalgia", "Headache", "Fever", "Shortness.breath", "Fatigue",
                   "Abdominal.Pain", "Extremities.Redness.Pain", "Pink.Eye")

symptoms <- gsub("\\.", " ", symptoms_orig)
symptoms <- str_to_sentence(symptoms)

symptoms[which(symptoms == "Shortness breath")] <- "Shortness of breath"

covid_data_symptoms <- covid_data[, c(symptoms_orig, 'PCR.result.clean')]
colnames(covid_data_symptoms) <- c(symptoms, 'PCR.result.clean')

# Remove any rows with NA symptoms.
covid_data_symptoms <- covid_data_symptoms[-which(rowSums(is.na(covid_data_symptoms)) > 0), ]

covid_data_symptoms_positive <- covid_data_symptoms[which(covid_data_symptoms$PCR.result.clean == 'Positive'), which(colnames(covid_data_symptoms) != 'PCR.result.clean')]
covid_data_symptoms_negative <- covid_data_symptoms[which(covid_data_symptoms$PCR.result.clean == 'Negative'), which(colnames(covid_data_symptoms) != 'PCR.result.clean')]

# Comparison of the number of symptoms per respondent.
num_symptoms <- data.frame(Result = c(rep('Negative', nrow(covid_data_symptoms_negative)),
                                      rep('Positive', nrow(covid_data_symptoms_positive))),
                           Num_symptoms = c(rowSums(covid_data_symptoms_negative),
                                            rowSums(covid_data_symptoms_positive)))

num_symptoms <- num_symptoms %>% dplyr::count(Result, Num_symptoms)

num_symptoms$percent <- NA
num_symptoms[which(num_symptoms$Result == 'Negative'), 'percent'] <- (num_symptoms[which(num_symptoms$Result == 'Negative'), 'n'] / sum(num_symptoms[which(num_symptoms$Result == 'Negative'), 'n'])) * 100
num_symptoms[which(num_symptoms$Result == 'Positive'), 'percent'] <- (num_symptoms[which(num_symptoms$Result == 'Positive'), 'n'] / sum(num_symptoms[which(num_symptoms$Result == 'Positive'), 'n'])) * 100

num_symptoms_plot <- ggplot(data = num_symptoms, aes(x = Num_symptoms, y = percent, fill = Result)) +
  geom_col(position = 'dodge') +
  ylab('Percent of respondents') +
  xlab('Number of symptoms') +
  theme_bw() +
  scale_fill_manual(name = 'PCR result', values=c('grey80', 'coral3'))


# Then remove asymptomatic cases, as they are by far the most frequent category.
covid_data_symptoms_positive_NoAsymp <- covid_data_symptoms_positive[which(rowSums(covid_data_symptoms_positive) > 0), ]
covid_data_symptoms_negative_NoAsymp <- covid_data_symptoms_negative[which(rowSums(covid_data_symptoms_negative) > 0), ]

# NOTE LIMITED TO ONLY CATEGORIES WITH > 5 INSTANCES FOR THIS PLOT
pos_upset <- upset(covid_data_symptoms_positive_NoAsymp,
                   intersect = sort(symptoms, decreasing = TRUE),
                   #n_intersections = 40,
                   width_ratio=0.1,
                   wrap = TRUE,
                   sort_sets = FALSE,
                   name = '',
                   min_size = 5,
                   height_ratio = 0.85,
                   stripes = upset_stripes(geom = geom_segment(size = 5)),
                   themes=upset_modify_themes(
                     list(
                       'intersections_matrix'=theme(axis.text.y=element_text(margin=margin(l=-80))),
                       'overall_sizes'=theme(axis.text.x=element_text(angle=45),
                                             plot.margin=margin(r=80))
                     )
                   )) +
            ggtitle("Positive SARS-CoV-2 Results") + theme(plot.title = element_text(hjust = 0.5))

# SINCE THE ABOVE RESULTED IN 20 CATEGORIES, DECIDED TO JUST LIMIT TO TOP 20 HERE TO MAKE IT EASIER TO COMPARE
neg_upset <- upset(covid_data_symptoms_negative_NoAsymp,
                   intersect = sort(symptoms, decreasing = TRUE),
                   n_intersections = 20,
                   width_ratio=0.1,
                   wrap = TRUE,
                   sort_sets = FALSE,
                   name = '',
                   min_size = 5,
                   height_ratio = 0.85,
                   stripes = upset_stripes(geom = geom_segment(size = 5)),
                   themes=upset_modify_themes(
                     list(
                       'intersections_matrix'=theme(axis.text.y=element_text(margin=margin(l=-80))),
                       'overall_sizes'=theme(axis.text.x=element_text(angle=45),
                                                plot.margin=margin(r=80))
                     )
                   )) + 
               ggtitle("Negative SARS-CoV-2 Results") + theme(plot.title = element_text(hjust = 0.5))




combined_upset <- pos_upset / neg_upset + plot_annotation(tag_levels = 'a')


# Write out plots and symptom count data.
ggsave(filename = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/display_items/num_symptoms.pdf',
       plot = num_symptoms_plot,
       device = 'pdf',
       width = 6,
       height = 4,
       units = 'in')

ggsave(filename = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/display_items/symptom_upsets.pdf',
       plot = combined_upset,
       device = 'pdf',
       width = 10,
       height = 12,
       units = 'in')

write.table(file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/display_data/num_symptoms_by_result.tsv',
            x = num_symptoms,
            col.names = TRUE,
            row.names = FALSE,
            sep = '\t',
            quote = FALSE)
