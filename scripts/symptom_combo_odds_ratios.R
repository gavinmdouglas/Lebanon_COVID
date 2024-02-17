rm(list = ls(all.names = TRUE))

# Get a breakdown of how enriched different combinations of symptoms are between
# positive and negative samples.

# Use same approach as for the individual symptoms, but restrict to cases where there at least
# 10 samples (irrespective of whether these are positive and/or negative cases).

# Also, ignore single symptoms for this analysis, as they were tested elsewhere.

library(ggplot2)
library(stringr)
library(exact2x2)

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

# Remove all individuals with <= 1 symptoms for this analysis.
covid_data_symptoms_noNA <- covid_data_symptoms_noNA[which(rowSums(covid_data_symptoms_noNA) > 1), ]

colnames(covid_data_symptoms_noNA) <- gsub("\\.", " ", colnames(covid_data_symptoms_noNA))
colnames(covid_data_symptoms_noNA) <- str_to_sentence(colnames(covid_data_symptoms_noNA))

symptoms_clean <- colnames(covid_data_symptoms_noNA)

PCR_result <- covid_data[rownames(covid_data_symptoms_noNA), 'PCR.result.clean']

covid_data_symptoms_noNA_neg <- covid_data_symptoms_noNA[which(PCR_result == 'Negative'), ]
covid_data_symptoms_noNA_pos <- covid_data_symptoms_noNA[which(PCR_result == 'Positive'), ]

# Convert presence/absence table into vector of symptom combinations.
neg_symptom_combos <- character()
for (neg_i in 1:nrow(covid_data_symptoms_noNA_neg)) {
  neg_symptoms_present_i <- which(covid_data_symptoms_noNA_neg[neg_i, ] > 0)
  neg_combo_str <- paste(sort(symptoms_clean[neg_symptoms_present_i]), collapse ='; ')
  neg_symptom_combos <- c(neg_symptom_combos, neg_combo_str)
}

pos_symptom_combos <- character()
for (pos_i in 1:nrow(covid_data_symptoms_noNA_pos)) {
  pos_symptoms_present_i <- which(covid_data_symptoms_noNA_pos[pos_i, ] > 0)
  pos_combo_str <- paste(sort(symptoms_clean[pos_symptoms_present_i]), collapse ='; ')
  pos_symptom_combos <- c(pos_symptom_combos, pos_combo_str)
}

# Restrict to symptom combinations observed at least 10 times
# (across both negative and positive cases).
all_combos_tally <- table(c(neg_symptom_combos, pos_symptom_combos))
all_combos_tally_atleast10 <- all_combos_tally[which(all_combos_tally >= 10)]

neg_symptom_combos <- neg_symptom_combos[which(neg_symptom_combos %in% names(all_combos_tally_atleast10))]
pos_symptom_combos <- pos_symptom_combos[which(pos_symptom_combos %in% names(all_combos_tally_atleast10))]

raw_OR_out <- list()

for (combo in names(all_combos_tally_atleast10)) {
 neg_w_symptom <- length(which(neg_symptom_combos == combo))
 neg_wo_symptom <- length(which(neg_symptom_combos != combo))
 
 pos_w_symptom <- length(which(pos_symptom_combos == combo))
 pos_wo_symptom <- length(which(pos_symptom_combos != combo))
 
 tally_tab <- matrix(c(pos_w_symptom, pos_wo_symptom,
                       neg_w_symptom, neg_wo_symptom),
                     nrow = 2)
 
 fisher_out <- exact2x2(tally_tab)

 raw_OR_out[[combo]] <- data.frame(symptom_combo=combo,
                                     pos_w_symptom=pos_w_symptom,
                                     pos_wo_symptom=pos_wo_symptom,
                                     neg_w_symptom=neg_w_symptom,
                                     neg_wo_symptom=neg_wo_symptom,
                                     pos_w_vs_wo=pos_w_symptom/pos_wo_symptom,
                                     neg_w_vs_wo=neg_w_symptom/neg_wo_symptom,
                                     OR=fisher_out$estimate,
                                     OR_95_lower=fisher_out$conf.int[[1]],
                                     OR_95_higher=fisher_out$conf.int[[2]],
                                     P=fisher_out$p.value)
}

OR_out <- do.call(rbind, raw_OR_out)

OR_out <- OR_out[order(OR_out$OR, decreasing = FALSE), ]

OR_out$FDR <- p.adjust(OR_out$P, 'BH')

# Note that no hits were significant at the stringent cut-off of FDR < 0.05.
length(which(OR_out$FDR < 0.05))

# Restricted to the more exploratory cut-off of FDR < 0.2.
OR_out <- OR_out[which(OR_out$FDR < 0.2), ]

OR_out$symptom_combo[which(OR_out$symptom_combo == "Cold; Dry cough; Fatigue; Fever; Headache; Myalgia; Smell loss; Taste loss")] <- "Cold; Dry cough; Fatigue; Fever;\nHeadache; Myalgia; Smell loss;\nTaste loss"
OR_out$symptom_combo[which(OR_out$symptom_combo == "Cold; Dry cough; Fatigue; Headache; Myalgia; Sore throat")] <- "Cold; Dry cough; Fatigue;\nHeadache; Myalgia; Sore throat"

OR_out$symptom_combo <- sapply(OR_out$symptom_combo, function(x) { paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x)))) })
OR_out$symptom_combo <- factor(OR_out$symptom_combo, levels = OR_out$symptom_combo)

OR_summary_plot <- ggplot(data = OR_out,
                          aes(x = OR, y = symptom_combo)) +
  geom_point() +
  geom_errorbar(xmin=OR_out$OR_95_lower, xmax=OR_out$OR_95_higher) +
  theme_bw() +
  xlab("Odd's ratio (enrichment in positive vs. negative cases)") +
  ylab("Significant symptom combinations (FDR < 0.2)") +
  geom_vline(xintercept = 1, lty = 2, colour='grey65') +
  xlim(0.8, 30) +
  ggtitle("Symptom combination enrichments\n(restricted to individuals with at least two symptoms)") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/display_items/OR_combo_summary_plot.pdf',
       plot = OR_summary_plot,
       device = 'pdf',
       width = 7,
       height = 5,
       units = 'in')

write.table(file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/display_data/symptom_OR_summary.tsv',
            x = OR_out,
            col.names = TRUE,
            row.names = FALSE,
            sep = '\t',
            quote = FALSE)
