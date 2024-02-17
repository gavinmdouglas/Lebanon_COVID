rm(list = ls(all.names = TRUE))

# Get a breakdown of how enriched each symptom is among positive cases vs. negative cases,
# and then plot out the odd's ratios in a simple format.
# Run this analysis for (1) all individuals and (2) restricted to individuals with at least one symptom.

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

colnames(covid_data_symptoms_noNA) <- gsub("\\.", " ", colnames(covid_data_symptoms_noNA))
colnames(covid_data_symptoms_noNA) <- str_to_sentence(colnames(covid_data_symptoms_noNA))

PCR_result <- covid_data[rownames(covid_data_symptoms_noNA), 'PCR.result.clean']

covid_data_symptoms_noNA_neg <- covid_data_symptoms_noNA[which(PCR_result == 'Negative'), ]
covid_data_symptoms_noNA_pos <- covid_data_symptoms_noNA[which(PCR_result == 'Positive'), ]

covid_data_symptoms_noNA_neg_atleast1 <- covid_data_symptoms_noNA_neg[which(rowSums(covid_data_symptoms_noNA_neg) > 0), ]
covid_data_symptoms_noNA_pos_atleast1 <- covid_data_symptoms_noNA_pos[which(rowSums(covid_data_symptoms_noNA_pos) > 0), ]

raw_OR_out <- list()

for (subset_type in c('Full dataset\n(including those without symptoms)', 'Individuals with at\nleast one symptom')) {
  
  if (subset_type == 'Full dataset\n(including those without symptoms)') {
    pos_tab <- covid_data_symptoms_noNA_pos
    neg_tab <- covid_data_symptoms_noNA_neg
  } else if (subset_type == 'Individuals with at\nleast one symptom') {
    pos_tab <- covid_data_symptoms_noNA_pos_atleast1
    neg_tab <- covid_data_symptoms_noNA_neg_atleast1
  } else {
    stop('Error')
  }
  
  for (symptom in colnames(neg_tab)) {
   neg_w_symptom <- length(which(neg_tab[, symptom] > 0))
   neg_wo_symptom <- length(which(neg_tab[, symptom] == 0)) 
   
   pos_w_symptom <- length(which(pos_tab[, symptom] > 0))
   pos_wo_symptom <- length(which(pos_tab[, symptom] == 0))
   
   tally_tab <- matrix(c(pos_w_symptom, pos_wo_symptom,
                         neg_w_symptom, neg_wo_symptom),
                       nrow = 2)
   
   fisher_out <- exact2x2(tally_tab)
   
   raw_OR_out[[paste(symptom, subset_type)]] <- data.frame(symptom=symptom,
                                                           subset=subset_type,
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
}

OR_out <- do.call(rbind, raw_OR_out)

OR_out <- OR_out[order(OR_out$OR, decreasing = FALSE), ]
OR_out$symptom <- factor(OR_out$symptom, levels = unique(OR_out$symptom))

OR_out$`Test result` <- 'Not sig.'
OR_out$FDR <- p.adjust(OR_out$P, 'BH')
OR_out[which(OR_out$FDR < 0.05), 'Test result'] <- 'Sig.'

OR_out$subset <- factor(OR_out$subset, levels = c('Full dataset\n(including those without symptoms)', 'Individuals with at\nleast one symptom'))

OR_summary_plot <- ggplot(data = OR_out,
                       aes(x = OR, y = symptom, colour = `Test result`)) +
                  geom_point() +
                  geom_errorbar(xmin=OR_out$OR_95_lower, xmax=OR_out$OR_95_higher) +
                  xlim(c(0.75, 3.5)) +
                  theme_bw() +
                  xlab("Odd's ratio (enrichment in positive vs. negative cases)") +
                  ylab("Symptom") +
                  geom_vline(xintercept = 1, lty = 2) +
  facet_wrap(subset ~ .) +
  scale_colour_manual(values=c('grey80', 'black'))

ggsave(filename = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/display_items/OR_summary_plot.pdf',
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
