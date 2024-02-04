rm(list = ls(all.names = TRUE))

library(ggplot2)
library(scales)
library(reshape2)

covid_data <- read.table(file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/COVID_data_filt.csv.gz',
                         header = TRUE,
                         sep = ',',
                         stringsAsFactors = FALSE)

covid_data$clean_date <- as.Date(covid_data$Test.Taken.Date, format="%B %d, %Y")
covid_data$month_year <- format(as.Date(covid_data$clean_date), format = "%m/%Y")

covid_data_nounclear <- covid_data[which(covid_data$PCR.result.clean != "Unclear"), ]

covid_data_nounclear_noNA <- covid_data_nounclear[which(! is.na(covid_data_nounclear$month_year)), ]

covid_data_nounclear_noNA$month_year <- factor(covid_data_nounclear_noNA$month_year,
                                               levels = c("04/2020", "05/2020", "06/2020",
                                                          "07/2020", "08/2020", "09/2020",
                                                          "10/2020", "11/2020", "12/2020",
                                                          "01/2021", "02/2021", "03/2021"))

participant_results_by_month <- ggplot(data = covid_data_nounclear_noNA, aes(x = month_year, fill = PCR.result.clean)) +
                                      geom_bar() +
                                      scale_fill_discrete(type=c("grey", "red"), name = "SARS-CoV-2")+  
                                      theme_bw() +
                                      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
                                      xlab("") +
                                      ylab("Number of participants\n")

# Prep a table of all plotted values.
results_tab <- data.frame(matrix(NA, ncol = 3, nrow = length(levels(covid_data_nounclear_noNA$month_year))))
colnames(results_tab) <- c('Negative_count', 'Positive_count', 'Positive_percent')
rownames(results_tab) <- levels(covid_data_nounclear_noNA$month_year)

for (month_year in levels(covid_data_nounclear_noNA$month_year)) {
  month_year_pcr <- covid_data_nounclear_noNA[which(covid_data_nounclear_noNA$month_year == month_year), 'PCR.result.clean']
  results_tab[month_year, 'Negative_count'] <- length(which(month_year_pcr == 'Negative'))
  results_tab[month_year, 'Positive_count'] <- length(which(month_year_pcr == 'Positive'))
  results_tab[month_year, 'Positive_percent'] <- (results_tab[month_year, 'Positive_count'] / (results_tab[month_year, 'Positive_count'] + results_tab[month_year, 'Negative_count'])) * 100
}

# This is a quick hack to add the positivity percentage to the bars.
date_num <- 1
for (date in rownames(results_tab)) {
  month_total <- results_tab[date, 'Positive_count'] + results_tab[date, 'Negative_count']
  participant_results_by_month <- participant_results_by_month + annotate(geom="text",
                                                                          x=date_num,
                                                                          y=month_total + 45,
                                                                          label=paste0(sprintf("%.1f", results_tab[date, 'Positive_percent']), "%"),
                                                                          colour = "red")
  date_num <- date_num + 1
}

ggsave(filename = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/display_items/positivity_by_month.pdf',
       plot = participant_results_by_month,
       device = 'pdf',
       width = 8,
       height = 6,
       units = 'in')

results_tab$Month_year <- rownames(results_tab)
results_tab <- results_tab[, c('Month_year', 'Negative_count', 'Positive_count', 'Positive_percent')]

write.table(file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/display_data/positivity_by_month.tsv',
            x = results_tab,
            col.names = TRUE,
            row.names = FALSE,
            sep = '\t',
            quote = FALSE)
