rm(list = ls(all.names = TRUE))

library(ggplot2)
library(ggbeeswarm)

covid_data <- read.table(file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/COVID_data_filt.csv.gz',
                         header = TRUE,
                         sep = ',',
                         stringsAsFactors = FALSE)

covid_data$dummy <- "All samples"

age_boxplot <- ggplot(data=covid_data, aes(y= Age, x=dummy)) +
  geom_quasirandom(colour = "orange") +
  geom_boxplot(outlier.shape = NA, alpha = 0.8, fill = "grey") +
  xlab("") +
  ylab("Age\n") +
  theme_bw()

ggsave(filename = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/display_items/age_boxplot.pdf',
       plot = age_boxplot,
       device = 'pdf',
       width = 4,
       height=4,
       units = 'in')
