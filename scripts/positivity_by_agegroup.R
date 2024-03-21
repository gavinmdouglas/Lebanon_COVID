rm(list = ls(all.names = TRUE))

library(ggplot2)

covid_data <- read.table(file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/COVID_data_filt.csv.gz',
                         header = TRUE,
                         sep = ',',
                         stringsAsFactors = FALSE)

# Define age groups.
covid_data$Age.Group <- NA
covid_data$Age.Group[which(covid_data$Age >= 5 & covid_data$Age <= 10)] <- '5-10'
covid_data$Age.Group[which(covid_data$Age >= 11 & covid_data$Age <= 17)] <- '11-17'
covid_data$Age.Group[which(covid_data$Age >= 18 & covid_data$Age <= 29)] <- '18-29'
covid_data$Age.Group[which(covid_data$Age >= 30 & covid_data$Age <= 39)] <- '30-39'
covid_data$Age.Group[which(covid_data$Age >= 40 & covid_data$Age <= 49)] <- '40-49'
covid_data$Age.Group[which(covid_data$Age >= 50 & covid_data$Age <= 59)] <- '50-59'
covid_data$Age.Group[which(covid_data$Age >= 60 & covid_data$Age <= 69)] <- '60-69'
covid_data$Age.Group[which(covid_data$Age >= 70 & covid_data$Age <= 79)] <- '70-79'
covid_data$Age.Group[which(covid_data$Age >= 80 & covid_data$Age <= 89)] <- '80-89'

# Decided to drop this age group, because there were only 11 samples, so it's misleading.
# covid_data$Age.Group[which(covid_data$Age >= 90 & covid_data$Age <= 100)] <- '90-100'
covid_data <- covid_data[which(covid_data$Age <= 89), ]

group_order <- c("5-10", "11-17", "18-29",
                 "30-39", "40-49", "50-59",
                 "60-69", "70-79", "80-89")

age_group_breakdown <- data.frame(age_group = group_order,
                                  Negative = 0,
                                  Positive = 0,
                                  Unclear = 0) 

rownames(age_group_breakdown) <- age_group_breakdown$age_group

age_group_breakdown$age_group <- factor(age_group_breakdown$age_group,
                                        levels = age_group_breakdown$age_group)

for (i in 1:nrow(covid_data)) {
  row_group <- covid_data[i, 'Age.Group']
  row_result <- covid_data[i, 'PCR.result.clean']
  age_group_breakdown[row_group, row_result] <- age_group_breakdown[row_group, row_result] + 1
}

age_group_breakdown$Neg_and_Pos <- age_group_breakdown$Negative + age_group_breakdown$Positive
age_group_breakdown$percent_pos <- (age_group_breakdown$Positive / age_group_breakdown$Neg_and_Pos) * 100

positivity_versus_agegroups <- ggplot(data=age_group_breakdown, aes(x=age_group, y=percent_pos)) +
  geom_bar(stat = "identity", fill = "slategrey", width = 0.7, position = position_dodge()) + 
  geom_text(aes(label=Positive), position=position_dodge(width=0.6), vjust=-0.8, size = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10, face= "bold")) +
  theme(axis.text.y = element_text( size = 10,face= "bold"))+
  theme(axis.title.x = element_text(size = 10,face= "bold"))+
  theme(axis.title.y = element_text(size = 10,face= "bold"))+
  theme(text = element_text(size = 10,face= "bold"))+
  xlab("Age Groups") +
  ylab("Percentage SARS-CoV-2 positivity\n")

ggsave(filename = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/display_items/positivity_by_agegroup.pdf',
       plot = positivity_versus_agegroups,
       device = 'pdf',
       width = 6,
       height = 5,
       units = 'in')

write.table(file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/display_data/positivity_by_agegroup.tsv',
            x = age_group_breakdown,
            col.names = TRUE,
            row.names = FALSE,
            sep = '\t',
            quote = FALSE)
