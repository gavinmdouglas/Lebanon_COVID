rm(list = ls(all.names = TRUE))

# Prep tables of positivity by region prior to plotting.

covid_data <- read.table(file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/COVID_data_filt.csv.gz',
                         header = TRUE,
                         sep = ',',
                         stringsAsFactors = FALSE)

# First, by Governorate
all_Governorates <- sort(unique(covid_data[, 'Governorate']))
  
Governorate_info <- data.frame(matrix(0, nrow = length(all_Governorates), ncol = 5))
colnames(Governorate_info) <- c("Governorate", "num_pos", "num_neg", "num_unclear", "percent_pos")
rownames(Governorate_info) <- all_Governorates
  
for (Gov in all_Governorates) {
    Governorate_info[Gov, "Governorate"] <- Gov
    Governorate_info[Gov, "num_pos"] <- length(which(covid_data$Governorate == Gov & covid_data$PCR.result.clean == 'Positive'))
    Governorate_info[Gov, "num_neg"] <- length(which(covid_data$Governorate == Gov & covid_data$PCR.result.clean == 'Negative'))
    Governorate_info[Gov, "num_unclear"] <- length(which(covid_data$Governorate == Gov & covid_data$PCR.result.clean == 'Unclear'))
    Governorate_info[Gov, "percent_pos"] <- (Governorate_info[Gov, "num_pos"]  / (Governorate_info[Gov, "num_pos"]  + Governorate_info[Gov, "num_neg"])) * 100
}


# Then by Caza.
# First, collapse 'Akkar El Chamali' to 'Akkar'.
covid_data[which(covid_data$Caza == 'Akkar  El Chamali'), 'Caza'] <- "Akkar"

all_Cazas <- sort(unique(covid_data[, 'Caza']))

Caza_info <- data.frame(matrix(0, nrow = length(all_Cazas), ncol = 5))
colnames(Caza_info) <- c("Caza", "num_pos", "num_neg", "num_unclear", "percent_pos")
rownames(Caza_info) <- all_Cazas

for (Gov in all_Cazas) {
  Caza_info[Gov, "Caza"] <- Gov
  Caza_info[Gov, "num_pos"] <- length(which(covid_data$Caza == Gov & covid_data$PCR.result.clean == 'Positive'))
  Caza_info[Gov, "num_neg"] <- length(which(covid_data$Caza == Gov & covid_data$PCR.result.clean == 'Negative'))
  Caza_info[Gov, "num_unclear"] <- length(which(covid_data$Caza == Gov & covid_data$PCR.result.clean == 'Unclear'))
  Caza_info[Gov, "percent_pos"] <- (Caza_info[Gov, "num_pos"]  / (Caza_info[Gov, "num_pos"]  + Caza_info[Gov, "num_neg"])) * 100
}


# Last, by town.
all_Towns <- sort(unique(covid_data$Town))

Town_info <- data.frame(matrix(0, nrow = length(all_Towns), ncol = 5))
colnames(Town_info) <- c("Town", "num_pos", "num_neg", "num_unclear", "percent_pos")
rownames(Town_info) <- all_Towns

for (Gov in all_Towns) {
  Town_info[Gov, "Town"] <- Gov
  Town_info[Gov, "num_pos"] <- length(which(covid_data$Town == Gov & covid_data$PCR.result.clean == 'Positive'))
  Town_info[Gov, "num_neg"] <- length(which(covid_data$Town == Gov & covid_data$PCR.result.clean == 'Negative'))
  Town_info[Gov, "num_unclear"] <- length(which(covid_data$Town == Gov & covid_data$PCR.result.clean == 'Unclear'))
  Town_info[Gov, "percent_pos"] <- (Town_info[Gov, "num_pos"]  / (Town_info[Gov, "num_pos"]  + Town_info[Gov, "num_neg"])) * 100
}


# Write out tables.
write.table(file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/display_data/Governorate_positivity.tsv',
            x = Governorate_info,
            col.names = TRUE,
            row.names = FALSE,
            sep = '\t',
            quote = FALSE)

write.table(file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/display_data/Caza_positivity.tsv',
            x = Caza_info,
            col.names = TRUE,
            row.names = FALSE,
            sep = '\t',
            quote = FALSE)

write.table(file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/display_data/Town_positivity.tsv',
            x = Town_info,
            col.names = TRUE,
            row.names = FALSE,
            sep = '\t',
            quote = FALSE)
