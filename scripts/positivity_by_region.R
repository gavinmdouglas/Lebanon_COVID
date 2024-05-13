rm(list = ls(all.names = TRUE))

library(ggplot2)
library(maptools)

covid_data <- read.table(file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/COVID_data_filt.csv.gz',
                         header = TRUE,
                         sep = ',',
                         stringsAsFactors = FALSE)

# Re-name governorates as needed to match shapefiles (and fix lowercase typo 'aakaar')
covid_data[which(covid_data$Governorate == 'aakkar'), 'Governorate'] <- 'Akkar'
covid_data[which(covid_data$Governorate == 'Aakkar'), 'Governorate'] <- 'Akkar'
covid_data[which(covid_data$Governorate == 'Baalbek_El-Hermel'), 'Governorate'] <- 'Baalbek-El Hermel'
covid_data[which(covid_data$Governorate == 'Mount-Lebanon'), 'Governorate'] <- 'Mount Lebanon'
covid_data[which(covid_data$Governorate == 'Beqaa'), 'Governorate'] <- 'Bekaa'
covid_data[which(covid_data$Governorate == 'Nabatieh'), 'Governorate'] <- 'El Nabatieh'

all_Governorates <- sort(unique(covid_data[, 'Governorate']))
  
Governorate_info <- data.frame(matrix(0, nrow = length(all_Governorates), ncol = 5))
colnames(Governorate_info) <- c("Governorate", "num_pos", "num_neg", "num_unclear", "pos_rate")
rownames(Governorate_info) <- all_Governorates
  
for (Gov in all_Governorates) {
    Governorate_info[Gov, "Governorate"] <- Gov
    Governorate_info[Gov, "num_pos"] <- length(which(covid_data$Governorate == Gov & covid_data$PCR.result.clean == 'Positive'))
    Governorate_info[Gov, "num_neg"] <- length(which(covid_data$Governorate == Gov & covid_data$PCR.result.clean == 'Negative'))
    Governorate_info[Gov, "num_unclear"] <- length(which(covid_data$Governorate == Gov & covid_data$PCR.result.clean == 'Unclear'))
    Governorate_info[Gov, "percent_pos"] <- Governorate_info[Gov, "num_pos"]  / (Governorate_info[Gov, "num_pos"]  + Governorate_info[Gov, "num_neg"])
}


shapefile1 <- sf::st_read("/Users/gavin/Drive/mcgill/mira/covid/Lebanon_shapefiles/lbn_adm_cdr_20200810/lbn_admbnda_adm1_cdr_20200810.shp")

shapefile1$positivity_rate <- 100 * Governorate_info[shapefile1$admin1Name, "pos_rate"]

ggplot(data = shapefile1) +
  geom_sf(aes(fill = positivity_rate), colour = "black") +
  scale_fill_gradient(low="white", high="dark red", name = "% positive", limits=c(0, 20)) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())


# Caza positivity rate.
Caza_info <- combined_category_info[which(combined_category_info$category == "Caza"), ]

# Again need to standardize some names.
Caza_info[which(Caza_info$level == "Batroun"), "level"] <- "El Batroun"
Caza_info[which(Caza_info$level == "El-Koura"), "level"] <- "El Koura"
Caza_info[which(Caza_info$level == "Keserwan"), "level"] <- "Kesrwane"
Caza_info[which(Caza_info$level == "El-Metn"), "level"] <- "El Meten"
Caza_info[which(Caza_info$level == "Aaley"), "level"] <- "Aley"
Caza_info[which(Caza_info$level == "El-Chouf"), "level"] <- "Chouf"
Caza_info[which(Caza_info$level == "Minieh-Danniyeh"), "level"] <- "El Minieh-Dennie"
Caza_info[which(Caza_info$level == "Rachaiya"), "level"] <- "Rachaya"
Caza_info[which(Caza_info$level == "Aakkar"), "level"] <- "Akkar"
Caza_info[which(Caza_info$level == "West Beqaa"), "level"] <- "West Bekaa"
Caza_info[which(Caza_info$level == "El-Hermel"), "level"] <- "El Hermel"
Caza_info[which(Caza_info$level == "Bent-Jbeil"), "level"] <- "Bent Jbeil"

# Mira pointed out that these are the same:
Caza_info[which(Caza_info$level == "Sidon"), "level"] <- "Saida"

rownames(Caza_info) <- Caza_info$level


# Need to combine Akkar categories again:
Caza_info["Akkar", c("num_pos", "num_neg", "num_unclear")] <- Caza_info["Akkar", c("num_pos", "num_neg", "num_unclear")] + Caza_info["Aakkar el-Chamali", c("num_pos", "num_neg", "num_unclear")]
Caza_info["Akkar", "pos_rate"] <- Caza_info["Akkar", "num_pos"] / (Caza_info["Akkar", "num_pos"] + Caza_info["Akkar", "num_neg"])



shapefile2 <- sf::st_read("~/Google_Drive/postdoc/mira/covid/Lebanon_map_plotting/Lebanon_shapefiles/lbn_adm_cdr_20200810/lbn_admbnda_adm2_cdr_20200810.shp")

shapefile2$positivity_rate <- 100 * Caza_info[shapefile2$admin2Name, "pos_rate"]

ggplot(data = shapefile2) +
  geom_sf(aes(fill = positivity_rate), colour = "black") +
  scale_fill_gradient(low="white", high="dark red", name = "% positive", limits=c(0, 20)) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())


# Town positivity rate

Town_info <- combined_category_info[which(combined_category_info$category == "Town"), ]
rownames(Town_info) <- Town_info$level

Town_info$total <- Town_info$num_pos + Town_info$num_neg
Town_info[which(Town_info$total < 10), "pos_rate"] <- NA

shapefile3 <- sf::st_read("~/Google_Drive/postdoc/mira/covid/Lebanon_map_plotting/Lebanon_shapefiles/lbn_adm_cdr_20200810/lbn_admbnda_adm3_cdr_20200810.shp")

shapefile3$positivity_rate <- 100 * Town_info[shapefile3$admin3Name, "pos_rate"]

ggplot(data = shapefile3) +
  geom_sf(aes(fill = positivity_rate), colour = "black") +
  scale_fill_gradient(low="white", high="dark red", name = "% positive", limits=c(0, 50)) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

