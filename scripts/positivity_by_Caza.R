rm(list = ls(all.names = TRUE))

library(ggplot2)
library(maptools)

# Prep tables of positivity by region prior to plotting.
Caza_info <- read.table(file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/display_data/Caza_positivity.tsv',
                         header = TRUE,
                         sep = '\t',
                         stringsAsFactors = FALSE)
rownames(Caza_info) <- Caza_info$Caza

shapefile2 <- sf::st_read("~/Drive/mcgill/mira/covid/Lebanon_shapefiles/lbn_adm_cdr_20200810/lbn_admbnda_adm2_cdr_20200810.shp")

# Add count ratio as character.
Caza_info$num_pos_or_neg <- Caza_info$num_pos + Caza_info$num_neg
Caza_info$percent_ratio_char <- paste(as.character(Caza_info$num_pos), Caza_info$num_pos_or_neg, sep = '/')
missing_Caza <- setdiff(shapefile2$admin2Name, rownames(Caza_info))
Caza_info[missing_Caza, 'percent_ratio_char'] <- 'NA'
Caza_info[missing_Caza, 'Caza'] <- missing_Caza
Caza_info$percent_ratio_char <- paste(Caza_info$Caza, Caza_info$percent_ratio_char, sep = '\n')

shapefile2$positivity_rate <- Caza_info[shapefile2$admin2Name, 'percent_pos']
shapefile2$percent_ratio_char <- Caza_info[shapefile2$admin2Name, 'percent_ratio_char']

Caza_plot <- ggplot(data = shapefile2) +
                geom_sf(aes(fill = positivity_rate), colour = "black") +
                scale_fill_gradient(na.value = 'grey85', low="white", high="coral2", name = "Percent\npositive", limits=c(0, 20)) +
                geom_sf_text(aes(label = percent_ratio_char), colour = "black", na.rm = FALSE, check_overlap = TRUE, fontface = "bold") +
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

ggsave(filename = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/display_items/positivity_by_Caza.pdf',
       plot = Caza_plot,
       device = 'pdf',
       width = 8,
       height = 8,
       units = 'in')
