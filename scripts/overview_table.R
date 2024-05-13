rm(list = ls(all.names = TRUE))

# Generate table used for participant demographic and characteristic breakdowns.
# Breakdowns by total and positive-only participants separately.

breakdown_col <- function(in_df, focal_col) {

  tallies <- table(in_df[, focal_col])
  num_missing <- length(which(is.na(in_df[, focal_col])))
  
  pos_df <- in_df[which(in_df$PCR.result.clean == "Positive"), ]
  pos_tallies <- table(pos_df[, focal_col])
  pos_num_missing <- length(which(is.na(pos_df[, focal_col])))
  
  pos_tallies_clean <- numeric()
  for (level in names(tallies)) {
    if (level %in% names(pos_tallies)) {
      pos_tallies_clean <- c(pos_tallies_clean, pos_tallies[level])
    } else {
      pos_tallies_clean <- c(pos_tallies_clean, 0)
    }
  }
  
  out_df <- data.frame(variable=focal_col,
                       categories = c(names(tallies), "Missing"),
                       total_counts = c(tallies, num_missing),
                       pos_counts = c(pos_tallies_clean, pos_num_missing))
  
  out_df$total_percent <- (out_df$total_counts / sum(out_df$total_counts)) * 100
  out_df$pos_percent_of_allpos <- (out_df$pos_counts / sum(out_df$pos_counts)) * 100
  
  out_df$pos_percent_of_level <- (out_df$pos_counts / out_df$total_counts) * 100
  
  if (length(is.na(out_df$pos_percent_of_level)) > 0) {
    out_df$pos_percent_of_level[which(is.na(out_df$pos_percent_of_level))] <- 0
  }
  
  out_df$total_summary <- paste(sprintf("%.1f", out_df$total_percent), "% (", scales::label_comma()(out_df$total_counts), ")", sep = "")
  out_df$pos_by_level_summary <- paste(sprintf("%.1f", out_df$pos_percent_of_level), "% (", scales::label_comma()(out_df$pos_counts), ")", sep = "")

  return(out_df)
}

covid_data <- read.table(file = '/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/COVID_data_filt.csv.gz',
                         header = TRUE,
                         sep = ',',
                         stringsAsFactors = FALSE)

raw_tab <- list()

categories_of_interest <- c("Governorate", "Gender", "blood.group",
                            "Social.Distancing", "Public.Transportation",
                            "Crowded.Area", "Trip", "Contact.Traveler",
                            "Contact.CoV", "Hospital.Visit")

for (category in categories_of_interest) {
  raw_tab[[category]] <- breakdown_col(in_df = covid_data,
                                       focal_col = category)
}

combined_tab <- do.call(rbind, raw_tab)

write.table(x = combined_tab,
            file = "/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/display_data/overview_table.tsv",
            sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)
