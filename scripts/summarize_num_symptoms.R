rm(list = ls(all.names = TRUE))

# Get summary statistics of the number of symptoms by positivity.
num_symptoms <- read.table('/Users/gavin/Drive/mcgill/mira/covid/Lebanon_COVID/data/display_data/num_symptoms_by_result.tsv',
                           header = TRUE, sep = '\t', stringsAsFactors = FALSE)

# Quickly build raw distributions.
pos_num <- numeric()
neg_num <- numeric()

for (i in 1:nrow(num_symptoms)) {
 
  result <- num_symptoms[i, 'Result']
  num_sym <- num_symptoms[i, 'Num_symptoms']
  n <- num_symptoms[i, 'n']
  
  if (result == 'Negative') {
    neg_num <- c(neg_num, rep(num_sym, n))
  } else if (result == 'Positive') {
    pos_num <- c(pos_num, rep(num_sym, n))
  } else {
    stop('Error') 
  }
   
}

# Then run statistical tests and get summary stats.

# KS test result:
ks.test(pos_num, neg_num)

# Or alternatively, a rank-based comparison:
wilcox.test(pos_num, neg_num)

# Clearly these are significantly different - probably the most useful thing is
# to just compare the means and standard deviations.
mean(pos_num)
sd(pos_num)

mean(neg_num)
sd(neg_num)
