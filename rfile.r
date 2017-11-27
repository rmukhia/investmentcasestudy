library(dplyr)
library(stringr)

# Load companies data
companies <- read.delim('input/companies.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE)

# Load rounds2 data
rounds2 <- read.csv('input/rounds2.csv', stringsAsFactors = FALSE)

# Start of checkpoint 1

# The company permalink have mismatching unicode characters, this can be cleaned
# by replacing the unicode character with '?'
# In companies the permalink is in title case whereas in rounds2 the company_permalink
# is either uppercase or lowercase. Convert both permalink to uppercase so that they are
# similar. 

companies$permalink <- iconv(companies$permalink, "", "ASCII", sub="?")
rounds2$company_permalink <- iconv(rounds2$company_permalink, "", "ASCII", sub="?")

companies$permalink <- sapply(companies$permalink, toupper)
rounds2$company_permalink <- sapply(rounds2$company_permalink, toupper)

distinct_companies <- distinct(companies, permalink, .keep_all = TRUE)
distinct_rounds2 <- distinct(rounds2, company_permalink, .keep_all = TRUE)

# How many unique companies are present in rounds2?
sprintf("Unique companies present in rounds2: %d", nrow(distinct_rounds2))

# How many unique companies are present in the companies file?
sprintf("Unique companies present in companies: %d", nrow(distinct_companies))

# Are there any companies in the rounds2 file which are not present in companies ? Answer Y/N.
no_difference <- identical(setdiff(distinct_rounds2$company_permalink, distinct_companies$permalink), character(0))
sprintf("Any companies in the rounds2 file which are not present in companies? %s", ifelse(no_difference, 'N', 'Y'))

# Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.
# Name the merged frame master_frame. How many observations are present in master_frame ?
master_frame <- merge(rounds2, companies, by.x = "company_permalink", by.y = "permalink")

sprintf("Number of observations in master_frame: %d", nrow(master_frame))

# End of checkpoint 1