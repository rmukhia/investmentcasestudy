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

# Start of checkpoint 2

avg_funding_per_type <- summarize(group_by(master_frame, funding_round_type),
                                  raised_amount_usd.avg = mean(raised_amount_usd, na.rm=TRUE))

# Average funding amount of venture type
sprintf("Average funding amount of venture type: $%f", 
        avg_funding_per_type[avg_funding_per_type$funding_round_type == 'venture', "raised_amount_usd.avg"])

# Average funding amount of angel type
sprintf("Average funding amount of angel type: $%f", 
        avg_funding_per_type[avg_funding_per_type$funding_round_type == 'angel', "raised_amount_usd.avg"])

# Average funding amount of seed type
sprintf("Average funding amount of seed type: $%f", 
        avg_funding_per_type[avg_funding_per_type$funding_round_type == 'seed', "raised_amount_usd.avg"])

# Average funding amount of private equity type
sprintf("Average funding amount of private equity type: $%f", 
        avg_funding_per_type[avg_funding_per_type$funding_round_type == 'private_equity', "raised_amount_usd.avg"])


# Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, 
# which investment type is the most suitable for them?

filtered_avg_funding_per_type <- filter(
  avg_funding_per_type, funding_round_type == 'venture'| 
  funding_round_type == 'angel' |
  funding_round_type == 'seed' |
  funding_round_type == 'private_equity')

most_suitable_index <- which(filtered_avg_funding_per_type$raised_amount_usd.avg >= 5000000 &
                               filtered_avg_funding_per_type$raised_amount_usd.avg <= 15000000)

sprintf("Most suitable investment type: %s", filtered_avg_funding_per_type[most_suitable_index, "funding_round_type"])

# End of checkpoint 2
