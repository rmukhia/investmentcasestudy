library(dplyr)
library(stringr)
library(tidyr)

# Load companies data, using NA for blank strings
companies <- read.delim('input/companies.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE, na.strings=c("","NA"))

# Load rounds2 data, using NA for blank strings
rounds2 <- read.csv('input/rounds2.csv', stringsAsFactors = FALSE, na.strings=c("","NA"))

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

# Clean global environment
rm(distinct_companies)
rm(distinct_rounds2)
rm(no_difference)

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

most_suitable_funding <- filtered_avg_funding_per_type[[most_suitable_index, "funding_round_type"]];

sprintf("Most suitable investment type: %s", most_suitable_funding)

# Clean Global Environment
rm(avg_funding_per_type)
rm(filtered_avg_funding_per_type)
rm(most_suitable_index)

# End of checkpoint 2

# Start of checkpoint 3

# Filter master_frame and get only venture investments
venture_type <- filter(master_frame, funding_round_type == most_suitable_funding)

# Group by country code
top9 <- group_by(venture_type, country_code)
# Summarise and find the total funding per country
top9 <- summarise(top9, raised_amount_usd.total = sum(raised_amount_usd, na.rm = TRUE))
# Sort by descending order
top9 <- arrange(top9, desc(raised_amount_usd.total))

# Remove NA from top9

top9 <- na.omit(top9)
top9 <- top9[1:9,]

# The top three english speaking countries are
# United States
# Great Britian
# India

# Clean Global Environment
rm(venture_type)

# End of checkpoint 3

# Start of checkpoint 4

# Break category_list column into primary_sector by using '|' seperator
sector_frame <- separate(master_frame, category_list, 'primary_sector', sep = '[|]', remove= FALSE, extra = 'drop')

# Load mapping.csv, using NA for blank strings
sector_mapping <- read.csv("input/mapping.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))

# Convert from wide to long format
sector_mapping <- gather(sector_mapping, main_sector, val, 2:10)

# Remove artifacts from the previous conversion
sector_mapping <- sector_mapping[!(sector_mapping$val == 0),][1:2]

# Remove NA rows
sector_mapping <- na.omit(sector_mapping)

# Merge sector frame and sector mapping using primary_sector and category_list respectively
sector_frame <- merge(sector_frame, sector_mapping, by.x = "primary_sector", by.y ="category_list", all.x = TRUE )

# Clean Gobal Environment
rm(sector_mapping)

# End of checkpoint 4
# Start of checkpoint 5

# Custom filter condition based on country_code, funding_round_type and raised_amount_usd
is_investable <- function (country_code, expected_country_code, funding_round_type, raised_amount_usd) {
  condition1 <- country_code == expected_country_code;
  condition2 <- funding_round_type == most_suitable_funding;
  condition3 <- raised_amount_usd >= 5000000 & raised_amount_usd < 15000000
  return (condition1 & condition2 & condition3)
}

# Create three new data frames by filtering with country_code and funding_round_type
D1 <- filter(sector_frame, is_investable(country_code, "USA", funding_round_type ,raised_amount_usd))
D2 <- filter(sector_frame, is_investable(country_code, "GBR", funding_round_type ,raised_amount_usd))
D3 <- filter(sector_frame, is_investable(country_code, "IND", funding_round_type ,raised_amount_usd))

# Get the total count and total amount of investment in each sector
getTotalInvestmentsSorted <- function(data) {
  total <- summarise(group_by(data, main_sector), total.count = n(), total.amount = sum(raised_amount_usd, na.rm = TRUE))
  total <- arrange(total, desc(total.count))
  return(total);
}

D1_summary <- getTotalInvestmentsSorted(D1)
D2_summary <- getTotalInvestmentsSorted(D2)
D3_summary <- getTotalInvestmentsSorted(D3)

# Merge total investment count and total investment amount per sector
D1 <- merge(D1, D1_summary, by = "main_sector")
D2 <- merge(D2, D2_summary, by = "main_sector")
D3 <- merge(D3, D3_summary, by = "main_sector")

# Top sector (based on count of investments)
sprintf("Total number of investments (count): (US) %d (GBR) %d (IN) %d",
        sum(D1_summary$total.count), sum(D2_summary$total.count), sum(D3_summary$total.count))

# Total amount of investment (USD)
sprintf("Total number of investments (count): (US) $%f (GBR) $%f (IN) $%f",
        sum(D1_summary$total.amount), sum(D2_summary$total.amount), sum(D3_summary$total.amount))

top_sectors <- c(D1_summary$main_sector[1], D2_summary$main_sector[1], D3_summary$main_sector[1])
# Top sector (based on count of investments)
sprintf("Top sector (based on count of investments): (US) %s (GBR) %s (IN) %s",
        top_sectors[1], top_sectors[2], top_sectors[3])

second_best_sector <- c(D1_summary$main_sector[2], D2_summary$main_sector[2], D3_summary$main_sector[2])
# Second-best sector (based on count of investments)
sprintf("Second-best sector (based on count of investments): (US) %s (GBR) %s (IN) %s",
        second_best_sector[1], second_best_sector[2], second_best_sector[3])

# Third-best sector (based on count of investments)
sprintf("Third-best sector (based on count of investments): (US) %s (GBR) %s (IN) %s",
        D1_summary$main_sector[3], D2_summary$main_sector[3], D3_summary$main_sector[3])

# Number of investments in the top sector (refer to point 3)
sprintf("Number of investments in the top sector (refer to point 3): (US) %d (GBR) %d (IN) %d",
        D1_summary$total.count[1], D2_summary$total.count[1], D3_summary$total.count[1])

# Number of investments in the second-best sector (refer to point 4)
sprintf("Number of investments in the second-best sector (refer to point 4): (US) %d (GBR) %d (IN) %d",
        D1_summary$total.count[2], D2_summary$total.count[2], D3_summary$total.count[2])

# Number of investments in the third-best sector (refer to point 5)
sprintf("Number of investments in the third-best sector (refer to point 5): (US) %d (GBR) %d (IN) %d",
        D1_summary$total.count[3], D2_summary$total.count[3], D3_summary$total.count[3])

# Get the name of the company with the highest investment amount in sector
getTopCompany <- function(data, sector) {
  # Filter data for sector
  sector_data <- filter(data, main_sector == sector)
  # Sum of investment per company
  data_summary <- summarise(group_by(sector_data, company_permalink), raised_amount_usd.total = sum(raised_amount_usd))
  # The permalink of the company which received the highest investment
  top_company_permalink <- data_summary[[which.max(data_summary$raised_amount_usd.total), 1]]
  # Find the company name given the permalink 
  top_company_name <- unique(data[data$company_permalink == top_company_permalink, 'name'])
  return (top_company_name)
}
                                              
# For the top sector count-wise (point 3), which company received the highest investment?
sprintf("For the top sector count-wise (point 3), which company received the highest investment? (US) %s (GBR) %s (IN) %s",
        getTopCompany(D1, top_sectors[1]), getTopCompany(D2, top_sectors[2]), getTopCompany(D3, top_sectors[3]))

# For the second-best sector count-wise (point 4), which company received the highest investment?
sprintf("For the second-best sector count-wise (point 4), which company received the highest investment? (US) %s (GBR) %s (IN) %s",
       getTopCompany(D1, second_best_sector[1]), getTopCompany(D2, second_best_sector[2]), getTopCompany(D3, second_best_sector[3]))

# Clean Global Environment
rm(D1_summary)
rm(D2_summary)
rm(D3_summary)
rm(top_sectors)
rm(second_best_sector)

# End of checkpoint 5
