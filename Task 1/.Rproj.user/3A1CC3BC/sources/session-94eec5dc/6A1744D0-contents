# Load libraries
library(tidyverse)
library(here)
library(janitor)
library(readr)
library(dplyr)

--------------------------------------------------------------------------------

# Load Data

decathlon_raw <- read_rds(here("raw_data/decathlon.rds"))

# Inspect Data
decathlon_raw
glimpse(decathlon_raw)
head(decathlon_raw)
summary(decathlon_raw)

# There are 13 variables - 12 are numeric and 1 is string.
# There are 41 observations

--------------------------------------------------------------------------------

# Cleaning Data
  
# Change row names to column called "athlete"
  
decathlon_clean <- rownames_to_column(decathlon_raw, "athlete")

# Change column headers to snake_case using janitor

decathlon_clean <- clean_names(decathlon_clean)


# Tidy data: convert to long format
decathlon_clean_long <- pivot_longer(decathlon_clean,
             cols = "x100m":"x1500m",
             names_to = "sport",
             values_to = "score_metres_seconds")



# Tidy data: rearrange columns and sort by athlete

decathlon_clean_long <- decathlon_clean_long %>% 
  relocate(sport, .after = athlete) %>% 
  relocate(score_metres_seconds, .after = sport) %>% 
  relocate(competition, .after = sport)

decathlon_clean_long <- decathlon_clean_long %>% 
  arrange(athlete, sport,competition)

# Tidy data: check column types
head(decathlon_clean_long)
# The "competition" column is a factor type, will not convert it currently as it may be useful.

# Check for NA values
  
  colSums(is.na(decathlon_clean_long))
# There are no NA values   
  
# make all data lower case - it appears that athletes are listed both uppercase and lowercase
 
   decathlon_clean_long_lower <- decathlon_clean_long %>% 
    mutate_if(is.character, str_to_lower)

# Write cleaned data to csv

  write_csv(decathlon_clean_long_lower, "clean_data/decathlon_cleaned.csv")

