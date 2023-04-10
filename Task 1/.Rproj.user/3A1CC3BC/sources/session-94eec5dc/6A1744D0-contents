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


# convert to tidy