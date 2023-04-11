# Load libraries
library(tidyverse)
library(janitor)
library(here)
library(readxl)
library(waldo)

# Load data
candy_2015 <- read_xlsx(here("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx"))
candy_2016 <- read_xlsx(here("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx"))
candy_2017 <- read_xlsx(here("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx"))

# Look at data 

dim(candy_2015)
# 5630, 124
dim(candy_2016)
# 1259, 123
dim(candy_2017)
# 2460, 120



# clean column names
candy_2015_clean <- clean_names(candy_2015)
candy_2016_clean <- clean_names(candy_2016)
candy_2017_clean <- clean_names(candy_2017)

cols_2015 <- str_sort(colnames(candy_2015_clean_rename))
cols_2016 <- str_sort(colnames(candy_2016_clean_rename))
cols_2017 <- str_sort(colnames(candy_2017_clean_rename))

class(cols_2015)
# There are some 'admin' columns at the start of each data set (age, gender, country, state, "going-out"); 
# recode these so that they are labelled the same in all 3 data sets

#2015: rename for going_out and age

candy_2015_clean_rename <- candy_2015_clean %>% 
  rename("going_out" = "are_you_going_actually_going_trick_or_treating_yourself", 
         "age" = "how_old_are_you") 

# 2016: rename for going_out, age, country, state

candy_2016_clean_rename <- candy_2016_clean %>% 
  rename("going_out" = "are_you_going_actually_going_trick_or_treating_yourself", 
         "age" = "how_old_are_you", 
         "gender" = "your_gender", 
         "country" = "which_country_do_you_live_in", 
         "state" = "which_state_province_county_do_you_live_in")

# 2017: rename for all names to remove "q1_" & then for state
candy_2017_clean_rename <- candy_2017_clean

colnames(candy_2017_clean_rename) <- str_remove_all(colnames(candy_2017_clean), "q[0-9]_")

candy_2017_clean_rename <- candy_2017_clean_rename %>% 
  rename("state" = "state_province_county_etc")

# find a way to compare other columns
adist("bonkers", "bonkers_the_candy")

missing_15_16 <- setdiff(cols_2015, cols_2016)
missing_16_15 <- setdiff(cols_2016, cols_2015)

missing_15_16

# what happens if we remove underscore?
candy_2015_clean_rename_rm <- candy_2015_clean_rename
colnames(candy_2015_clean_rename_rm) <- str_remove_all(colnames(candy_2015_clean_rename_rm), "_")

candy_2016_clean_rename_rm <- candy_2016_clean_rename
colnames(candy_2016_clean_rename_rm) <- str_remove_all(colnames(candy_2016_clean_rename_rm), "_")
cols_2015_rm <- str_sort(colnames(candy_2015_clean_rename_rm))
cols_2016_rm <- str_sort(colnames(candy_2016_clean_rename_rm))

missing_15_16_rm <- setdiff(cols_2015_rm, cols_2016_rm)
missing_16_15_rm <- setdiff(cols_2016_rm, cols_2015_rm)