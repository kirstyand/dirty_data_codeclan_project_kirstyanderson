# Load libraries
library(tidyverse)
library(janitor)
library(here)
library(readxl)
library(waldo)
library(gtools)
library(plyr)
library(dplyr)
library(tidyr)
# Load data

#candy_2015 <- read_xlsx(here("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx"))
#candy_2016 <- read_xlsx(here("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx"))
#candy_2017 <- read_xlsx(here("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx"))

candy_2015 <- read_xlsx("/Users/KDizzle/Documents/GitHub/dirty_data_codeclan_project_kirstyanderson/Task 4/raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx")
candy_2016 <- read_xlsx("/Users/KDizzle/Documents/GitHub/dirty_data_codeclan_project_kirstyanderson/Task 4/raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx")
candy_2017 <- read_xlsx("/Users/KDizzle/Documents/GitHub/dirty_data_codeclan_project_kirstyanderson/Task 4/raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx")
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



# There are some 'admin' columns at the start of each data set (age, gender, country, state, "going-out"); 
# recode these so that they are labelled the same in all 3 data sets

#2015: rename for going_out and age

candy_2015_clean_rename <- candy_2015_clean %>% 
  dplyr::rename("going_out" = "are_you_going_actually_going_trick_or_treating_yourself", 
         "age" = "how_old_are_you") 

# 2016: rename for going_out, age, country, state

candy_2016_clean_rename <- candy_2016_clean %>% 
  dplyr::rename("going_out" = "are_you_going_actually_going_trick_or_treating_yourself", 
         "age" = "how_old_are_you", 
         "gender" = "your_gender", 
         "country" = "which_country_do_you_live_in", 
         "state" = "which_state_province_county_do_you_live_in")

# 2017: rename for all names to remove "q1_" & then for state
candy_2017_clean_rename <- candy_2017_clean

colnames(candy_2017_clean_rename) <- str_remove_all(colnames(candy_2017_clean), "q[0-9]_")

candy_2017_clean_rename <- candy_2017_clean_rename %>% 
 dplyr::rename("state" = "state_province_county_etc")

cols_2015 <- str_sort(colnames(candy_2015_clean_rename))
cols_2016 <- str_sort(colnames(candy_2016_clean_rename))
cols_2017 <- str_sort(colnames(candy_2017_clean_rename))



# try and combine 2015 and 2016
# need to get list of variables that are missing in each table 

merge_15_16_17 <- c(cols_2015, cols_2016, cols_2017) %>% 
  unique() %>% 
  sort()
merge_15_16_17  

# we now have a list of unique columns, now it's easy to see obvious typos/ differences

candy_2015_clean_rename <- candy_2015_clean_rename %>% 
  dplyr::rename("boxo_raisins" = "box_o_raisins",
         "bonkers_the_candy" = "bonkers")

# bind all tables together, fill blanks with NA

bind_15_16_17_pass1 <- rbind.fill(candy_2015_clean_rename, candy_2016_clean_rename, candy_2017_clean_rename)

# create list of all column names
all_colnames_pass1 <- sort(colnames(bind_15_16_17_pass1))

# check for columns that may be repeated by looking at full list
all_colnames_pass1
merge_15_16_17
# possible combinations:

# 2015 & 2016: "anonymous_brown_globs_that_come_in_black_and_orange_wrappers"
# 2017: "anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes"
# 2015& 2016 also have "mary_janes?" but contain different data!
# just change all 3 to "anonymous_brown_globs_that_come_in_black_and_orange_wrappers"


candy_2017_clean_rename <- candy_2017_clean_rename %>% 
  dplyr::rename("anonymous_brown_globs_that_come_in_black_and_orange_wrappers" = 
         "anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes")
              

# 2015: sweetums
# 2016 & 2017 : sweetums a friend to diabetes

candy_2015_clean_rename <- candy_2015_clean_rename %>% 
  dplyr::rename("sweetums_a_friend_to_diabetes" = "sweetums")


# 2016 "that_dress_that_went_viral_a_few_years_back_when_i_first_saw_it_it_was"                                                            
# 2015 "that_dress_that_went_viral_early_this_year_when_i_first_saw_it_it_was"    
# 2017  "q10_dress"
# rename all to viral_dress
candy_2015_clean_rename <- candy_2015_clean_rename %>% 
  dplyr::rename("viral_dress" = "that_dress_that_went_viral_early_this_year_when_i_first_saw_it_it_was")

candy_2016_clean_rename <- candy_2016_clean_rename %>% 
  dplyr::rename("viral_dress" = "that_dress_that_went_viral_a_few_years_back_when_i_first_saw_it_it_was")

candy_2017_clean_rename <- candy_2017_clean_rename %>% 
  dplyr::rename("viral_dress" = "q10_dress")


# 2017 "q11_day"
# 2016 & 2015 "which_day_do_you_prefer_friday_or_sunday"  

candy_2017_clean_rename <- candy_2017_clean_rename %>% 
  dplyr::rename("which_day_do_you_prefer_friday_or_sunday" = "q11_day")

# 2017 "100_grand_bar"
# 2016 and 2015 : x100_grand_bar

candy_2017_clean_rename <- candy_2017_clean_rename %>% 
  dplyr::rename("x100_grand_bar" = "100_grand_bar")

# now bind again

bind_15_16_17 <- rbind.fill(candy_2015_clean_rename, candy_2016_clean_rename, candy_2017_clean_rename)

head(bind_15_16_17, n = 0)
all_colnames_pass2 <- sort(colnames(bind_15_16_17))
all_colnames_pass2

# a quick messy table showing all column names 
all_cols_df <- cbind(cols_2015, cols_2016, cols_2017)


# columns are good (ish)
# look at country column

country_list <- bind_15_16_17 %>% 
  select(country) %>% 
  filter(!is.na(country)) %>% 
  distinct()

# start by putting all in lower case and arranging alphabetically
bind_15_16_17 <- bind_15_16_17 %>% 
  mutate(country = str_to_lower(country)) %>% 
  arrange(country)

# recode for usa

bind_15_16_17 <- bind_15_16_17 %>% 
  mutate(country = recode(country,
                                  "'merica" = "usa",
                                  "america" = "usa",
                                  "i pretend to be from canada, but i am really from the united states." = "usa",
                                  "n. america" = "usa",
                                  "the best one - usa" = "usa",
                                  "the united states" = "usa",
                                  "the united states of america" = "usa",
                                  "sub-canadian north america... 'merica" = "usa",
                                  "the yoo ess of aaayyyyyy" = "usa",
                                  "u s" = "usa",
                                  "u s a" = "usa",
                                  "u.s." = "usa",
                                  "u.s.a" = "usa",
                                  "u.s.a." = "usa",
                                  "unhinged states" = "usa",
                                  "unied states" = "usa",
                                  "unite states" = "usa",
                                  "united  states of america" = "usa",
                                  "united sates" = "usa",
                                  "united staes" = "usa",
                                  "united state" = "usa",
                                  "united statea" = "usa",
                                  "united stated" = "usa",
                                  "united states" = "usa",
                                  "united states of america" = "usa",
                                  "united statss" = "usa",
                                  "united stetes" = "usa",
                                  "united ststes" = "usa",
                                  "unites states" = "usa",
                                  "units states" = "usa",
                                  "us" = "usa",
                                  "us of a" = "usa",
                                  "ussa"  = "usa")) %>% 
  relocate(country, .after = going_out) %>% 
  relocate(state, .after = country) %>% 
  relocate(gender, .after = age) %>% 
  relocate("green_party_m_ms":"take_5")

# turn candy ratings into long format


candy_clean_long <- pivot_longer(bind_15_16_17,
                                     cols = c("butterfinger": "york_peppermint_patties", 
                                             "necco_wafers", 
                                             "sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year",
                                             "bonkers_the_board_game":"whatchamacallit_bars",
                                             "green_party_m_ms":"take_5"),
                                     names_to = "candy",
                                     values_to = "rating") %>% 
  relocate(candy, .after = state) %>% 
  relocate(rating, .after = candy)

#write csv

write_csv(candy_clean_long, "clean_data/candy_cleaned.csv")
