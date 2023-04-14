# Load libraries

library(tidyverse)
library(janitor)
library(here)
library(readxl)


--------------------------------------------------------------------------------

# Load data

candy_2015 <- read_xlsx(here("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx"))
candy_2016 <- read_xlsx(here("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx"))
candy_2017 <- read_xlsx(here("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx"))

--------------------------------------------------------------------------------

# Look at data (Highlighted out as optional)

#dim(candy_2015)
# 5630, 124
#dim(candy_2016)
# 1259, 123
#dim(candy_2017)
# 2460, 120

#head(candy_2015)
#head(candy_2016)
#head(candy_2017)
--------------------------------------------------------------------------------
# Clean Data
  
# Clean Column Names:
# use janitor to transform to snake_case, create new df for each to avoid writing over original data
  
candy_2015_clean <- clean_names(candy_2015)
candy_2016_clean <- clean_names(candy_2016)
candy_2017_clean <- clean_names(candy_2017)

--------------------------------------------------------------------------------
  
# Admin Columns:

# There are some 'admin' columns at the start of each data set (age, gender, country, state, "going-out"); 
# recode using rename() so that they are labelled the same in all 3 data sets:


#2015: rename for "going_out" and "age"

candy_2015_clean <- candy_2015_clean %>% 
  rename("going_out" = "are_you_going_actually_going_trick_or_treating_yourself", 
         "age"       = "how_old_are_you") 


#2016: rename for "going_out", "age", "country", "state"

candy_2016_clean <- candy_2016_clean %>% 
  rename("going_out" = "are_you_going_actually_going_trick_or_treating_yourself", 
         "age"       = "how_old_are_you", 
         "gender"    = "your_gender", 
         "country"   = "which_country_do_you_live_in", 
         "state"     = "which_state_province_county_do_you_live_in")


#2017: remove all instances of "q#_' using str_remove, then rename for "state". 

colnames(candy_2017_clean) <- str_remove_all(colnames(candy_2017_clean), "q[0-9]_")

candy_2017_clean <- candy_2017_clean %>% 
  rename("state" = "state_province_county_etc")


--------------------------------------------------------------------------------

# Candy & Other Columns:
  
# Compare the 3 datasets by (1) creating a list of columns in each dataset, 
# and then (2) combining these and collect one instance of each column name: 

#(1)
cols_2015 <- str_sort(colnames(candy_2015_clean))
cols_2016 <- str_sort(colnames(candy_2016_clean))
cols_2017 <- str_sort(colnames(candy_2017_clean))

#(2)
all_cols  <- c(cols_2015, cols_2016, cols_2017) %>% 
                  unique() %>% 
                 sort()

# Check this worked (optional)

# all_cols 


# "all_cols" now provides a list of unique columns, 
# now it's easy to see obvious typos/ differences

# Possible Combinations:

# 2015  : box_o_raisins              <- rename
# 2016/7: boxo_raisins

# 2015  : bonkers                    <- rename
# 2016/7: bonkers_the_candy

# 2015  : sweetums                   <- rename
# 2016/7: sweetums_a_friend_to_diabetes

# 2015/6: anonymous_brown_globs_that_come_in_black_and_orange_wrappers
# 2017  : "as above"_a_k_a_mary_janes <- rename
# 2015 & 2016 also have "mary_janes"? But contain different data!

# 2015  : that_dress_that_went_viral_early_this_year_when_i_first_saw_it_it_was <- rename
# 2016  : that_dress_that_went_viral_a_few_years_back_when_i_first_saw_it_it_was<- rename                                                            
# 2017  : q10_dress <- rename 

# 2015/6: which_day_do_you_prefer_friday_or_sunday
# 2017  : q11_day                    <- rename

# 2015/6: x100_grand_bar
# 2017  : 100_grand_bar              <- rename

# Rename combinations from above so that data sets can be joined:

candy_2015_clean <- candy_2015_clean %>% 
  rename("boxo_raisins"                  = "box_o_raisins",
         "bonkers_the_candy"             = "bonkers",
         "sweetums_a_friend_to_diabetes" = "sweetums",
         "viral_dress" = "that_dress_that_went_viral_early_this_year_when_i_first_saw_it_it_was")

candy_2016_clean <- candy_2016_clean %>% 
  rename("viral_dress" = "that_dress_that_went_viral_a_few_years_back_when_i_first_saw_it_it_was")

candy_2017_clean <- candy_2017_clean %>% 
  rename("anonymous_brown_globs_that_come_in_black_and_orange_wrappers" = 
         "anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes",
         "viral_dress" = "q10_dress",
         "which_day_do_you_prefer_friday_or_sunday" = "q11_day",
         "x100_grand_bar" = "100_grand_bar")


# Preserve "year" info by adding a year column to each dataset

candy_2015_clean <- candy_2015_clean %>% 
  add_column(year = 2015,.after = "going_out")

candy_2016_clean <- candy_2016_clean %>% 
  add_column(year = 2016,.after = "going_out")

candy_2017_clean <- candy_2017_clean %>% 
  add_column(year = 2017,.after = "going_out")



# Bind the 3 datasets together, fill any missing data with NA

candy_clean_bound <- plyr::rbind.fill(candy_2015_clean, 
                                candy_2016_clean, 
                                candy_2017_clean)
# Check that binding worked (optional)

#head(candy_clean_bound, n = 0)


# Move 'admin' columns to the front:

candy_clean_bound <- candy_clean_bound %>% 
                     relocate(year, .after = timestamp) %>% 
                     relocate(gender,  .after = age)    %>% 
                     relocate(country, .after = gender) %>% 
                     relocate(state, .after = country)

# Transform to long format, with one column for candy and one column for rating

candy_clean_long <- pivot_longer(candy_clean_bound,
                                 cols = c("butterfinger": "york_peppermint_patties", 
                                          "necco_wafers", 
                                          "sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year",
                                          "bonkers_the_board_game":"whatchamacallit_bars",
                                          "green_party_m_ms":"take_5"),
                                 names_to = "candy",
                                 values_to = "rating")  %>% 
                    relocate(candy, .after = going_out) %>% 
                    relocate(rating, .after = candy)


# The columns are now (reasonably) clean! Now deal with "dirty" column values in next section

--------------------------------------------------------------------------------
# Sorting "dirty" column values

# "Country" column
# Needs recoding, as there are lots of different values that probably mean the same country
  
# Get list of all countries:

country_list <- candy_clean_long %>% 
  select(country) %>% 
  filter(!is.na(country)) %>% 
  distinct()

# start by putting all in lower case and arranging alphabetically

candy_clean_long <- candy_clean_long                        %>% 
                    mutate(country = str_to_lower(country)) %>% 
                    arrange(country)

# recode for usa
# Ideally use a smart RegEx function, but for the sake of time and brain power I will do it manually!

candy_clean_long <- candy_clean_long %>% 
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
                          "ussa"  = "usa")) 




# "Age" column

# replace all non-numeric values with NA

# create column showing distinct ages to make it easier to visualise (optional)

#distinct_candy_age <- candy_clean_long %>% 
#                      select(age)      %>% 
#                      distinct(age)


# Create a function so that if the age value contains no numbers, replace with NA

# (1) search for strings that don't contain the pattern, replace these with NA:
candy_clean_long$age[!grepl("^-?[0-9]+(\\.[0-9]+)?$", candy_clean_long$age)] <- NA 
candy_clean_long$age <- as.numeric(candy_clean_long$age) # (2) convert the column to numeric
candy_clean_long$age <- round(candy_clean_long$age, digits = 0) # (3) round age to nearest integer 

# check this worked (optional)
# candy_clean_long$age %>%  unique()


#also replace infinity "inf" with NA

candy_clean_long$age[sapply(candy_clean_long$age, is.infinite)] <- NA

--------------------------------------------------------------------------------
# Save cleaned data

#Write cleaned data to CSV, save in "clean_data" folder:

write_csv(candy_clean_long, "clean_data/candy_cleaned.csv")
