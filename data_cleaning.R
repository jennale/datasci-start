library(dplyr)
library(tidyr)
library(tidytext)

calldata <- readxl::read_excel("CMHA/Copy of Research Report December 1 2016 to September 30 2017.xlsx");

#------------------------------
# CLEANUP NOTES
#------------------------------

#Aggregate the notes into one string so that now we have exactly one row per call.
collapsed_notes <- calldata %>%
  mutate(`Call ID` = zoo::na.locf(`Call ID`, na.rm = FALSE)) %>%
  group_by(`Call ID`) %>%
  summarise(Notes = paste(Notes, collapse = "|"))

#Join the aggregated notes to the original data, using only rows where `Call ID` is not NA
calldata_with_notes <- calldata %>%
  filter(!is.na(`Call ID`)) %>%
  select(-Notes) %>%
  left_join(collapsed_notes) %>%
  mutate(Notes = ifelse(Notes == 'NA', NA , Notes))

# Cleanup for next step
rm(calldata, collapsed_notes)

#------------------------------
# MISSING AGE GROUPS
#------------------------------
age_groups <- calldata_with_notes$`Age Group` %>% 
  tbl_df %>%
  unique %>% 
  separate(value, c('StartAgeGroup', 'EndAgeGroup'),'-', FALSE) %>%
  filter(!is.na(StartAgeGroup), !is.na(EndAgeGroup)) %>%
  arrange(StartAgeGroup)

# Create a final age group (MaxAge + ie 85+) for those who don't fall into the specified groups
maxAge <- as.integer(max(age_groups$EndAgeGroup)) + 1
# 
age_groups <- age_groups %>%
  bind_rows(c(value = paste(maxAge, '+') , StartAgeGroup = maxAge, EndAgeGroup = 10000))

# Given an age, return an age group based on unique values in the data
get.age.group <- function(age, age_groups) {
  result <- age_groups %>%
    filter(as.integer(StartAgeGroup) <= as.integer(age) & as.integer(age) <= as.integer(EndAgeGroup)) %>%
    select(value)
    toString
  result
}

get.age.group(25, age_groups)

# 1096 entries without an age group, but have an age. Assign them age groups using age_groups object
collapsed_ages <- calldata_with_notes %>%
  filter(is.na(`Age Group`), !is.na(Age)) %>%
  select(`Call ID`,`Age Group`, Age) %>%
  rowwise() %>%
  mutate(`Age Group` = toString(get.age.group(toString(Age), age_groups)))

# Combine with previously cleaned data
cleaner_calldata <- calldata_with_notes %>%
  left_join(collapsed_ages, c("Call ID", "Age")) %>%
  mutate(`Age Group` = ifelse(is.na(`Age Group.x`), `Age Group.y`, `Age Group.x`)) %>%
  select(-`Age Group.x`, -`Age Group.y`)

#Result shows no more data without age group but with age. 
# filter(cleaner_calldata, is.na(`Age Group`), !is.na(Age))

# Cleanup for next step
rm(age_groups, maxAge, collapsed_ages, calldata_with_notes)

#------------------------------
# MISSING GENDERS
#------------------------------
# 1,781 rows missing gender but have notes
genderless_notes <- cleaner_calldata %>% 
  filter(is.na(Gender), !is.na(Notes)) %>%
  select(`Call ID`, Gender, Notes)

# Gendered pronouns to look for
female <- c("she", "her", "hers")
male <- c("he", "him", "his")
pronouns <- append(female,male)

# Create a table of tokenized pronouns for each record with pronouns
wordslist <- genderless_notes %>% 
  unnest_tokens(word,Notes) %>% 
  group_by(`Call ID`) %>% 
  filter(word %in% pronouns)

# There are 780 records with a missing gender, but gendered pronouns in the description

# Create gendered pronoun counts
females_count<- wordslist %>% count(word %in% female) %>% 
  filter(`word %in% female` == TRUE)
males_count<- wordslist %>% count(word %in% male) %>% 
  filter(`word %in% male` == TRUE)

# We will assign the Probable Gender if % of either gender is > 60%. Anything else is labeled NA
pronouns_breakdown <- left_join(females_count, males_count, c("Call ID")) %>% 
  mutate(n.x = ifelse(is.na(n.x), 0 , n.x), 
         n.y = ifelse(is.na(n.y), 0, n.y)) %>% 
  mutate('Female Pronouns' = n.x, 
         'Male Pronouns' = n.y, 
         'Total Pronouns' = n.x + n.y, 
         "% Female" = n.x/(n.x + n.y), 
         "% Male" = n.y/(n.x + n.y)) %>%
  mutate('Probable Gender' = ifelse(
   `% Female` > as.double(0.60), "Female",
   ifelse(`% Male` > as.double(0.60), "Male", NA)
  )) %>% 
  select(-n.x, -n.y, -`word %in% male` -`word %in% female`)

# Resulted in 750 values that were classified
collapsed_genders <- pronouns_breakdown %>% select(`Call ID`, `Probable Gender`)

# Combine with previously cleaned data
cleanest_calldata <- cleaner_calldata %>%
  left_join(collapsed_genders) %>%
  mutate(`Gender` = ifelse(is.na(`Gender`), `Probable Gender`, `Gender`)) %>%
  mutate(`Gender` = ifelse(`Gender` == 'F' || `Gender` == 'f', "Female", `Gender` )) %>% 
  mutate(`Gender` = ifelse(`Gender` == 'M'|| `Gender` == 'm', "Male", `Gender` )) %>% 
  select(-`Probable Gender`)

# Cleanup
rm(cleaner_calldata, female, male, pronouns, females_count, males_count, pronouns_breakdown, collapsed_genders,wordslest)

summary(cleanest_calldata)
View(cleanest_calldata)