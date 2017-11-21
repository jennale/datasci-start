library(dplyr)
library(tidyr)

calldata <- readxl::read_excel("CMHA/Copy of Research Report December 1 2016 to September 30 2017.xlsx");

#Aggregate the notes into one string so that now we have exactly one row per call.
collapsed_notes <- calldata %>%
  mutate(`Call ID` = zoo::na.locf(`Call ID`, na.rm = FALSE)) %>%
  group_by(`Call ID`) %>%
  summarise(Notes = paste(Notes, collapse = "|"))

#Join the aggregated notes to the original data, using only rows where `Call ID` is not NA
calldata_with_notes <- calldata %>%
  filter(!is.na(`Call ID`)) %>%
  select(-Notes) %>%
  left_join(collapsed_notes)

View(calldata_with_notes)

age_groups <- calldata_with_notes$`Age Group` %>% 
  unique %>% 
  tbl_df %>%
  separate(value, c('StartAgeGroup', 'EndAgeGroup'),'-', FALSE) %>%
  filter(!is.na(StartAgeGroup), !is.na(EndAgeGroup)) %>%
  arrange(StartAgeGroup)
maxAge <- as.integer(max(age_groups$EndAgeGroup)) + 1
age_groups <- age_groups %>%
  bind_rows(c(value = paste(maxAge, '+') , StartAgeGroup = maxAge, EndAgeGroup = 10000))

age_groups

get.age.group <- function(age, age_groups) {
  result <- age_groups %>%
    filter(as.integer(StartAgeGroup) <= as.integer(age) & as.integer(age) <= as.integer(EndAgeGroup)) %>%
    first(value) %>%
    pull(value) %>%
    toString
  result
}

# 1096 entries without an age group, but with an age
collapsed_ages <- calldata_with_notes %>%
  filter(is.na(`Age Group`), !is.na(Age)) %>%
  select(`Call ID`,`Age Group`, Age) %>%
  rowwise() %>%
  mutate(`Age Group` = toString(get.age.group(toString(Age), age_groups)))

View(collapsed_ages)

cleaner_calldata <- calldata_with_notes %>%
  left_join(collapsed_ages, c("Call ID", "Age")) %>%
  mutate(`Age Group` = ifelse(is.na(`Age Group.x`), `Age Group.y`, `Age Group.x`), Notes = ifelse(Notes == 'NA', NA , Notes)) %>%
  select(-`Age Group.x`, -`Age Group.y`)

View(cleaner_calldata)

filter(calldata_with_notes, is.na(`Age Group`), !is.na(Age))
filter(cleaner_calldata, is.na(`Age Group`), !is.na(Age))
