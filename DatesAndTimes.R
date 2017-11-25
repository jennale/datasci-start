library(dplyr)
library(tidyr)
library(tidytext)
#Run 'data_cleaning' first

#GET # of calls a day
cmha <- cleanest_calldata

dates <- cleanest_calldata %>% 
  separate(`Call Date`, c('Year','Month', 'Day'),'-', FALSE) 

dates %>% count(Month, Year) 

count(filter(dates,Gender == 'F'))

count(filter(dates, Gender == 'F', Month == '01' & Year == '2017'))

byMonth <- dates %>% count(Month, Year) %>% rowwise() %>%
  mutate(
    Month = month.abb[as.integer(Month)], 
    Calls = n
  ) %>% select(-n)

byYear <- dates %>% count(Year) %>%
  mutate(
    Calls = n
  ) %>% select(-n)

byMonth
byYear
write.csv(byMonth, 'byMonth.csv')
write.csv(byYear, 'byYear.csv')
write.csv(cmha %>% count(Gender), 'genderBreakdown.csv')






