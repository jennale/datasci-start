library(RecordLinkage)
library(hashmap)

cities_correct_spelling <- c("Ailsa Craig", "Appin", "Aylmer", "Boumanville", "Brantford", "Brighton", "Brockville", "Chatham", "Elgin", "Exeter", "Glencoe", "Granton", "Hamilton", "Ingersoll", "Kingston", "Kitchener", "London", "Markham", "Melbourne", "Mississauga", "Newbury", "None Selected", "Oakville", "Oneida", "Ottawa", "Otterville", "Oxford county", "Parkdale", "Port Dover", "Southwold", "St. Thomas", "Stratford", "Strathroy", "Sudbury", "Thorndale", "Tillsonburg", "Toronto", "Unknown", "Up North", "Walkerton", "West Hamilton", "Whitby", "Windsor", "Woodstock")
cities_lookup <- hashmap(tolower(cities_correct_spelling), cities_correct_spelling)

check.similar.cities <- function(inputCity, cities_lookup) {
  return <- cities_lookup$find(tolower(inputCity))
  
  if (is.na(return)) {
    score <- 0;
    cities <- cities_lookup$values()
    
    for (city in cities) {
      # Use jarowinkler to analyse string similarity. Make both inputs lowercase to reduce noise
      newScore <- jarowinkler(tolower(city), tolower(inputCity))
      
      if (newScore > score) {
        score <- newScore
        return <- city
      }
    }
    
    # We only want to replace the input if there is a cityname that is >85 similar
    if (score < 0.85) {
      return <- inputCity
    }
  }
  return;
}

cmha <- cleanest_calldata

newCities <- cmha %>% filter(!is.na(City)) %>% rowwise() %>% mutate(
  NewCity = check.similar.cities(City, cities_lookup)
) %>% select(`Call ID`, NewCity )

cmha_newCities <- cmha %>%
  left_join(newCities, c("Call ID")) %>%
  mutate(`City` = ifelse(is.na(NewCity), City, NewCity)) %>%
  select(-`NewCity`)

write.csv(cmha_newCities %>% count(City), 'cityBreakdown.csv')

newCities$NewCity %>% unique %>% summary
newCities$City %>% unique %>% summary

View(newCities)

rm(cmha, CMHA_Daily_Call_Volumes, dates, newCities, cities, cities_correct_spelling, city)
