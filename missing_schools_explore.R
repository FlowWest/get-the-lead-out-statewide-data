library(tidyverse)
library(readxl)

# updated to fix schools with same name different districts
clean <- read_csv('ca_schools_lead_testing_data.csv')

# from Laura's manual deduplicating effort
pre_dedup <- read_csv('ca_schools_lead_testing_data_March11_pre_dedup.csv')
dedup <- read_csv('ca_schools_lead_testing_data_March11.csv')

dim(pre_dedup) - dim(dedup) 

duplicate_checked <- pre_dedup %>%
  left_join(mutate(dedup, non_duplicate = TRUE)) %>% 
  mutate(keep_record = ifelse(is.na(non_duplicate), FALSE, TRUE),
         duplicate_checked = TRUE) %>% 
  select(-non_duplicate)

View(duplicate_checked)

tt <- duplicate_checked %>% 
  full_join(mutate(clean, updated = TRUE)) 

tt %>% glimpse

tt2 <- duplicate_checked %>% 
  left_join(mutate(clean, updated = TRUE)) 

dim(tt2)
dim(clean)

clean %>% 
  left_join(duplicate_checked) %>% glimpse
  group_by(keep_record, duplicate_checked) %>% 
  summarise(n())

View(tt)

tt %>% 
  filter(is.na(non_duplicate) | non_duplicate) %>% 
  group_by(updated) %>% 
  summarise(n())

missing <- read_csv('missing_schools.csv')  %>% 
  filter(`not found in testing data list`) %>% 
  select(district = DISTRICT, schoolName = SchoolName) %>% 
  unique() %>% 
  mutate(missing = TRUE)

missing %>% 
  left_join(clean) %>% 
  filter(is.na(status)) %>% pull(district) %>% dput()


