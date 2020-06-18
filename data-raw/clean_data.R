library(tidyverse)
library(readxl)
library(stringr)

raw_lead_data <- read_excel('data-raw/monthlypostingMar2020.xlsx', skip = 1)
glimpse(raw_lead_data)

tested_schools <- raw_lead_data %>% 
  group_by(SchoolName, DISTRICT, XMOD) %>% 
  mutate(maxResult = max(RESULT), unit = 'ppb') %>% 
  select(XMOD, district = DISTRICT, schoolName = SchoolName, 
         schoolAddress = SchoolAddress, maxResult, unit) %>% 
  unique() %>% 
  ungroup() %>% 
  group_by(schoolName, district) %>% 
  mutate(maxResult = max(maxResult),
         lead = case_when(XMOD != "<" & maxResult > 5 ~ TRUE, 
                          XMOD == "<" & maxResult == 5 ~ FALSE, 
                          TRUE ~ as.logical(NA)),
         maxResult = ifelse(lead == FALSE, NA, maxResult),
         status = "tested") %>% 
  filter(!is.na(lead)) %>% 
  select(-XMOD) %>% 
  unique()

glimpse(tested_schools)

exempt <- read_excel('data-raw/exemption_formsMar2020.xlsx')
# glimpse(exempt)

exempt_schools <- exempt %>% 
  mutate(maxResult = NA, unit = NA, lead = NA, status = "exempt") %>% 
  select(district = `School District`, schoolName = Name, schoolAddress = Address, 
         maxResult, unit, lead, status)

# old not tested data, remove schools that have been tested now
un_tested <- read_excel('data-raw/SchoolsUnsampled.xlsx')
# glimpse(un_tested)

not_tested <- un_tested %>% 
  select(district = District, schoolName = School) %>% 
  unique() %>% 
  mutate(status = 'not tested')

not_tested_schools <- tested_schools %>%
  bind_rows(not_tested) %>% 
  group_by(district, schoolName, status) %>% 
  summarise(count = n()) %>% 
  spread(status, count) %>% 
  mutate(dup = sum(c(`not tested`, tested), na.rm = TRUE)) %>% 
  filter(dup == 1, is.na(tested)) %>% 
  ungroup() %>% 
  select(district, schoolName) %>% 
  unique() %>% 
  mutate(status = 'not tested')

all_schools <- tested_schools %>% 
  bind_rows(exempt_schools) %>% 
  bind_rows(not_tested_schools) %>% 
  arrange(district, schoolName) %>% 
  ungroup()

# CLEAN UP DISTRICT NAMES
# check for dirty district naming
dirty_district <- all_schools %>% 
  select(district) %>% 
  unique %>% 
  arrange(district) %>% 
  separate(district, c('begin', 'rest'), ' ', remove = FALSE) %>% 
  mutate(prev_beg = lag(begin), next_beg = lead(begin), prev_district = lag(district),
         next_district = lead(district),
         match = case_when(
           begin == prev_beg ~ prev_district,
           begin == next_beg ~ next_district,
           TRUE ~ 'no match'
         )) %>%
  filter(match != 'no match') %>% 
  select(district, match) 

# district names from CDE https://www.cde.ca.gov/ds/si/ds/pubschls.asp
cde_districts <- read_excel('data-raw/pubdistricts.xlsx', skip = 5) %>% 
  select(district = District) %>% 
  mutate(in_cde = TRUE)

district_lookup <- dirty_district %>%   
  left_join(cde_districts) %>% 
  filter(is.na(in_cde)) %>% 
  select(-in_cde)

cleaned_districts <- all_schools %>% 
  left_join(district_lookup) %>% 
  mutate(district = ifelse(is.na(match), district, match),
         district = ifelse(is.na(district) | district == 'private', 'Private', district)) %>% 
  select(-match) 

# duplicates due to addresses
clean_addresses <- cleaned_districts %>% 
  group_by(district, schoolName) %>% 
  arrange(district, schoolName) %>% 
  mutate(r = rank(schoolAddress, ties.method = 'first')) %>% 
  filter(r == 1) %>% 
  select(-r) %>% 
  bind_rows(
    all_schools %>%
      filter(schoolName %in% c("Bella Vista Elementary", "Wonderful College Prep Academy"),
             district %in% c("San Ramon Valley Unified School District", "Kern County Superintendent"))
  ) 

# from Laura's manual deduplicating effort
# pre_dedup <- read_csv('ca_schools_lead_testing_data_March11_pre_dedup.csv')
# dedup <- read_csv('ca_schools_lead_testing_data_March11.csv')
# 
# duplicate_checked <- pre_dedup %>%
#   left_join(mutate(dedup, non_duplicate = TRUE)) %>% 
#   mutate(keep_record = ifelse(is.na(non_duplicate), FALSE, TRUE),
#          duplicate_checked = TRUE) %>% 
#   select(-non_duplicate)
# 
# clean_addresses %>% 
#   left_join(duplicate_checked) %>% 
#   mutate(duplicate_checked = replace(duplicate_checked, is.na(duplicate_checked), FALSE)) %>% 
#   write_csv('data-raw/ca_schools_lead_testing_data_for_dedup_QC.csv')

# file back from Laura after manual deduplicating effort is finsihed
# NOTE Laure only checked the remaining ones that were not tested or exempt for duplication (the entries of interest)
clean_data <- read_csv('data-raw/ca_schools_lead_testing_data_dedup_checked.csv') 

clean_data %>% 
  mutate(keep_record = replace(keep_record, is.na(keep_record), TRUE)) %>% 
  filter(keep_record) %>% 
  select(-keep_record, -duplicate_checked) %>% 
  write_csv('ca_schools_lead_testing_data.csv') 


# summary stats -----
# % of schools who have tested out of those required to test (so exempting exempt schools). 
cleaned_data <- read_csv('ca_schools_lead_testing_data.csv')
tested <- cleaned_data %>% 
  filter(status != 'exempt') %>% 
  select(schoolName, status) %>% 
  unique() %>% 
  group_by(status) %>% 
  summarise(count = n()) %>% 
  pull(count)

tested[2] / (tested[1] + tested[2]) # prop tested

# Also great to know the % and number of schools and school districts that found lead over 5 PPB.
lead_found <- cleaned_data %>% 
  select(schoolName, lead) %>% 
  unique() %>%
  filter(lead == 1) %>%
  summarise(count = n()) %>% 
  pull(count)

lead_found / (tested[2])
lead_found / (tested[1] + tested[2])

district_lead_found <- cleaned_data %>% 
  select(district, lead) %>% 
  unique() %>% 
  filter(lead == 1) %>% 
  summarise(count = n()) %>% 
  pull(count)

num_districts <- cleaned_data %>% 
  select(district, status) %>% 
  unique() %>% 
  summarise(count = n()) %>% 
  pull(count)

num_district_tested <- cleaned_data %>% 
  select(district, status) %>% 
  unique() %>% 
  filter(status == 'tested' | status == 'exempt') %>% 
  summarise(count = n()) %>% 
  pull(count)

district_lead_found / num_districts
district_lead_found / num_district_tested
