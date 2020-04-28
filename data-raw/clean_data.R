library(tidyverse)
library(readxl)
library(stringr)

raw_lead_data <- read_excel('data-raw/monthlypostingMar2020.xlsx', skip = 1)
# glimpse(raw_lead_data)

tested_schools <- raw_lead_data %>% 
  group_by(SchoolName, XMOD) %>% 
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
  arrange(district, schoolName)

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

cleaned_data <- all_schools %>% 
  left_join(district_lookup) %>% 
  mutate(district = ifelse(is.na(match), district, match),
         district = ifelse(is.na(district) | district == 'private', 'Private', district)) %>% 
  select(-match) 

# cleaned_data %>% 
#   group_by(district, schoolName) %>% 
#   mutate(count = n()) %>% 
#   filter(count > 1) %>% 
#   mutate(r = rank(schoolAddress, ties.method = 'first')) %>% View

# duplicates due to addresses
cleaned_data %>% 
  group_by(district, schoolName) %>% 
  arrange(district, schoolName) %>% 
  mutate(r = rank(schoolAddress, ties.method = 'first')) %>% 
  filter(r == 1) %>% 
  select(-r) %>% 
  write_csv('ca_schools_lead_testing_data.csv') 

