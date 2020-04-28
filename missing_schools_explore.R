clean <- read_csv('ca_schools_lead_testing_data.csv')
missing <- read_csv('missing_schools.csv') %>% 
  select(district = DISTRICT, schoolName = SchoolName) %>% 
  unique() %>% 
  mutate(missing = TRUE)

clean %>% 
  filter(schoolName == 'Larchmont Charter')

missing %>% 
  left_join(clean) %>% 
  filter(is.na(status)) %>%  View