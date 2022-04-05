# Load packages.
library(tidytuesdayR)
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)

# Identify datasets available (fire service data).
# tt_df <- tidytuesdayR::tt_load("2021-06-29")

# Pull out the data.
# fire_df <- tt_df$animal_rescues

# Save for train working.
# save.image("data/train_working.Rdata")
load("data/train_working.Rdata")

# Basic cleaning.
fire_clean_df <- fire_df %>% 
  filter(cal_year < 2021) %>% 
  mutate(animal_group_broad = fct_lump(animal_group_parent, n = 5))

# Create some wide data. Freq counts of incidents for each animal by year,
# then wide. 2021 is not complete. Note animal category is just for the demo!
# It is a crude recode.
fire_wide1_df <- fire_clean_df %>% 
  filter(cal_year < 2021) %>% 
  select(incident_number, cal_year, animal_group_broad) %>% 
  group_by(cal_year, animal_group_broad) %>% 
  summarise(yearly_count = n()) %>% 
  pivot_wider(names_from = cal_year, values_from = yearly_count)

# A non-longitudinal example. Note the fiddly way of keeping zero counts. Requires
# an ungroup() and then complete(). There might be a better way of doing this!
fire_wide2_df <- fire_clean_df %>% 
  filter(cal_year == 2009) %>% 
  select(incident_number, animal_group_broad, property_category)  %>% 
  group_by(animal_group_broad, property_category) %>% 
  summarise(yearly_count = n()) %>% 
  ungroup() %>% 
  complete(animal_group_broad, property_category, fill = list(yearly_count = 0)) %>% 
  pivot_wider(names_from = property_category, values_from = yearly_count)

# Nested data / multilevel example.
fire_wards_df <- fire_clean_df %>%
  group_by(borough_code, ward_code, cal_year) %>% 
  summarise(yearly_count = n()) %>% 
  ungroup() %>% 
  filter(ward_code != "NULL") %>% 
  arrange(borough_code, ward_code, cal_year)

# Make wide for example, fill in zeros and arrange.
# Why interesting? Because it's nested levels, longitudinal and 'year' in
# the var names.
fire_wide3_df <- fire_wards_df %>% 
  arrange(cal_year) %>% 
  pivot_wider(names_from = cal_year, values_from = yearly_count, names_prefix = "year_") %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  arrange(ward_code) 

# Remove things that might confuse people.
rm(fire_clean_df, fire_df, tt_df, fire_wards_df)
