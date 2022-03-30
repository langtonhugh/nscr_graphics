# Load packages.
library(tidytuesdayR)
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)

# IOdentify datasets available (fire service data).
tt_df <- tidytuesdayR::tt_load("2021-06-29")

# Pull out the data.
fire_df <- tt_df$animal_rescues

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
  
# Plot the following. It's difficult without the pivot_longer, right?
fire_wide1_df %>% 
  pivot_longer(cols = -animal_group_broad, names_to = "cal_year", values_to = "counts") %>% 
  ggplot(data = .) +
  geom_line(mapping = aes(y = counts, x = cal_year, group = animal_group_broad, colour = animal_group_broad))

# So first, we need to pivot_longer.
pivot_long1_df <- fire_wide1_df %>% 
  pivot_longer(cols = -animal_group_broad, names_to = "cal_year", values_to = "counts")

# Then the plot is easy.
ggplot(data = pivot_long1_df) +
  geom_line(mapping = aes(y = counts, x = cal_year, group = animal_group_broad, colour = animal_group_broad))

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

# Bar chart of these counts? Difficult...
fire_wide2_df %>% 
  pivot_longer(cols = -animal_group_broad, names_to = "property_category", values_to = "yearly_count") %>% 
  ggplot(data = .) +
  # facet_wrap(~property_category) + # optional
  geom_col(mapping = aes(x = animal_group_broad, y = yearly_count)) 
  


