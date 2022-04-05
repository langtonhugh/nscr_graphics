# Load packages.
library(dplyr)
library(tidyr)

# Example 1
fire_wide1_df

# Psuedo code.
ggplot(data = long_data) +
  geom_line(mapping = aes(x = cal_year, y = yearly_count, colour = animal_group_broad))

# Wide to long.
fire_long1_df <- fire_wide1_df %>% 
  pivot_longer(cols = -animal_group_broad, names_to = "years", values_to = "counts")
  
# Real code.
ggplot(data = fire_long1_df) +
  geom_line(mapping = aes(x = years, y = counts, group = animal_group_broad, colour = animal_group_broad))

# Example 2
fire_wide2_df

# Psuedo code
ggplot(data = long_wide) +
  geom_col(mapping = aes(x = animal_group_broad, y = yearly_count)) +
  facet_wrap(~location_type)

# Wide to long
fire_long2_df <- fire_wide2_df %>% 
  pivot_longer(cols = Dwelling:`Road Vehicle`, names_to = "location_type", values_to = "counts" )

# Real code
ggplot(data = fire_long2_df) +
  geom_col(mapping = aes(animal_group_broad, y = counts)) +
  facet_wrap(~location_type)

# Back to wide
fire_wide_demo_df <- fire_long2_df %>% 
  pivot_wider(names_from = location_type, values_from = counts)

# Example 3
fire_long3_df <- fire_wide3_df %>% 
  pivot_longer(cols = year_2009:year_2020, names_to = "year", values_to = "counts", 
               names_prefix = "year_")
