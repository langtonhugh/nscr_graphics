ggplot(data = long_wide) +
  geom_line(mapping = aes(x = years, y = counts, group = animal, colour = animal))

# Load packages.
library(tidyr)
library(dplyr)

# Example 1
fire_wide1_df

# Wide to long
fire_long1_df <- fire_wide1_df %>% 
  pivot_longer(cols = -animal_group_broad, names_to = "years", values_to = "counts")

# Real ggplot code
ggplot(data = fire_long1_df) +
  geom_line(mapping = aes(x = years, y = counts, group = animal_group_broad,
                          colour = animal_group_broad))

# Example 2
fire_wide2_df

# Psuedo.
ggplot(data = long_data) +
  geom_col(mapping = aes(x = animal_group_broad, y = counts)) +
  facet_wrap(~property_type)

# Wide to long.
fire_long2_df <- fire_wide2_df %>% 
  pivot_longer(cols = `Dwelling`:`Road Vehicle`, names_to = "property_type",
               values_to = "counts")

# Real code.
ggplot(data = fire_long2_df) +
  geom_col(mapping = aes(x = animal_group_broad, y = counts)) +
  facet_wrap(~property_type)

# Long to wide.
fire_wide_ex_df <- fire_long2_df %>% 
  pivot_wider(names_from = "property_type", values_from = "counts")





