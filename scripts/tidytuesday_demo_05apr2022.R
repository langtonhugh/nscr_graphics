# Thanks to Franziska Yasrebi-de Kom for additional comments to the script.

# As with any script, I recommend running it within a dedicated R Project. Either way,
# remember to save this script and prep data in an appropriate folder structure, and/or
# set your working directory.

# First, we run a prep script. This loads in some raw tidytuesday data about London
# Fire Service calls involving animals. In the prep script, I transform the data into
# three different wide datasets which we then explore here.

source(file = "tidytuesday_prep_05apr2022.R") # Change path accordingly if not in project.

# Load packages.
library(tidyr)
library(dplyr)

# Example 1
fire_wide1_df

# Psuedo code for the first plot. This might help you think about what the data needs
# to look like in order to write a sensible ggplot code chunk.
# ggplot(data = long_wide) +
#   geom_line(mapping = aes(x = years, y = counts, group = animal, colour = animal))


# Wide to long
# Note that minus sign basically means "choose everything *apart* from this variable.
# In names_to() we name the new single column which will be created from the existing cols.
# In values_to() we state the new column that will be created from the values in cells.
fire_long1_df <- fire_wide1_df %>% 
  pivot_longer(cols = -animal_group_broad, names_to = "years", values_to = "counts") 

# Real ggplot code
ggplot(data = fire_long1_df) +
  geom_line(mapping = aes(x = years, y = counts, group = animal_group_broad,
                          colour = animal_group_broad))

# Example 2
fire_wide2_df

# Psuedo code.
# ggplot(data = long_data) +
#   geom_col(mapping = aes(x = animal_group_broad, y = counts)) +
#   facet_wrap(~property_type)

# Wide to long.
# Note that we use `:` to specify multiple cols, but we could have used minus sign again.
fire_long2_df <- fire_wide2_df %>% 
  pivot_longer(cols = `Dwelling`:`Road Vehicle`, names_to = "property_type",
               values_to = "counts")

# Real code.
ggplot(data = fire_long2_df) +
  geom_col(mapping = aes(x = animal_group_broad, y = counts)) +
  facet_wrap(~property_type)

# Long to wide 
# Note that names_from() and values_from() are now reversing the earlier process.
fire_wide_ex_df <- fire_long2_df %>% 
  pivot_wider(names_from = "property_type", values_from = "counts")
