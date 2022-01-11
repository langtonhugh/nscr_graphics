# Load libraries.
library(readr)
library(dplyr)
library(ggplot2)

# Load data. 
star_df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv")

# Explore.
glimpse(star_df)
dim(star_df)
sum(is.na(star_df))
names(star_df)

# Frequency count.
table(star_df$whip) # normal way

freq_df <- star_df %>% # tidyverse way
  group_by(whip) %>% 
  tally() 

is_grouped_df(freq_df)

# Basic cleaning.
star_clean_df <- star_df %>%
  select(product_name, size, milk, whip, calories, sugar_g) %>% 
  filter(size == "grande")

# Basic plot.
ggplot(data = star_clean_df) +
  geom_point(mapping = aes(x = sugar_g, y = calories))




  
  
  



