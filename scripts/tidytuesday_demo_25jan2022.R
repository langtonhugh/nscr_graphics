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

# Frequency counts: different methods.

# Base R way.
table(star_df$whip) 

# Grouping way.
star_df %>%
  group_by(whip) %>% 
  tally() 

# Ideal way (thanks Wim!).
freq_df <- count(star_df, whip)

# Basic cleaning.
star_clean_df <- star_df %>%
  select(product_name, size, milk, whip, calories, sugar_g) %>% 
  filter(size == "grande") 

# Basic plot.
ggplot(data = star_clean_df, mapping = aes(x = sugar_g, y = calories)) +
  geom_point()

ggplot(data = star_clean_df) +
  geom_point(mapping = aes(x = sugar_g, y = calories))

ggplot() +
  geom_point(data = star_clean_df, mapping = aes(x = sugar_g, y = calories))

star_clean_df %>%
  filter(whip == 1) %>% 
  ggplot() +
  geom_point(mapping = aes(x = sugar_g, y = calories))

# Change milk class.
star_clean_df <- star_clean_df %>% 
  mutate(milk_char = as.character(milk))

# Extend.
ggplot(data = star_clean_df) +
  geom_point (mapping = aes(x = sugar_g, y = calories)) +
  geom_smooth(mapping = aes(x = sugar_g, y = calories), method = "lm") +
  facet_wrap(~milk_char)

ggplot(data = star_clean_df, mapping = aes(x = sugar_g, y = calories)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(vars(milk_char, whip))
  
ggplot(data = star_clean_df) +
  geom_point(mapping = aes(x = sugar_g, y = calories, shape = milk_char, colour = milk_char))




