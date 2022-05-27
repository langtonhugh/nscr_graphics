# Load libraries.
library(readr) 
library(dplyr)
library(lubridate) 
library(ggplot2) 

# Load data directly. You need the internet for this!
detroit_df <- read_csv("https://github.com/langtonhugh/nscr_graphics/raw/main/data/detroit_calls.csv")

# Explore data.
glimpse(detroit_df)

# Make a frequency table of call types.
calls_freq_df <- detroit_df %>% 
  count(calldescription)

# Subset for shots fired in progress.
shots_df <- detroit_df %>% 
  filter(calldescription == "SHOTS FIRED IP")

# Make a descriptive table of response times (travel time).
shots_desc_df <- shots_df %>% 
  summarise(`Min.` = min(traveltime),
            `Max.` = max(traveltime),
            Mean   = mean(traveltime),
            Median = median(traveltime),
            SD     = sd(traveltime))
# Hourly trends.
shots_df %>% 
  mutate(time_hours = hour(call_timestamp)) %>% 
  group_by(time_hours) %>% 
  tally() %>% 
  ggplot(data = .) +
  geom_line(mapping = aes(x = time_hours, y = n)) +
  geom_point(mapping = aes(x = time_hours, y = n))

