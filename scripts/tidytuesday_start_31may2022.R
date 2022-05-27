# Load libraries.
library(readr)      # Loading in data
library(lubridate)  # Dealing with dates

# Load data directly. You need the internet for this!
detroit_df <- read_csv("https://github.com/langtonhugh/nscr_graphics/raw/main/data/detroit_calls.csv")

# Explore data.
glimpse(detroit_df)