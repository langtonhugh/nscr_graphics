# Load libraries.
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

# Load raw data.
detroit_df <- read_csv("https://opendata.arcgis.com/datasets/4f49eb825f564efa9a23cd103c4ba13b_0.csv")

# Subset.
detroit_2022_df <- detroit_df %>% 
  mutate(call_timestamp = ymd_hms(call_timestamp),
         year_lub = year(call_timestamp)) %>% 
  filter(year_lub == "2022") %>% 
  select(incident_id, zip_code, X, Y, priority,
         calldescription, call_timestamp, intaketime:totaltime)

# Save.
write_csv(detroit_2022_df, file = "data/detroit_cfs.csv")

# Quick change to drop missings.
detroit_2022_df <- read_csv("data/detroit_cfs.csv")

# Drop missing times we want for the demo.
detroit_2022_df <- detroit_2022_df %>% 
  drop_na(traveltime, time_on_scene) # !!! 

# Savea again. Overwrite the old one.
write_csv(detroit_2022_df, file = "data/detroit_calls.csv")
