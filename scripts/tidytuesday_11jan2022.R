# Load libraries.
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# Load data directly from the TT github.
star_df <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv')

# Initial explore.
star_df
names(star_df)
glimpse(star_df)
dim(star_df)
sum(is.na(star_df))

# Subset brewed coffee.
brew_df <- star_df %>% 
  filter(str_detect(product_name, "brewed coffee"))

# Quick clean for the plot.
big_ones_df <- star_df %>%
  select(product_name, size, milk, whip, calories:trans_fat_g) %>% 
  filter(size == "grande") %>% 
  mutate(whip_char = as.character(whip),
         milk_labs = recode(milk,
                            `0` = "no milk",
                            `1` = "non-fat",
                            `2` = "2% fat",
                            `3` = "soy",
                            `4` = "coconut",
                            `5` = "whole")) # These categories are on the tt git page.
  
# What in the relationship between calories and fat?
my_plot_gg <- ggplot(data = big_ones_df) +
  geom_point(mapping = aes(x = total_fat_g, y = calories, fill = whip_char),
             size = 2, alpha = 0.8, pch = 21, colour = "black") +
  facet_wrap(~milk_labs) +
  labs(title = "Starbucks: fat, calories and milk types",
       caption = "Data notes: grande drink size | Data source: tidytuesday | NSC-R workshop 11 Jan 2022",
       fill = NULL, x = "total fat (grams)") +
  scale_fill_manual(values = c("#036635", "#b5651d"),
                      labels = c("Without whipped cream", "With whipped cream")) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size = 4))

# Save.
ggsave(my_plot_gg, file = "C:/Users/langt/Documents/GitHub/nscr_graphics/visuals/starbucks_plot.png",
       height = 12, width = 12, unit = "cm")
