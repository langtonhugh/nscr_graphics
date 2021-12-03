# Load packages.
library(readxl)
library(readr)
library(dplyr)
library(janitor)
library(stringr)
library(biscale)
library(cowplot)
library(sf)
library(ggplot2)


# Useful function. 
`%nin%` <- Negate(`%in%`)

# Load CBS data containing income levels at buurt-level. Note that missings are defined as ".".
cbs_df <- read_xls("data/kwb-2019.xls", na = ".")

# Load police data. Note that missings are ".".
burglary_df <- read_delim("data/Misdrijven_per_wijk_en_buurt_per_jaar_03122021_095311.csv", trim_ws = TRUE)

# Clean police data.
burglary_clean_df <- burglary_df %>% 
  clean_names() 

# Clean and subset CBS data.
# a_hh (n households), g_woz (house value), p_mgezw (family homes).
income_df <- cbs_df %>% 
  filter(recs == "Wijk") %>%                             # too many missings at buurt level.
  select(gwb_code_10, regio, a_hh, g_woz, p_mgezw) %>%   # relevant variables.           
  filter(regio %in% burglary_clean_df$wijken_en_buurten) # subset cbs data for the Amsterdam Wijk.
  
# Join income data with the burglary data. Note it is still over many years.
full_df <- left_join(income_df, burglary_clean_df, by = c("regio" = "wijken_en_buurten"))

# Filter for 2019, convert to numerics, create burglary rate per number of households.
full_2019_df <- full_df %>% 
  filter(perioden == "2019") %>% 
  mutate_at(c("a_hh", "g_woz", "p_mgezw"), as.numeric) %>% 
  filter(a_hh > 0) %>% 
  mutate(burg_rate = 1000*(geregistreerde_misdrijven_aantal/a_hh)) 

# Basic scatterplot. Note that missings are removed.
ggplot(data = full_2019_df, mapping = aes(x = g_woz, y = burg_rate, group = 1)) +
  geom_point()  +
  geom_text(mapping = aes(label = regio) )

# Read in wijken boundaries.
wijk_sf <- st_read("data/WijkBuurtkaart_2019_v3/wijk_2019_v3.shp")
# wijk_sf <- st_read("data/WijkBuurtkaart_2020_v2/wijk_2020_v2.shp")

# Clean, subset and join.
wijk_clean_sf <- wijk_sf %>% 
  clean_names() %>% 
  select(gm_naam, wk_code, wk_naam) %>% 
  filter(wk_code %in% full_2019_df$gwb_code_10) %>% 
  left_join(full_2019_df, by = c("wk_code" = "gwb_code_10")) 

# Simple map.
ggplot(data = wijk_clean_sf) +
  geom_sf(mapping = aes(fill = g_woz)) 
  
# The police data contained wijken that were outside of Amsterdam, for whatever reason. Filter out.
wijk_clean_ams_sf <- wijk_clean_sf %>% 
  filter(gm_naam == "Amsterdam")

# Simple map.
ggplot(data = wijk_clean_ams_sf) +
  geom_sf(mapping = aes(fill = g_woz)) 

# Create biscale. Two missings.
wijk_clean_ams_sf <- bi_class(wijk_clean_ams_sf, x = g_woz, y = burg_rate, style = "quantile", dim = 3)

# Biscale map.
biscale_map_gg <- wijk_clean_ams_sf %>% 
  filter(!is.na(g_woz)) %>%
  ggplot(data = .) +
  geom_sf(mapping = aes(fill = bi_class)) +
  bi_scale_fill(pal = "GrPink", dim = 3, na.value = "snow") +
  theme_void() +
  theme(legend.position = "none")

# Legend.
my_legend <- bi_legend(pal = "GrPink",
                       dim = 3,
                       xlab = "Higher house prices",
                       ylab = "Higher burglary",
                       size = 9) +
  theme(plot.background = element_rect(fill = "snow"))

# Plot.
map_gg <- ggdraw() +
  draw_plot(biscale_map_gg) +
  draw_plot(my_legend,
            x = 0.1, y = 0.2, width = 0.22, height = 0.22) 

# Get biscale palette.
bi_hex <- bi_pal("GrPink", dim = 3, preview = FALSE)

# Scatter plot with biscale classes.
scat_gg <- ggplot(data = wijk_clean_ams_sf) +
  geom_point(mapping = aes(x = g_woz, y = burg_rate, fill = bi_class), size = 5, pch = 21, colour = "black") +
  scale_fill_manual(values = bi_hex) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "House prices", y = "Burglary rate") +
  theme(plot.margin = margin(t = 80, r = 0, l = 0, b = 60))

# Annotate scatter plot.
scat_ann_gg <- scat_gg +
  annotate(geom = "text", x = 780, y = 30, label = "Lutkemeer and Ookmeer", size = 4) +
  annotate(geom = "curve" , xend = 549, yend = 35.4, x = 700, y = 31, size = 0.7,
           arrow = arrow(length = unit(0.01, "npc")), curvature = 0.3)

# Arrange.
main_plot <- plot_grid(map_gg, scat_ann_gg, ncol = 2, scale = c(1,0.9)) +
  theme(axis.text.x = element_text (size = 12),
        axis.text.y = element_text (size = 12),
        axis.title.y = element_text(size = 14, margin = margin(0,20,0,0)),
        axis.title.x = element_text(size = 14, margin = margin(20,0,0,0)),
        strip.text = element_text(size = 16, margin = margin(10,10,10,10)),
        legend.title = element_text(size = 16),
        legend.text  = element_text(size = 12),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 16, margin = margin(0,0,40,0)),
        plot.caption = element_text(size = 14, margin = margin(40,0,0,0)),
        plot.background = element_rect(fill = "snow"),
        plot.margin = margin(50,50,50,50))

# Add NSCR logo.
nscr_logo <- png::readPNG("img/nscr_logo_snow.png")

main_plot_logo <- ggdraw() +
  draw_plot(main_plot) + 
  draw_image(nscr_logo, x = 0.35, y = 0.42, scale = 0.2) +
  # draw_label(label = "No data", x = 0.17, y = 0.65, size = 6) +
  # draw_label(label = "No data", x = 0.13, y = 0.60, size = 6) +
  draw_label(label = "The relationship between residential burglary and house prices in Amsterdam",
             x = 0.37, y = 0.925, size = 22) +
  draw_label(label = "Police-recorded residential burglary and average house prices in 2019",
             x = 0.265, y = 0.885, size = 16) +
  draw_label(label = "Police data obtained from data.politie.nl | Average prices from CBS | Author: Samuel Langton",
             x = 0.7, y = 0.08, size = 12)

# Save graphic in relevant subject folders.
ggsave(filename = ("visuals/ams_burglary.svg"), height = 29.7, width = 42, unit = "cm")
