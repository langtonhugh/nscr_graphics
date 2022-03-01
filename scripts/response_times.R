# Load packages.
library(here)
library(cbsodataR)
library(dplyr)
library(janitor)
library(stringr)
library(tidyr)
library(haven)
library(forcats)
library(purrr)
library(ggplot2)
library(sf)
library(png)
library(cowplot)

# TOC.
# cbs_get_catalogs()

# Search.
politie_df <- cbs_search("Politie")

# Get relevant survey. You can view the data online:
# https://opendata.cbs.nl/statline/#/CBS/nl/dataset/81928NED/table?ts=1646036578337.
citpol_df <- cbs_get_data("81928NED")

# Explore.
glimpse(citpol_df)
unique(citpol_df$Perioden) # no 2018.

# Get province and nationwide figures.
citpol_pd_df <- citpol_df %>% 
  clean_names() %>% 
  zap_label() %>% 
  mutate(regio_s = trimws(regio_s)) %>% 
  filter(str_detect(regio_s, "PV|NL"))

# Check. 12 provinces and 1 nationwide.
unique(citpol_pd_df$regio_s)

# Initial clean.
citpol_pd_clean_df <- citpol_pd_df %>% 
  rename(pol_niet_snel = komt_niet_snel_als_je_ze_roept_71) %>% 
  select(marges, regio_s, perioden, pol_niet_snel) %>% 
  group_split(marges) %>%
  bind_cols() %>%
  clean_names() %>% 
  rename(regio = regio_s_2, 
         perioden = perioden_3,
         pol_niet_snel_est = pol_niet_snel_8,
         pol_niet_snel_ci  = pol_niet_snel_4) %>% 
  select(regio, perioden, pol_niet_snel_est, pol_niet_snel_ci) %>% 
  mutate(perioden = str_remove_all(perioden, "JJ00"))
         
# Load in province names.
# https://www.cbs.nl/nl-nl/cijfers/detail/84378NED?q=financi%C3%ABn%20provincies.
prov_df <- readr::read_csv("data/table__84378NED.csv")

# Get codes and province names.
prov_clean_df <- prov_df %>% 
  clean_names() %>% 
  rename(regio      = lokaliseringen_van_gemeenten_provincies_code_code,
         regio_naam = provincies_naam_naam) %>% 
  distinct(regio, regio_naam)

# Join names to existing data and add nationwide label.
citpol_pd_provs_df <- citpol_pd_clean_df %>%  
  left_join(prov_clean_df) %>% 
  mutate(regio_naam = replace_na(regio_naam, "Nationwide"))

# Who is worst?
regio_order <- citpol_pd_provs_df %>% 
  filter(perioden == "2012", str_detect(regio, "PV")) %>% 
  arrange(desc(pol_niet_snel_est)) %>% 
  select(regio_naam) %>%
  pluck("regio_naam")

# Reorder factor to match the start point level.
citpol_pd_provs_df <- citpol_pd_provs_df %>% 
  mutate(regio_naam = fct_relevel(regio_naam, as.character(regio_order)))

# Initial plot.
# citpol_pd_provs_df %>%
#   filter(str_detect(regio, "PV")) %>%
#   ggplot(data = .) +
#   geom_ribbon(mapping = aes(x = perioden,
#                             group = regio_naam,
#                             ymax = pol_niet_snel_est+pol_niet_snel_ci,
#                             ymin = pol_niet_snel_est-pol_niet_snel_ci),
#               alpha = 0.5) +
#   geom_line(mapping = aes(x = perioden,
#                           y = pol_niet_snel_est,
#                           group  = regio_naam)) +
#   facet_wrap(~regio_naam, nrow = 3)

# For now, we filter out the nationwide trend.
citpol_provs_only_df <- citpol_pd_provs_df %>% 
  filter(regio_naam != "Nationwide") %>% 
  mutate(regio_naam = factor(regio_naam)) # removes the empty factor level.

# Main line plot.
line_plots <- ggplot(data = citpol_provs_only_df) +
  geom_ribbon(mapping = aes(x = perioden,
                              group = regio_naam,
                              ymax = pol_niet_snel_est+pol_niet_snel_ci,
                              ymin = pol_niet_snel_est-pol_niet_snel_ci),
                alpha = 0.1,
                fill = "#B1005D") +
  geom_line(mapping = aes(x = perioden,
                            y = pol_niet_snel_est,
                            group  = regio_naam),
            col = "#B1005D") +
  scale_y_continuous(breaks = c(15, 30), labels = c("15%","30%"), limits = c(10, 30)) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~regio_naam, nrow = 3) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 90, vjust = 0.5, size = 12),
    axis.text.y  = element_text(size = 12),
    axis.title.y = element_text(size = 12 , margin = margin(0,20,0,0)),
    axis.title.x = element_text(size = 12 , margin = margin(20,0,0,0)),
    strip.text   = element_text(size = 14 , margin = margin(10,10,10,10)),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 10)
    )

# # Split into list.
# citpol_provs_only_list <- citpol_provs_only_df %>% 
#   group_split(regio_naam)
# 
# # Run through list.
# line_plots_list <- lapply(citpol_provs_only_list, line_plot_fun)
# 
# Name elements in the plot list.
# names(line_plots_list) <- levels(citpol_provs_only_df$regio_naam)

# Load in boundaries for maps.
# https://maps.princeton.edu/catalog/stanford-st293bj4601.
prov_sf <- st_read("data/stanford-st293bj4601-shapefile/st293bj4601.shp")

# Project to Amersfoort.
prov_sf <- st_transform(prov_sf, crs = 28992)

# Plot gemoetry.
# plot(st_geometry(prov_sf))

# Remove water polygons, leaving the 12 provinces. Relevel the factor to match order of line plots.
prov_clean_sf <- prov_sf %>% 
  filter(type_1 != "Water body") %>% 
  rename(province = name_1) %>% 
  mutate(province = fct_relevel(province, levels(citpol_provs_only_df$regio_naam)))

# Plot gemoetries to check it worked.
# plot(st_geometry(prov_sf), col = "blue")
# plot(st_geometry(prov_clean_sf), add = T, col = "darkgreen")

# Split sf into list by province.
prov_clean_list <- prov_clean_sf %>%
  mutate(province = fct_relevel(province, levels(citpol_provs_only_df$regio_naam))) %>%
  group_split(province)

# Create nationwide template for the facet.
# nl_sf <- prov_clean_sf %>%
#   select(-province)  # This helps it ignore the facet wrap.
# 
# # Create map for each province.
# maps_sf <- ggplot() +
#   geom_sf(data = nl_sf, colour = "black", fill = "snow", size = 0.1, alpha = 0.2) +
#   geom_sf(data = prov_clean_sf, mapping = aes(fill = province), fill = "black", colour = "black", alpha = 0.2) +
#   facet_wrap(~province) +
#   theme_void() +
#   theme(strip.text = element_blank())


# myplots3 <- prov_clean_sf %>% 
#   split(ceiling(group_indices(.,province)/12)) %>% 
#   map(~ggplot() +
#         # geom_sf(data = nl_sf, colour = "black", fill = "snow", size = 0.1) +
#         geom_sf(data = prov_clean_sf, mapping = aes(fill = province), fill = "black", colour = "black") +
#         facet_wrap(~province) +
#         theme_void() +
#         theme(strip.text = element_blank()))

# Create map for each province.
maps_list <- lapply(prov_clean_list, function(x){
  ggplot() +
    geom_sf(data = prov_clean_sf, colour = "black", fill = "snow", size = 0.1) +
    geom_sf(data = x, fill = "black", colour = "black") +
    theme_void() +
    coord_sf(xlim = c(13895.64, 277998.5), ylim = c(303925.3, 619270.2), expand = FALSE) +
    theme(strip.text = element_blank())
})

# Add names.
# names(maps_list) <- names(line_plots_list)
maps_list_cow  <- plot_grid(plotlist = maps_list, nrow = 3, scale = 0.2)
                            

# Arrange.
main_plot <- ggdraw() +
  draw_plot(line_plots) +
  draw_plot(maps_list_cow, x = 0.07, y = 0.125)

# Load logo.
nscr_logo <- readPNG(here("static/img/logos/nscr_logo_snow.png"))

# Combine graphic and logo, and add titles and captions.
full_plot <- ggdraw() +
  # Plot the graphic.
  draw_plot(main_plot) +
  # Update the theme.
  theme(plot.background = element_rect(fill = "snow"),
        plot.margin = margin(t = 150, r = 70, b = 100, l = 70)) +
  # Add the NSCR logo.
  draw_image(nscr_logo, x = 0.35, y = 0.62, scale = 0.25) +
  # Add the main title.
  draw_text(text = "Citizen satisfaction with police response times",
            x = 0, y = 1.15, size = 24, hjust = 0) +
  # Add the subtitle.
  draw_text(text = "Percentage (strongly) agreeing with the statement: 'The police don't come quickly when you call them'",
            x = 0, y = 1.10, size = 16, hjust = 0) +
  # Add the caption in the bottom right.
  draw_text(text = "No data collected in 2018 | CBS StatLine 'Burgers en politie' | Author: Samuel Langton",
            x = 0.99, # Right-hand legend? 1 should be fine. No legend? Slight adjustment (e.g., 0.99).
            y = -0.1, size = 10, hjust = 1) 

# Save.
ggsave(filename = here("static/img/police_response/police_response_mdsave.svg"), dpi = 300,
       # width = 29.7, height = 42, unit = "cm") # portrait
       height = 29.7, width = 42, unit = "cm") # landscape
