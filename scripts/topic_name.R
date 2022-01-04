# Load packages.
library(ggplot2)
library(cowplot)

# All your data handling.

# Crate graphic.
basic_plot <- ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, colour = class), size = 4) +
  scale_colour_manual(values = c("#b1005d",  # nscr default purple
                                 "#ad1158",  # nscr similar purple
                                 "#841446",  # nscr darker purple
                                 "#a0a0a0",  # nscr light grey
                                 "#5b5b5b",  # nscr dark grey
                                 "#dc5050",  # nscr red/orange
                                 "#edb362")) # nscr yellow
# "#242943"   # non-nscr blue/purple

# Add labels and theme.
main_plot <- basic_plot +
  # Labels.
  labs(x = "X label (or NULL) ", y = "Y label (or NULL)") +
  # Theme.
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12, margin = margin(0,20,0,0)),
        axis.title.x = element_text(size = 12, margin = margin(20,0,0,0)),
        strip.text = element_text(size = 14, margin = margin(10,10,10,10)),
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 10) )


# Add NSCR logo.
nscr_logo <- png::readPNG(here::here("img/nscr_logo_snow.png"))

full_plot <- ggdraw() +
  # Plot the graphic.
  draw_plot(main_plot) +
  # Update the theme.
  theme(plot.background = element_rect(fill = "snow"),
        plot.margin = margin(t = 150, r = 50, b = 100, l = 50)) +
  # Add the NSCR logo.
  draw_image(nscr_logo, x = 0.35, y = 0.65, scale = 0.25) +
  # Add the main title.
  draw_text(text = "Police response times across Dutch municipalities",
            x = -0.01, y = 1.17, size = 24, hjust = 0) +
  # Add the subtitle.
  draw_text(text = "Average percent of responses within 15 minutes.",
            x = -0.01, y = 1.13, size = 16, hjust = 0) +
  # Add the caption in the bottom right.
  draw_text(text = "No data collected in 2018 | State the data source | Author: Samuel Langton",
            x = 0.68, # Use 0.72 if you have a legend. Or, adjust as appropriate.
            y = -0.1, size = 10, hjust = 0) 


# Save graphic.
ggsave(filename = here("visuals/topic_name.svg"),
       # width = 29.7, height = 42, unit = "cm") # portrait
       height = 29.7, width = 42, unit = "cm") # landscape
