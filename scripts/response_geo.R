# Load packages.
library(here)
library(cbsodataR)
library(dplyr)
library(sf)
library(ggplot2)
library(lubridate)
library(stringr)
library(cowplot)
library(png)
# All your data handling.
# Get municipalities map data from PDOK
municipalities_sf <- st_read("https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_gemeente_2021_gegeneraliseerd&outputFormat=json",quiet=TRUE)

# Query police-recorded response time data from CBS
politie_df<-cbs_get_datasets(catalog="Politie")
prio1_df<-cbs_get_data(id="47008NED",catalog="Politie")

NL_prio1_df <- prio1_df %>% filter(RegioS=="NL01  ") %>%
  # create a date-class for time
  cbs_add_date_column()

# Plot trendline.
trend <-
  NL_prio1_df %>% filter(Perioden_freq=="M") %>%
  ggplot() +
  # monthly measurements as points 
  geom_point(aes(x = Perioden_Date, y = Totaal_2),
             size = 2,
             show.legend = FALSE) +
  # and a line to 'guide the eye'
  geom_line(aes(x = Perioden_Date, y = Totaal_2),
            size = 1,
            show.legend = FALSE) +
  # Add vertical dashed lines every January
  geom_vline(
    xintercept =
      as.numeric(seq(
        from = as.Date("2015-01-01"),
        to = as.Date("2021-01-01"), by = "year"
      )),
    linetype = 2
  ) +
  scale_x_date(
    name = "Period", date_breaks = "1 year",
    date_minor_breaks = "1 month",
    date_labels = "%b    %Y") +
  scale_y_continuous(breaks=seq(18000,30000,3000), limits=c(18000,30000)) +
  xlab("Month") +
  ylab("Police emergency responses") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, hjust=.2))
#trend

mean_month_count <- NL_prio1_df %>% filter(Perioden_freq=="M") %>% select(Totaal_2) %>% summarise(mean_month_count=mean(Totaal_2))
bla<-round(pull(mean_month_count,mean_month_count))

# Create year and month variables
prio1_df$year<-substr(prio1_df$Perioden,1,4)
prio1_df$month<-as.numeric(substr(prio1_df$Perioden,7,8))



# Average response time percentages over regions. Note to self....averaging annual percentages is something else than calculating the percentage over multiple years.
NL_trend_df<-prio1_df %>% mutate(NL=str_detect(RegioS,pattern="NL"),rownumber=row_number()) %>% filter(NL==TRUE) %>% select(Reactietijd0Tot15MinutenRelatief_1,rownumber)
# plot(NL_trend_df$rownumber,NL_trend_df$Reactietijd0Tot15MinutenRelatief_1,type="l",main="Average emergency response time trend",xlab="Time",ylab="Percent within 15 minutes",ylim=c(0,100))
# summary(lm(Reactietijd0Tot15MinutenRelatief_1~rownumber,data=NL_trend_df))

aggr_df<- prio1_df %>%
  filter(year!=2022) %>%
  mutate(gemeente=str_detect(RegioS,pattern="GM")) %>%
  filter(gemeente==TRUE) %>%
  group_by(RegioS) %>%
  mutate(sumscore=sum(Totaal_2,na.rm=TRUE),sumscore_on_target=sum(Reactietijd0Tot15Minuten_3,na.rm=TRUE),percent_on_target=sumscore_on_target/sumscore) %>%
  select(sumscore,sumscore_on_target,percent_on_target) %>% 
  summarise(outcome=100*mean(percent_on_target,na.rm=TRUE)) %>%
  mutate(statcode=RegioS) %>%
  select(statcode,outcome)

# Join the averages with the sf object

final_sf<-left_join(municipalities_sf,aggr_df)

# Create graphic.
basic_plot <- ggplot(data = final_sf) +
  geom_sf(aes(fill=outcome))

# Plot the averages on the map of 2021
# Add labels and theme.
main_plot <- basic_plot +
  # Labels.
  labs(title = "Police response times across Dutch municipalities",
       subtitle = "Give your graphic a subtitle",
       caption = "Add some notes on the graphic | Data: State the data source | Author: Stijn Ruiter") +
  # Theme.
  theme_minimal() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 12, margin = margin(0,20,0,0)),
        axis.title.x = element_text(size = 12, margin = margin(20,0,0,0)),
        strip.text = element_text(size = 14, margin = margin(10,10,10,10)),
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 10),
        plot.title = element_text(size = 18))
        # plot.subtitle = element_text(size = 14, margin = margin(0,0,40,0)),
        # plot.caption = element_text(size = 10, margin = margin(40,0,0,0), hjust = 1.1), # hjust optional depending on legend.
        # plot.background = element_rect(fill = "snow"))
        # plot.margin = margin(50,50,50,50))

# Add logo.
nscr_logo <- readPNG(here("img/nscr_logo_snow.png"))

full_plot <- ggdraw() +
  draw_plot(main_plot) +
  # draw_image(nscr_logo, x = 0.35, y = 0.44, scale = 0.2) # portrait
  draw_image(nscr_logo, x = 0.35, y = 0.42, scale = 0.2) +
  theme(
    plot.background = element_rect(fill = "snow"),
    plot.margin = margin(50,50,50,50)
  )
  
# Save graphic.
ggsave(filename = here("visuals/police_response_times_municipalities.svg"),
       # width = 29.7, height = 42, unit = "cm") # portrait
       height = 29.7, width = 42, unit = "cm") # landscape
