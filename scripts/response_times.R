# Load packages.
library(cbsodataR)
library(dplyr)
library(janitor)
library(stringr)
library(tidyr)
library(haven)
library(forcats)
library(purrr)
library(ggplot2)

# TOC.
cbs_get_catalogs()

# Search.
politie_df <- cbs_search("Politie")

# Get one.
citpol_df <- cbs_get_data("81928NED", Periods = has_substring("JJ"))

# Get nationwide figure.
citpol_pd_df <- citpol_df %>% 
  clean_names() %>% 
  zap_label() %>% 
  mutate(regio_s = trimws(regio_s),
         pd      = str_detect(regio_s, "RE|NL")) %>% 
  filter(pd == TRUE) %>% 
  rename(pol_quick_call = komt_niet_snel_als_je_ze_roept_71) %>% 
  select(marges, regio_s, perioden, pol_quick_call) %>% 
  group_split(marges) %>%
  bind_cols() %>%
  clean_names() %>% 
  rename(regio = regio_s_2, 
         perioden = perioden_3,
         pol_quick_call_est = pol_quick_call_8,
         pol_quick_call_ci = pol_quick_call_4) %>% 
  select(regio, perioden, pol_quick_call_est, pol_quick_call_ci) %>% 
  mutate(perioden = str_remove_all(perioden, "JJ00"),
         regio_naam = fct_recode(regio,
                                 `Northern Netherlands`   = "RE01",
                                 `East Netherlands`       = "RE02",
                                 `Central Netherlands`    = "RE03",
                                 `North Holland`          = "RE04",
                                 `Amsterdam`              = "RE05",
                                 `The Hague`              = "RE06",
                                 `Rotterdam`              = "RE07",
                                 `Zeeland - West Brabant` = "RE08",
                                 `East Brabant`           = "RE09",
                                 `Limburg`                = "RE10"))

# Who is worst?
region_order <- citpol_pd_df %>% 
  filter(perioden == "2012", regio != "NL") %>% 
  arrange(desc(pol_quick_call_est)) %>% 
  select(regio_naam) %>%
  pluck(1)

# Reorder factor to match the start point level.
citpol_pd_df <- citpol_pd_df %>% 
  mutate(regio_naam = fct_relevel(regio_naam, as.character(region_order)))
  
# Test plot.
ggplot() + 
  theme_minimal() +
  geom_ribbon(data = filter(citpol_pd_df, str_detect(regio, "RE")),
              mapping = aes(x = perioden, group  = regio_naam,
                            ymax = pol_quick_call_est+pol_quick_call_ci, ymin = pol_quick_call_est-pol_quick_call_ci),
              fill = "#B1005D", alpha = 0.1) +
  geom_line(data = filter(citpol_pd_df, str_detect(regio, "RE")),
            mapping = aes(x = perioden, y = pol_quick_call_est, group  = regio_naam), col = "#B1005D") +
  facet_wrap(~regio_naam, nrow = 2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_y_continuous(breaks = c(12, 29), labels = c("10%", "30%"), limits = c(10, 35)) +
  labs(x = NULL, y = NULL, title = "Citizen satisfaction with police response times",
       subtitle = "Percentage (strongly) agreeing with the statement: 'The police don't come quickly when you call them'",
       caption = "\n No data collected in 2018")

# Save.
ggsave(filename = "visuals/police_response_satisfaction_agree.png", height = 12, width = 22, unit = "cm")

         