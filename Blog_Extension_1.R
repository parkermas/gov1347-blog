library(tidyverse)
library(ggplot2)
library(usmap)
library(dplyr)

# Reading in popular vote data provided on Canvas
popvote <- read_csv("popvote_1948-2016.csv")
popvote_state <- read_csv("popvote_bystate_1948-2016.csv")
popvote_state$full <- popvote_state$state

# Generated the US map shapefile
states_map <- usmap::us_map()
unique(states_map$abbr)

# Created new column that shows if the Democrat or Republican recieved more votes 
(popvote_state <- popvote_state %>% 
    mutate(winner = case_when(D > R ~ "D",
                              TRUE ~ "R")))

# Sample state popular vote map from 2016
popvote_state_2016 <- popvote_state %>%
  filter(year == 2016)

plot_usmap(data = popvote_state_2016, regions = "states", values = "winner") +
  scale_fill_manual(values = c("blue", "red"), name = "state PV winner") +
  theme_void()

# State popular vote win margins map from 2012
pv_margins_map3 <- popvote_state %>%
  mutate(win_margin = (D_pv2p - R_pv2p)) %>%
  filter(year == c(2012))

plot_usmap(data = pv_margins_map3, regions = "states", values = "win_margin") +
  scale_fill_gradient2(
    high = "blue", 
    mid = "white",
    low = "red", 
    breaks = c(-50,-25,0,25,50), 
    limits = c(-50,50),
    name = "Margin of Victory"
  ) +
  theme_void() +
  labs(title = "2012 Presidential Election Results",
       subtitle  = "Colored by Magnitude of Partisan Win Margin") +
  theme(panel.border    = element_blank(),
        plot.title      = element_text(size = 20, hjust = 0.5, face="bold"), 
        plot.subtitle = element_text(size = 15, hjust = 0.5, face="bold"),
        plot.caption = element_text(size = 8, hjust = 0.5),
        strip.text      = element_text(size = 18, face = "bold"))


# State popular vote win margins map from 2016
pv_margins_map2 <- popvote_state %>%
  mutate(win_margin = (D_pv2p - R_pv2p)) %>%
  filter(year == c(2016))

plot_usmap(data = pv_margins_map2, regions = "states", values = "win_margin") +
  scale_fill_gradient2(
    high = "red", 
    mid = "white",
    low = "blue", 
    breaks = c(-50,-25,0,25,50), 
    limits = c(-50,50),
    name = "Margin of Victory"
  ) +
  theme_void() +
  labs(title = "2016 Presidential Election Results",
       subtitle  = "Colored by Magnitude of Partisan Win Margin") +
  theme(panel.border    = element_blank(),
        plot.title      = element_text(size = 20, hjust = 0.5, face="bold"), 
        plot.subtitle = element_text(size = 15, hjust = 0.5, face="bold"),
        plot.caption = element_text(size = 8, hjust = 0.5),
        strip.text      = element_text(size = 18, face = "bold"))

# Creating swing map for 2016
pv_margins_map <- popvote_state

margins_ext <- pv_margins_map %>%
  filter(year == 2012 | year ==2016)

margins_spread <- margins_ext %>%
  spread(key = year, value = D_pv2p)

 
margins_ext_2 <- margins_spread %>%
  mutate(lead_2016 = lead(`2016`),
         lag_2016 = lag(margins_spread$`2016`),
         diff_margin = lead_2016 - `2012`,
         swing_states = R_pv2p - `2016`)

five_points <- margins_ext_2 %>%
  mutate(diff_margin_2 = lag(diff_margin)) %>%
  filter(swing_states <= 5 & swing_states >= -5)

ten_points <- margins_ext_2 %>%
  mutate(diff_margin_2 = lag(diff_margin)) %>%
  filter(swing_states <= 10 & swing_states >= -10)

  
plot_usmap(data = five_points, regions = "states", values = "diff_margin_2") +
  scale_fill_gradient2(
    high = "blue",
    mid = "white",
    low = "red",
    na.value = "white",
    limits = c(-7, 7),
    breaks = c(-7, -5, -3, -1, 0, 1, 3, 5, 7),
    name = "Partisan Shift"
  ) +
  labs(title = "2016 Presidential Election Swing States",
       subtitle  = "Colored by Magnitude of Partisan Shift",
       caption = "Note: Intensity of Color Represents Partisan Swing") +
  theme(panel.border    = element_blank(),
        plot.title      = element_text(size = 20, hjust = 0.5, face="bold"), 
        plot.subtitle = element_text(size = 15, hjust = 0.5, face="bold"),
        plot.caption = element_text(size = 8, hjust = 0.5),
        strip.text      = element_text(size = 18, face = "bold"))


# Creating swing state map for 2012 
margins_ext_2012 <- pv_margins_map %>%
  filter(year == 2008 | year ==2012)

margins_spread_2012 <- margins_ext_2012 %>%
  spread(key = year, value = D_pv2p)


margins_ext_2_2012 <- margins_spread_2012 %>%
  mutate(lead_2012 = lead(`2012`),
         lag_2012 = lag(margins_spread_2012$`2012`),
         diff_margin = lead_2012 - `2008`,
         swing_states = R_pv2p - `2012`)

five_points_2012 <- margins_ext_2_2012 %>%
  mutate(diff_margin_2 = lag(diff_margin)) %>%
  filter(swing_states <= 5 & swing_states >= -5)


plot_usmap(data = five_points_2012, regions = "states", values = "diff_margin_2") +
  scale_fill_gradient2(
    high = "blue",
    mid = "white",
    low = "red",
    na.value = "white",
    limits = c(-3, 3),
    breaks = c(-3, -2, -1, 0, 1, 2, 3),
    name = "Partisan Shift"
  ) +
  labs(title = "2012 Presidential Election Swing States",
       subtitle  = "Colored by Magnitude of Partisan Shift",
       caption = "Note: Intensity of Color Represents Partisan Swing") +
  theme(panel.border    = element_blank(),
        plot.title      = element_text(size = 20, hjust = 0.5, face="bold"), 
        plot.subtitle = element_text(size = 15, hjust = 0.5, face="bold"),
        plot.caption = element_text(size = 8, hjust = 0.5),
        strip.text      = element_text(size = 18, face = "bold"))

