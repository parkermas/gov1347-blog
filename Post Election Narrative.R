library(tidyverse)
library(ggplot2)
library(gridExtra)
library(gt)
library(data.table)
library(geofacet)
library(rnaturalearth)
library(sf)
library(rnaturalearthdata)
library(janitor)
library(FedData)
library(usmap)
library(gridExtra)

popvote_2020 <- read_csv("popvote_bycounty_2020.csv")
popvote_df <- read_csv("popvote_bycounty_2000-2016.csv")
popvote_bystate <- read_csv("popvote_bystate_1948-2020.csv")

popvote_2020$`Total Vote` <- as.numeric(popvote_2020$`Total Vote`)
popvote_2020$`Joseph R. Biden Jr.`<- as.numeric(popvote_2020$`Joseph R. Biden Jr.`)
popvote_2020$`Donald J. Trump`<- as.numeric(popvote_2020$`Donald J. Trump`)


popvote_2020 <- popvote_2020 %>%
  select(FIPS, `Geographic Name`, `Geographic Subtype`, `Total Vote`, `Joseph R. Biden Jr.`, `Donald J. Trump`) %>%
  mutate(d_pct = (`Joseph R. Biden Jr.`/`Total Vote`) * 100,
         r_pct = (`Donald J. Trump`/`Total Vote`) * 100,
         d_pv2p = (`Joseph R. Biden Jr.`/(`Joseph R. Biden Jr.` + `Donald J. Trump`))*100,
         r_pv2p = (`Donald J. Trump`/(`Joseph R. Biden Jr.` + `Donald J. Trump`))*100,
         d_pv2p_win = d_pv2p - r_pv2p)
  
popvote_2020$year <- 2020

popvote_2020 <- popvote_2020[-1,]
popvote_2020$fips <- popvote_2020$FIPS 
popvote_2020$D_win_margin <- popvote_2020$d_pv2p_win
popvote_2020$county <- popvote_2020$`Geographic Name`


popvote_1 <- popvote_df %>%
  select(year, county, fips, D_win_margin)


popvote_2 <- popvote_2020 %>%
  select(year, county, fips, D_win_margin)

popvote_total <- rbind(popvote_1, popvote_2)

plot_usmap(data = popvote_total, regions = "counties", values = "D_win_margin")

popvote_total <- popvote_total %>%
  mutate(partisan_lean = case_when(
    D_win_margin < 0 ~ "Republican",
    D_win_margin > 0 ~ "Democrat"))

popvote_total <- popvote_total %>%
  group_by(county, fips) %>%
  mutate(D_shift = (D_win_margin) - lag(D_win_margin)) %>%
  view()
  

popvote_total %>%
  filter(county == "Baldwin")

popvote_total <- popvote_total %>%
  group_by(county, fips) %>%
  mutate(D_since_2000 = (D_win_margin) - lag(D_win_margin, n = 5))
  


rep_2020 <- popvote_total %>%
  group_by(county, fips) %>%
  filter(year == 2020 & partisan_lean == "Republican")
  
rep_2000 <- popvote_total %>%
  group_by(county, fips) %>%
  filter(year == 2000 & partisan_lean == "Republican")

mean_rep <- inner_join(rep_2020, rep_2000, by = c("county", "fips")) %>%
  select(year.x, county, fips, D_win_margin.x, partisan_lean.x, D_since_2000.x) %>%
  summarize(mean_D_since_2000 = mean(D_since_2000.x))

mean(mean_rep$mean_D_since_2000)

plot_usmap(data = mean_rep, regions = "counties", values = "mean_D_since_2000") +
  scale_color_gradient2(low = "red",
                        mid = "white",
                        high = "blue",
                        midpoint = 0,
                        guide = "colourbar",
                        aesthetics = "fill",
                        name = "Change in D Win Margin Since 2000") +
  labs(title = "Shift in Dem Win Margin Since 2000",
       subtitle = "In Solid Republican Counties")


dem_2020 <- popvote_total %>%
  group_by(county, fips) %>%
  filter(year == 2020 & partisan_lean == "Democrat")

dem_2000 <- popvote_total %>%
  group_by(county, fips) %>%
  filter(year == 2000 & partisan_lean == "Democrat")

mean_dem <- inner_join(dem_2020, dem_2000, by = c("county", "fips")) %>%
  select(year.x, county, fips, D_win_margin.x, partisan_lean.x, D_since_2000.x) %>%
  summarize(mean_D_since_2000 = mean(D_since_2000.x))

mean(mean_dem$mean_D_since_2000)

plot_usmap(data = mean_dem, regions = "counties", values = "mean_D_since_2000") +
  scale_color_gradient2(low = "red",
                        mid = "white",
                        high = "blue",
                        midpoint = 0,
                        guide = "colourbar",
                        aesthetics = "fill",
                        name = "Change in Dem Win Margin Since 2000") +
  labs(title = "Shift in D Win Margin Since 2000",
       subtitle = "In Solid Democratic Counties")











popvote_2004 <- popvote_total %>%
  ungroup() %>%
  filter(partisan_lean == "Democrat") %>%
  filter(year == 2020) %>%
  summarize(mean_d = mean(D_win_margin, na.rm = TRUE))

tibble(Year = c(2004, 2008, 2012, 2016, 2020),
           `D Margin Shift in R Counties` = c(-6.35, -3.13, -8.94, -10.5, -5.34),
           `D Margin Shift in D Counties` = c(16.4, 21.8, 16.4, 19.7, 23.5),
           `Mean D Margin in R Counties` = c(-30, -28.6, -33.1, -41.8, -43.8),
           `Mean D Margin in D Counties` = c(15.9, 18.9, 20.9, 23.2, 23.3),
           `Gap Between Mean D Margin in D and R Counties` = c(45.9, 47.5, 54, 64, 67.1)) %>%
  gt() %>%
  tab_header(title = "Partisan Polarization by County")



baseline 2000 R = -26.1 
baseline 2000 D = 15.2

plot_usmap(data = popvote_2020, regions = "counties", values = "D_win_margin") +
  scale_color_gradient2(low = "red",
                        mid = "white",
                        high = "blue",
                        midpoint = 0,
                        limits = c(-100, 100),
                        breaks = c(-100, -50, 0 , 50 ,100),
                        guide = "colourbar",
                        aesthetics = "fill",
                        name = "Democratic Win Margin") +
  labs(title = "Presidential Election 2020 Democratic Win Margins",
       subtitle = "By County")



popvote_bystate_2020 <- popvote_bystate %>%
  filter(year == 2020) %>%
  mutate(margin = abs((D_pv2p - R_pv2p)*100),
         D_pv2p = D_pv2p*100,
         R_pv2p = R_pv2p*100)
  
popvote_bystate <- popvote_bystate %>%
  filter(year != 2020) %>%
  mutate(margin = abs(D_pv2p - R_pv2p))

test <- rbind(popvote_bystate, popvote_bystate_2020) %>%
  filter(margin <= 5) %>%
  count(year) %>%
  filter(year <= 1980) %>%
  summarize(mean_y = mean(n))
  drop_na() %>%
  filter(!is.na(state))


plot_usmap(data = na.omit(test), regions = "states", values = "margin") +
  scale_color_gradient2(low = "green", high = "darkgreen", aesthetics = "fill", name = "Margin of Victory") +
  facet_wrap(~ year, drop = T, scales = "fixed") +
  theme(legend.position = "right") +
  labs(title = "Battleground States by Election Year",
       subtitle = "States Where Margin of Victory < 5 Pct. Points")

lm(D_win_margin ~ , data = popvote_total)