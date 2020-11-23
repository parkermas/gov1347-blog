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

popvote_state_df <- read_csv("popvote_bystate_1948-2020.csv")
popvote_df <- read_csv("popvote_1948-2020.csv")

predictions

plot <- popvote_state_df %>%
  select(state, year, D_pv2p, R_pv2p)%>%
  filter(year == 2020) %>%
  inner_join(predictions) %>%
  mutate(d_error = abs((100*D_pv2p)-dem_p),
         r_error = abs((100*R_pv2p)-rep_p)) %>%
  select(state, D_pv2p, R_pv2p, dem_p, rep_p, d_error, r_error) %>%
  mutate(actual_winner = case_when(D_pv2p > R_pv2p ~ "Democrat",
                                   D_pv2p < R_pv2p ~ "Republican"),
         predicted_winner = case_when(dem_p > rep_p ~ "Democrat",
                                      dem_p < rep_p ~ "Republican"))



  
  
plot_usmap(data = plot, regions = "states", values = "d_error", labels = TRUE) +
  scale_fill_gradient2(
    high = "red",
    mid = "white",
    low = "green",
    breaks = c(0, 3, 6, 9, 12),
    limits = c(0, 12),
    name = "Error in Predicted Vote Share") +
  labs(title = "Error in Democratic Vote Share Prediction",
       subtitle = "By State")

plot_usmap(data = plot, regions = "states", values = "r_error", labels = TRUE) +
  scale_fill_gradient2(
    high = "red",
    mid = "white",
    low = "green",
    breaks = c(0, 3, 6, 9, 12),
    limits = c(0, 12),
    name = "Error in Predicted Vote Share") +
  labs(title = "Error in Republican Vote Share Prediction",
       subtitle = "By State")


all_states <- plot %>% 
  summarize(avg_d = mean(d_error),
            avg_r = mean(r_error)) 

all_states$category <- "All States"

swing_states <- plot %>% 
  filter(state == "Arizona" | state == "Texas" | state == "Georgia" | state == "North Carolina" | 
              state == "Florida" | state == "Ohio" | state == "Pennsylvania" | state == "Michigan" |
              state == "Wisconsin" | state == "Iowa" | state == "Nevada" | state == "New Hampshire" |
              state == "Minnesota") %>%
  summarize(avg_d = mean(d_error),
            avg_r = mean(r_error))

swing_states$category <- "Battleground States"


nonswing_states <- plot %>% 
  filter(state != "Arizona"  & state != "Texas" & state != "Georgia" & state != "North Carolina" & 
           state != "Florida" & state != "Ohio" & state != "Pennsylvania" & state != "Michigan" &
           state != "Wisconsin" & state != "Iowa" & state != "Nevada" & state != "New Hampshire" &
           state != "Minnesota") %>%
  summarize(avg_d = mean(d_error),
            avg_r = mean(r_error))

nonswing_states$category <- "Non-Battleground States"

error_categories <- rbind(all_states, swing_states, nonswing_states)

error_categories <- error_categories %>%
  select(category, avg_d, avg_r)

error_categories %>%
  mutate(`Category` = category,
         `Democratic Error` = round(avg_d, 2),
         `Republican Error` = round(avg_r, 2)) %>%
  select(-category, -avg_d, -avg_r) %>%
  gt() %>%
  tab_header(title = "Mean Dem. and Rep. Error",
             subtitle = "Battleground vs. Non-Battleground States")

actual_map <- plot_usmap(data = plot, regions = "states", values = "actual_winner", labels = TRUE) +
  scale_fill_discrete(type =  c("blue", "red")) +
  labs(title = "2020 Presidential Election Actual Electoral College Outcome",
       fill = "Winner")



plot_usmap(data = plot, regions = "states", values = "predicted_winner", labels = TRUE) +
  scale_fill_discrete(type =  c("blue", "red")) +
  labs(title = "2020 Presidential Election Actual Electoral College Outcome",
       fill = "Winner")

plot$ev <- c(9, 3, 11, 6, 55, 9, 7, 3, 29, 16, 4, 4, 20, 11, 6, 6, 8, 8, 4, 10, 11, 16,
                    10, 6, 10, 3, 5, 6, 4, 14, 5, 29, 15, 3, 18, 7, 7, 20, 4, 9, 3, 11, 38, 6, 3, 13, 12,
                    5, 10, 3)

actual_bargraph <- plot %>%
  group_by(actual_winner) %>%
  summarize(totals = sum(ev)) 

actual_bargraph[1, 2] = actual_bargraph[1, 2] + 3


prediction_bargraph <- plot %>%
  group_by(predicted_winner) %>%
  summarize(totals = sum(ev))

prediction_bargraph[1, 2] = prediction_bargraph[1, 2] + 3

pred <- prediction_bargraph %>%
  ggplot(aes(x = predicted_winner, y = totals, fill = predicted_winner)) +
  geom_col() +
  geom_text(aes(label = totals), vjust = -0.5) +
  geom_hline(yintercept = 270, color = "gold") +
  scale_fill_manual(values=c("blue", "red")) +
  labs(title = "Predicted Electoral Vote Share by Party",
       x = "Candidate Party",
       y = "Electoral College Votes") +
  theme(legend.position = "none")


act <- actual_bargraph %>%
  ggplot(aes(x = actual_winner, y = totals, fill = actual_winner)) +
  geom_col() +
  geom_text(aes(label = totals), vjust = -0.5) +
  geom_hline(yintercept = 270, color = "gold") +
  scale_fill_manual(values=c("blue", "red")) +
  labs(title = "Actual Electoral Vote Share by Party",
       x = "Candidate Party",
       y = "Electoral College Votes") +
  theme(legend.position = "none")

plot %>% 
  mutate(d_margin = round((D_pv2p - R_pv2p)*100, 2),
         dp_margin = round(dem_p - rep_p, 2),
         margin_error = round(abs(d_margin - dp_margin), 2)) %>%
  select(state, d_margin, dp_margin, margin_error) %>%
  filter(state == "Arizona" | state == "Texas" | state == "Georgia" | state == "North Carolina" | 
           state == "Florida" | state == "Ohio" | state == "Pennsylvania" | state == "Michigan" |
           state == "Wisconsin" | state == "Iowa" | state == "Nevada" | state == "New Hampshire" |
           state == "Minnesota") %>%
  arrange(d_margin, dp_margin, margin_error) %>%
  mutate(State = state,
         `Actual Win Margin` = d_margin,
         `Predicted Win Margin` = dp_margin,
         `Total Error` = margin_error) %>% 
  arrange(margin_error) %>%
  select(-state, -d_margin, -dp_margin, -margin_error) %>% 
  gt() %>% 
  tab_header(title = "Actual vs. Predicted Dem. Win Margins",
             subtitle = "In Battleground States") %>% 
  tab_style(
    style = list(
      cell_fill(color = "#F9BFAF")
    ),
    locations = cells_body(
      rows = c(11, 12))
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#AFF9D1")
    ),
    locations = cells_body(
      rows = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 13))
  )


demog <- big_df %>% filter(year == 2016, state == "Georgia" | state == "Arizona" | state == "Florida" | state == "Wisconsin" | state == "Michigan" | state == "Pennsylvania") %>%
 filter(candidate_name == "Donald Trump") %>%
 select(state, white_chng, black_chng, hisp_chng) %>%
  ungroup() %>% 
  select(-state, -year, -candidate_name)%>%
  round(2)


states <- big_df %>% filter(year == 2016, state == "Georgia" | state == "Arizona" | state == "Florida" | state == "Wisconsin" | state == "Michigan" | state == "Pennsylvania") %>%
  filter(candidate_name == "Donald Trump") %>%
  select(state) %>% 
  ungroup() %>%
  select(-year, -candidate_name)

merge(states, demog, by = "row.names") %>%
  select(-Row.names) %>%
  arrange(desc(black_chng)) %>%
  mutate(`State` = state,
         `Change in White Population` = white_chng,
         `Change in Black Population` = black_chng,
         `Change in Hispanic Population` = hisp_chng) %>%
  select(-state, -white_chng, -black_chng, -hisp_chng) %>%
  select(`State`, `Change in Black Population`, `Change in White Population`, `Change in Hispanic Population`) %>%
  gt() %>%
  tab_style(
    style = list(
      cell_fill(color = "#F9BFAF")
    ),
    locations = cells_body(
      rows = c(1, 2))
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#AFF9D1")
    ),
    locations = cells_body(
      rows = c(3, 4, 5, 6))
  ) %>% 
  tab_header(title = "Demographic Change in Key Battleground States",
             subtitle = "Percentage Point Change Since 2016")
