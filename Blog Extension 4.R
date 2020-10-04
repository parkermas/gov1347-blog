library(tidyverse)
library(ggplot2)
library(gridExtra)
library(gt)
library(data.table)

popvote_df <- read_csv("popvote_1948-2016.csv")
economy_df <- read_csv("econ.csv")
approval_df <- read_csv("approval_gallup_1941-2020.csv")
poll_df    <- read_csv("pollavg_1968-2016.csv")
polls_2020_df <- read_csv("polls_2020.csv")


# Rebuilding my most 'successful' model
complete_df <- popvote_df %>% 
  full_join(poll_df %>% 
              filter(weeks_left == 6) %>% 
              group_by(year,party) %>% 
              summarise(avg_support=mean(avg_support))) %>% 
  left_join(economy_df %>% 
              filter(quarter == 2))

complete_econ <- unique(complete_df[!is.na(complete_df$GDP_growth_qt),])
complete_econ_inc <- complete_econ[complete_econ$incumbent_party,]
complete_econ_chl <- complete_econ[!complete_econ$incumbent_party,]

wtA = .5
wtB = .3
wtless = .2


polls_2020_df_A <-
  polls_2020_df %>%
  filter(fte_grade == "A+" | fte_grade == "A" | fte_grade == "A-")

Apolls_trump2 <-polls_2020_df_A %>%
  filter(candidate_name == "Donald Trump") %>%
  summarize(pct_mean = mean(pct))

Apolls_biden2 <-polls_2020_df_A %>%
  filter(candidate_name == "Joseph R. Biden Jr.") %>%
  summarize(pct_mean = mean(pct))

polls_2020_df_B <- 
  polls_2020_df %>%
  filter(fte_grade == "B+" | fte_grade == "B" | fte_grade == "B-" | fte_grade == "B/C" | fte_grade == "A/B")

Bpolls_trump2 <-polls_2020_df_B %>%
  filter(candidate_name == "Donald Trump") %>%
  summarize(pct_mean = mean(pct))

Bpolls_biden2 <-polls_2020_df_B %>%
  filter(candidate_name == "Joseph R. Biden Jr.") %>%
  summarize(pct_mean = mean(pct))

polls_2020_df_less <-
  polls_2020_df %>%
  filter(fte_grade == "C" | fte_grade == "C-" | fte_grade == "C/D" | fte_grade == "C+" | fte_grade == "D-" | fte_grade == "D+" | fte_grade == "F")

Lpolls_trump2 <-polls_2020_df_A %>%
  filter(candidate_name == "Donald Trump") %>%
  summarize(pct_mean = mean(pct))

Lpolls_biden2 <-polls_2020_df_A %>%
  filter(candidate_name == "Joseph R. Biden Jr.") %>%
  summarize(pct_mean = mean(pct))

polls_wt_trump2 = (wtA * Apolls_trump2$pct_mean) + (wtB * Bpolls_trump2$pct_mean) + (wtless * Lpolls_trump2$pct_mean)
polls_wt_biden2 = (wtA * Apolls_biden2$pct_mean) + (wtB * Bpolls_biden2$pct_mean) + (wtless * Lpolls_biden2$pct_mean)

mod_econ_inc2 <- lm(pv2p ~ GDP_growth_qt, data = complete_econ_inc)
mod_econ_chl2 <- lm(pv2p ~ GDP_growth_qt, data = complete_econ_chl)

summary(mod_econ_inc2)
summary(mod_econ_chl2)
mean(mod_econ_inc2$residuals^2)


gdpgrowth_2020_2 <- data.frame(GDP_growth_qt = -9.494715859)

pred_econ_2020_inc <- predict(mod_econ_inc2, newdata = gdpgrowth_2020_2)
pred_econ_2020_chl <- predict(mod_econ_chl2, newdata = gdpgrowth_2020_2)

w1 = .7
w2 = .3

# My GDP + Weighted poll averages model 
pred_2020_chl <- (polls_wt_biden2 * w1) + (pred_econ_2020_chl * w2)
pred_2020_inc <- (polls_wt_trump2 * w1) + (pred_econ_2020_inc * w2)

# Building the time for change model dataset
tfc_df <- popvote_df %>%
  filter(incumbent_party) %>%
  select(year, candidate, party, pv, pv2p, incumbent) %>%
  inner_join(
    approval_df %>% 
      group_by(year, president) %>% 
      slice(1) %>% 
      mutate(net_approve=approve-disapprove) %>%
      select(year, incumbent_pres=president, net_approve, poll_enddate),
    by="year"
  ) %>%
  inner_join(
    economy_df %>%
      filter(quarter == 2) %>%
      select(GDP_growth_qt, RDI_growth, year),
    by="year"
  )

tfc_chl_df <- popvote_df %>%
  filter(!incumbent_party) %>%
  select(year, candidate, party, pv, pv2p, incumbent) %>%
  inner_join(
    approval_df %>% 
      group_by(year, president) %>% 
      slice(1) %>% 
      mutate(net_approve=approve-disapprove) %>%
      select(year, incumbent_pres=president, net_approve, poll_enddate),
    by="year"
  ) %>%
  inner_join(
    economy_df %>%
      filter(quarter == 2) %>%
      select(GDP_growth_qt, RDI_growth, year),
    by="year"
  )
# Visualizing incumbency and the popular vote
tfc_df %>%
  ggplot(aes(x=year, y=pv2p,
             label = candidate)) +
  geom_point() +
  ylim(40, 65)+
  geom_text(aes(label = year), hjust = 0, nudge_x = 0.5, nudge_y = .5, size = 3.5) +
  geom_hline(aes(yintercept = 52, color = "red")) +
  labs(title = "Incumbent Party Two-Party Vote Share, 1948-2016",
       x = "Year", 
       y = "Percentage of Two-Party Voteshare Recieved") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = .5, size = 14),
        legend.position = "none")

tfc_df %>%
  select(pv2p) %>%
  summarize(mean = mean(pv2p, na.rm = TRUE))


tfc_chl_df %>%
  ggplot(aes(x = year, y = pv2p)) +
  geom_point() +
  ylim(40, 65)+
  geom_text(aes(label = year), hjust = 0, nudge_x = 0.5, nudge_y = .5, size = 3.5) +
  geom_hline(aes(yintercept = 48, color = "red")) +
  labs(title = "Challenger Party Two-Party Vote Share, 1948-2016",
       x = "Year", 
       y = "Percentage of Two-Party Voteshare Recieved") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = .5, size = 14),
        legend.position = "none")

tfc_chl_df %>%
  select(pv2p) %>%
  summarize(mean = mean(pv2p, na.rm = TRUE))

# Fitting the time for change model
tfc_model = lm(formula = pv2p ~ GDP_growth_qt + net_approve + incumbent, data = tfc_df)
summary(tfc_model)  
mean(tfc_model$residuals^2)


tfc_model2 = lm(formula = pv2p ~ RDI_growth + net_approve + incumbent, data = tfc_df)
summary(tfc_model2)  
mean(tfc_model2$residuals^2)


tfc_model3 = lm(formula = pv2p ~ RDI_growth + GDP_growth_qt + net_approve + incumbent, data = tfc_df)
summary(tfc_model3)
mean(tfc_model3$residuals^2)
# 2020 data
data_2020 <- data.frame(GDP_growth_qt = -9.494715859, incumbent = TRUE, net_approve = -19)
data_2020_2 <- data.frame(RDI_growth = 0.0972422967, incumbent = TRUE, net_approve = -19)
data_2020_3 <- data.frame(incumbent = TRUE, net_approve = -19)


# TFC model prediction 
pred_tfc_model <- predict(tfc_model, newdata = data_2020)  
pred_tfc_model  


pred_tfc_model2 <- predict(tfc_model2, newdata = data_2020_2)  
pred_tfc_model2

pred_tfc_model3 <- predict(tfc_model3, newdata = data_2020_3)  
pred_tfc_model3

gt = data.table(
  Number = 1:4,
  Model = c("Weighted Avg. Polling + Q2 GDP","Time for Change Model (GDP)", "Time for Change Model (RDI)", "Time for Change Model (No economic indicator)"),
  `Incumbent PV share 2020 Prediction` = c("37%", "31.3%", "68.2%", "48.7%"),
  `Challenger PV share 2020 Prediction` = c("63%", "68.7%", "31.8%", "51.3%"),
  `R-Squared Value` = c("0.28", "0.62", "0.53", "0.53"), 
  `Mean Squared Error`= c("17.7", "8.3", "9.5", "10.8")
) %>%
  gt() %>%
  cols_align(align="center") %>%
  tab_header(title = "Model Summaries") %>%
tab_style(
    style = list(
      cell_fill(color = "lightblue"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = vars(Number),
      rows = Number == 1)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "orange"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = vars(Number),
      rows = Number == 2)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "green"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = vars(Number),
      rows = Number == 3)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "purple"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = vars(Number),
      rows = Number == 4)
  )




gt
 
# two party vote share with Trump Predictions
tfc_df %>%
  ggplot(aes(x=year, y=pv2p)) +
  geom_point() +
  geom_text(aes(label = year), hjust = 0, nudge_x = 0.5, nudge_y = .5, size = 3.5) +
  geom_point(aes(x=2020, y=37), colour="blue") +
  geom_point(aes(x=2020, y=31.3), colour="orange") +
  geom_point(aes(x=2020, y=68.2), colour="green") +
  geom_point(aes(x=2020, y=48.7), colour="purple") +
  labs(title = "Incumbent Party Two-Party Vote Share with 2020 Predictions",
       subtitle = "Historical Data from 1948-2016",
       x = "Year", 
       y = "Percentage of Two-Party Voteshare Recieved") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = .5, size = 14),
        plot.subtitle = element_text(hjust = .1735))


