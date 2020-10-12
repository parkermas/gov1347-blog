library(tidyverse)
library(ggplot2)
library(gridExtra)
library(gt)
library(data.table)
library(geofacet)

# Reading in data
campaigns_df <- read_csv("ad_campaigns_2000-2012.csv")
creative_df <- read_csv("ad_creative_2000-2012.csv")
ads_2020_df <- read_csv("ads_2020.csv")
vep_df <- read_csv("vep_1980-2016.csv")
pvstate_df <- read_csv("popvote_bystate_1948-2016.csv")
pollstate_df <- read_csv("pollavg_bystate_1968-2016.csv")
polls2020_df <- read_csv("polls_2020.csv")
president_polls <- read_csv("president_polls.csv")

# Ads data
ads_2020_df <- ads_2020_df %>%
  filter(period_startdate == "2020-09-05")

# VEP data minus DC and national data
vep_df <- vep_df %>%
  filter(state != "United States") %>%
  filter(state != "District of Columbia") %>%
  mutate(vep = as.numeric(VEP))


# Combining the popular vote and state poll datasets 
pvstate_polls_df <- pvstate_df %>%
  inner_join(
    pollstate_df %>%
      filter(weeks_left == 5))


# Calculating percentage of D and R popular vote
pvstate_polls_df$D_pv <- (pvstate_polls_df$D / pvstate_polls_df$total) * 100
pvstate_polls_df$R_pv <- (pvstate_polls_df$R / pvstate_polls_df$total) * 100


# Joining with VEP and ads datasets
poll_pvstate_vep_df <- pvstate_polls_df %>%
  inner_join(vep_df) %>%
  filter(year != 1976)



# Tried to create polling averages, but not having success. Going to manually enter FiveThirtyEight values. 
polls2020_df$start_date <- as.Date(polls2020_df$start_date, format = "%m/%d/%y")
polls2020_df$end_date <- as.Date(polls2020_df$end_date, format = "%m/%d/%y")

polls2020_df <- polls2020_df %>%
  filter(start_date >= "2020-09-03") %>%
  group_by(state, candidate_name) %>%
  summarize(pct_mean = mean(pct))


president_polls$start_date <- as.Date(president_polls$start_date, format = "%m/%d/%y")
president_polls$end_date <- as.Date(president_polls$end_date, format = "%m/%d/%y")

president_polls <- president_polls %>%
  filter(start_date >= "2020-09-03") %>%
  group_by(state, candidate_name) %>%
  summarize(pct_mean = mean(pct))
  
president_polls <- president_polls %>%
  filter(candidate_name == "Donald Trump" | candidate_name == "Joseph R. Biden Jr.")


polling_2020 <- data.frame(number = 1:100,
                           state = state.name) %>%
  arrange(state) %>%
  select(-number) 
  
polling_2020$party <- c("R", "D")

polling_2020$avgs <- c(56.7, 38.8, 50.1, 45.8, 44.8, 48.7, 55.7, 40.1, 30.9, 62.3, 39.9, 51.9, 34.0, 58.7, 34.1, 59.0, 44.6, 49.1, 46.7, 47.5, 31.7, 62.4, 59.9, 36.0, 38.2, 56.4, 52.7, 41.2, 46.9, 48.0, 50.1, 43.4, 56.9, 38.9, 53.7, 39.9, 39.3, 54.5, 30.6, 62.8, 28.7, 65.9, 43.2, 51.2, 41.1, 50.3, 53, 40.9, 50.9, 45.4, 51.8, 43.7, 53, 47,43.3, 50.1, 42.7, 53.5, 36.9, 56.0, 40.2, 54.3, 31.6, 62.4, 46.1, 49.0, 57.9, 36.2, 46.4, 47.1, 58.8, 35.2, 37.7, 57.0, 43.8, 51.0, 41, 56, 50.0, 44.6, 59, 38,54.6, 41.1, 48.3, 46.8, 51.7, 39.2, 30.3, 62.1, 40.3, 52.9, 33.7, 61.6, 61.1, 35.5, 43.2, 50.4, 73, 27)

state_list <- unique(polling_2020$state)

poll_pvstate_vep_df$state2<- state.name[match(poll_pvstate_vep_df$state, state.abb)]

predictions <- tibble(state_name = c(), dem_p= c(), rep_p= c())

# Running a for loop to create the binomial regression models, collect recent polling averages, make initial predictions, and then run 10,000 simulations to create distributions
for (s in state_list) {
  
  republicans <- poll_pvstate_vep_df %>%
    filter(party == "republican", state == s)
  
  democrats <- poll_pvstate_vep_df %>%
    filter(party == "democrat", state == s)
  
  republican_glm <- glm(cbind(R, vep - R) ~ avg_poll, republicans, family = "binomial")
  democrat_glm <- glm(cbind(D, vep - D) ~ avg_poll, democrats, family = "binomial")
  
  republican_poll <- polling_2020 %>%
    filter(state == s,
           party == "R") %>%
    select(avgs) %>%
    pull()

  democrat_poll <- polling_2020 %>%
    filter(state == s,
           party == "D") %>%
    select(avgs) %>%
    pull()
  
  rep_prediction <- predict(republican_glm,
                            newdata = data.frame(avg_poll = republican_poll),
                            type = "response")[[1]]
  dem_prediction <- predict(democrat_glm,
                            newdata = data.frame(avg_poll = democrat_poll),
                            type = "response")[[1]]
  
  VEP_state <- vep_df %>%
    filter(year == 2016, state == s) %>%
    select(vep) %>%
    pull()
  
  D_sims_2020 <- rbinom(n = 10000, size = VEP_state, prob = dem_prediction)
  R_sims_2020 <- rbinom(n = 10000, size = VEP_state, prob = rep_prediction)

  # Adding to the predictions object
  predictions <- predictions %>%
    add_row(state_name = s, dem_p = D_sims_2020, rep_p = R_sims_2020)
}

# Adding state abbreviations column 
predictions$state_abb<- state.abb[match(predictions$state_name, state.name)]

# Calculating Dem win margins and then coding based on win/loss
predictions$win_margin_D = (predictions$dem_p - predictions$rep_p)/(predictions$dem_p + predictions$rep_p) *100

predictions <- predictions %>%
  mutate(Outcome = case_when(win_margin_D > 0 ~ "Democratic Win",
                             win_margin_D < 0 ~ "Democratic Loss"))

# Creating geofacet plot of all the results (for some reason, several states are missing)
predictions %>%
  mutate(win_margin_D = (dem_p - rep_p)/(dem_p + rep_p) *100) %>%
  ggplot(aes(x = win_margin_D, fill = Outcome)) +
  geom_histogram(bins = 100) +
  facet_geo(~state_abb, scales = "free_x") +
  labs(title = "Predicted 2020 Democratic Win Margins by State",
       subtitle = "Based on 10,000 Simulations of a Binomial Regression Model",
       x = "Predicted Win Margin (%)",
       fill = "Race Outcomes") +
  theme(axis.text.x = element_text(angle = 45, size = 6),
        axis.text.y = element_text(size = 8))


# Selecting key states to analyze more deeply

## Texas

VEP_tx <- vep_df %>%
  filter(year == 2016) %>%
  filter(state == "Texas") %>%
  select(VEP) %>%
  pull() %>%
  as.integer()

TX_rep <- poll_pvstate_vep_df %>% filter(party == "republican", state == "Texas")
TX_dem <- poll_pvstate_vep_df %>% filter(party == "democrat", state == "Texas")

tx_rep_glm <- glm(cbind(R, VEP - R) ~ avg_poll, TX_rep, family = "binomial")
tx_dem_glm <- glm(cbind(D, VEP - D) ~ avg_poll, TX_dem, family = "binomial")

prob_rep_tx <- predict(tx_rep_glm, newdata = data.frame(avg_poll = 48.3), type = "response")[[1]]
prob_dem_tx <- predict(tx_dem_glm, newdata = data.frame(avg_poll = 46.8), type = "response")[[1]]

sim_rep_tx <- rbinom(n = 10000, size = VEP_tx, prob = prob_rep_tx)
sim_dem_tx <- rbinom(n = 10000, size = VEP_tx, prob = prob_dem_tx)

tx_margins <- ((sim_dem_tx - sim_rep_tx)/(sim_dem_tx + sim_rep_tx) *100)

hist(tx_margins, xlab = "Predicted Democratic Win Margin in Texas (%)",
     main = "2020 Texas Democratic Predicted Win Margin")


## Arizona

VEP_az <- vep_df %>%
  filter(year == 2016) %>%
  filter(state == "Arizona") %>%
  select(VEP) %>%
  pull() %>%
  as.integer()

AZ_rep <- poll_pvstate_vep_df %>% filter(party == "republican", state == "Arizona")
AZ_dem <- poll_pvstate_vep_df %>% filter(party == "democrat", state == "Arizona")

az_rep_glm <- glm(cbind(R, VEP - R) ~ avg_poll, AZ_rep, family = "binomial")
az_dem_glm <- glm(cbind(D, VEP - D) ~ avg_poll, AZ_dem, family = "binomial")

prob_rep_az <- predict(az_rep_glm, newdata = data.frame(avg_poll = 44.8), type = "response")[[1]]
prob_dem_az <- predict(az_dem_glm, newdata = data.frame(avg_poll = 48.7), type = "response")[[1]]

sim_rep_az <- rbinom(n = 10000, size = VEP_az, prob = prob_rep_az)
sim_dem_az <- rbinom(n = 10000, size = VEP_az, prob = prob_dem_az)

az_margins <- ((sim_dem_az - sim_rep_az)/(sim_dem_az + sim_rep_az) *100)

hist(az_margins, xlab = "Predicted Democratic Win Margin in Arizona (%)",
     main = "2020 Arizona Democratic Predicted Win Margin")

## Florida

VEP_fl <- vep_df %>%
  filter(year == 2016) %>%
  filter(state == "Florida") %>%
  select(VEP) %>%
  pull() %>%
  as.integer()

FL_rep <- poll_pvstate_vep_df %>% filter(party == "republican", state == "Florida")
FL_dem <- poll_pvstate_vep_df %>% filter(party == "democrat", state == "Florida")

fl_rep_glm <- glm(cbind(R, VEP - R) ~ avg_poll, FL_rep, family = "binomial")
fl_dem_glm <- glm(cbind(D, VEP - D) ~ avg_poll, FL_dem, family = "binomial")

prob_rep_fl <- predict(fl_rep_glm, newdata = data.frame(avg_poll = 44.6), type = "response")[[1]]
prob_dem_fl <- predict(fl_dem_glm, newdata = data.frame(avg_poll = 49.1), type = "response")[[1]]

sim_rep_fl <- rbinom(n = 10000, size = VEP_fl, prob = prob_rep_fl)
sim_dem_fl <- rbinom(n = 10000, size = VEP_fl, prob = prob_dem_fl)

fl_margins <- ((sim_dem_fl - sim_rep_fl)/(sim_dem_fl + sim_rep_fl) *100)

hist(fl_margins, xlab = "Predicted Democratic Win Margin in Florida (%)",
     main = "2020 Florida Democratic Predicted Win Margin")

