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

# Reading in DAta 
popvote_df <- read_csv("popvote_1948-2016.csv")
state_popvote_df <- read_csv("popvote_bystate_1948-2016.csv")
state_popvote_df2 <- state_popvote_df %>%
  mutate(D_pv = D*100/total,
  R_pv = R*100/total) %>%
  group_by(state) %>%
  mutate(lag_rpv2p = lag(R_pv2p),
         lag_dpv2p = lag(D_pv2p),
         lag_rpv = lag(R_pv),
         lag_dpv = lag(D_pv)) %>%
  filter(year > 1948)


pollavg_df <- read_csv("pollavg_1968-2016.csv")
state_pollavg_df <- read_csv("pollavg_bystate_1968-2016.csv")

demog_state_df <- read_csv("demographic_1990-2018.csv") %>%
  clean_names() %>%
  group_by(state) %>%
  mutate(black_chng = black - lag(black),
         asian_chng = asian - lag(asian),
         hisp_chng = hispanic - lag(hispanic),
         white_chng = white - lag(white),
         female_chng = female - lag(female),
         age20_chng = age20 - lag(age20),
         age3045_chng = age3045 - lag(age3045),
         age4565_chng = age4565 - lag(age4565),
         age65_chng = age65 - lag(age65),
         state = state.name[match(state, state.abb)])


econ_df <- read_csv("econ.csv")
rdi_state <- read_csv("rdi_state.csv", skip = 4) %>%
  mutate(state = GeoName) %>%
  select(-GeoFips, -GeoName) %>%
  head(-5)

# Creating full state by state dataset
rdi_state_list <- read_csv("rdi_state.csv", skip = 4) %>%
  mutate(state = GeoName) %>%
  select(state, -GeoFips, -GeoName) %>%
  head(-5)

rdi_state_df <- merge(rdi_state_list, rdi_state)

rdi_state_list <- sapply(rdi_state_list$state, as.character)
rdi_state_list[2] <- "Alaska"
rdi_state_list[12] <- "Hawaii"

rdi_state_df <- data.frame(sapply(rdi_state_df, as.numeric))

rdi_state_df$state <- rdi_state_list

state_poll_df <- state_pollavg_df %>%
  filter(weeks_left <= 5) %>%
  select(year, state, party, candidate_name, avg_poll) %>%
  group_by(year, state, candidate_name, party) %>%
  summarize(avg_poll = mean(avg_poll))

rdi_state_df[,1:2]

econ2_df <- rdi_state_df[,1:2]
colnames(econ2_df) <- c("state","rdi_q2")
econ2_df$rdi_q2 <- as.numeric(econ2_df$rdi_q2)


for(i in seq(from = 18, to = 290, by = 16)){
  temp <- rdi_state_df[,c(1,i)]
  colnames(temp) <- c("state","rdi_q2")
  temp$rdi_q2 <- as.numeric(temp$rdi_q2)
  econ2_df <- rbind(econ2_df, temp)
}

add_years <- rep(c(1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980,
               1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020), 51)
add_years2 <- c(rep(1948, 51), rep(1952, 51), rep(1956, 51), rep(1960, 51), rep(1964, 51), rep(1968, 51),
                rep(1972, 51), rep(1976, 51), rep(1980, 51), rep(1984,51), rep(1988, 51), rep(1992, 51), rep(1996, 51),
                rep(2000, 51), rep(2004, 51), rep(2008, 51), rep(2012, 51), rep(2016, 51), rep(2020, 51))
  
add_gdp <- econ_df %>% filter(quarter == 2, year >= 1948) %>% select(year, GDP_growth_qt)

# To find election year q2 gdps
add_gdp[seq(1, nrow(add_gdp), 4), ][,2]

# Manually making a list to add to dataframe
add_gdp2 <- rep(c(1.65, 0.215, 0.826, -0.539, 1.09, 1.67, 2.27, 0.734,
                  -2.06, 1.73, 1.31, 1.08, 1.67, 1.83, 0.762, 0.516, 0.430, 0.312, -0.49), 51)

add_gdp3 <- c(rep(1.65, 51), rep(0.215, 51), rep(0.826, 51), rep(-0.539, 51), rep(1.09, 51), rep(1.67, 51),
                rep(2.27, 51), rep(0.734, 51), rep(-2.06, 51), rep(1.73,51), rep(1.31, 51), rep(1.08, 51), rep(1.67, 51),
                rep(1.83, 51), rep(0.762, 51), rep(0.516, 51), rep(0.430, 51), rep(0.312, 51), rep(-0.49, 51))

econ2_df$gdp_q2 <- add_gdp3
econ2_df$year <- add_years2

# Creating my full dataframe
big_df <- state_poll_df %>% left_join(econ2_df, by= c("state", "year")) %>%
  full_join(state_popvote_df2, by=c("state", "year")) %>%
  left_join(popvote_df %>% select(year, party, winner, incumbent, incumbent_party, prev_admin), by= c("year", "party")) %>%
  mutate(inc_party = case_when((party=="democrat" & incumbent_party==TRUE) ~ "democrat",
                                     (party =="republican" & incumbent_party==TRUE) ~ "republican",
                                     (party == "democrat" & incumbent_party==FALSE) ~ "republican",
                                     (party == "republican" & incumbent_party==FALSE) ~ "democrat"),
         challenger_party = case_when((party=="democrat" & incumbent_party==TRUE) ~ "republican",
                                     (party =="republican" & incumbent_party==TRUE) ~ "democrat",
                                     (party == "democrat" & incumbent_party==FALSE) ~ "democrat",
                                     (party == "republican" & incumbent_party==FALSE) ~ "republican"),
         Inc_pv = case_when(inc_party=="republican" ~ R_pv2p,
                            inc_party=="democrat" ~ D_pv2p),
         Chl_pv = case_when(challenger_party == "republican" ~ R_pv2p,
                            challenger_party == "democrat" ~ D_pv2p)) %>%
  mutate(model_type = case_when(state == "Alabama" ~ "Red",
                                state == "Alaska" ~ "Red",
                                state == "Arkansas" ~ "Red",
                                state == "Idaho" ~ "Red",
                                state == "Indiana" ~ "Red",
                                state == "Kansas"~ "Red",
                                state == "Kentucky" ~ "Red",
                                state == "Louisiana" ~ "Red",
                                state == "Mississippi" ~ "Red",
                                state == "Missouri" ~ "Red",
                                state == "Montana" ~ "Red",
                                state == "Nebraska" ~ "Red",
                                state == "North Dakota" ~ "Red",
                                state == "Oklahoma" ~ "Red",
                                state == "South Carolina" ~ "Red",
                                state == "South Dakota" ~ "Red",
                                state == "Tennessee" ~ "Red",
                                state == "Utah" ~ "Red",
                                state == "West Virginia" ~ "Red",
                                state == "Wyoming" ~ "Red",
                                state == "California" ~ "Blue",
                                state == "Colorado" ~ "Blue",
                                state == "Connecticut" ~ "Blue",
                                state == "Delaware" ~ "Blue",
                                state == "Hawaii" ~ "Blue",
                                state == "Illinois" ~ "Blue",
                                state == "Maine" ~ "Blue",
                                state == "Maryland" ~ "Blue",
                                state == "Massachusetts" ~ "Blue",
                                state == "Minnesota" ~ "Blue",
                                state == "Nevada" ~ "Blue",
                                state == "New Jersey" ~ "Blue",
                                state == "New Mexico" ~ "Blue",
                                state == "New York" ~ "Blue",
                                state == "Oregon" ~ "Blue",
                                state == "Rhode Island" ~ "Blue",
                                state == "Vermont" ~ "Blue",
                                state == "Virginia" ~ "Blue",
                                state == "Washington" ~ "Blue",
                                state == "Arizona" ~ "Sun Swing",
                                state == "Florida" ~ "Sun Swing",
                                state == "Georgia" ~ "Sun Swing",
                                state == "Iowa" ~ "Swing",
                                state == "Michigan" ~ "Swing",
                                state == "North Carolina" ~ "Sun Swing",
                                state == "Ohio" ~ "Swing",
                                state == "Pennsylvania" ~ "Swing",
                                state == "Texas" ~ "Sun Swing",
                                state == "Wisconsin" ~ "Swing",
                                state == "New Hampshire" ~ "Swing"))


big_df <- big_df %>%
  left_join(demog_state_df, by=c("year", "state")) 
  
big_df$avg_poll[is.na(big_df$avg_poll)] <- big_df$d[is.na(big_df$avg_poll)]











# Creating and testing model(s)
# Party-based model seems preferable

demog_state_df %>%
  filter(year == 2018, state == "Alaska") %>%
  select(hisp_chng, age20_chng, age65_chng, white_chng)

california_inc <- big_df %>%
  filter(party == "republican", state == "Alaska")



test <- lm(R_pv ~ lag_rpv2p + white_chng  + incumbent_party, data = california_inc)

test_2 <- lm(R_pv ~ avg_poll, data = california_inc)


pollstate_2020 %>%
  group_by(party) %>%
  summarize(mean(avg_pct))

demog_state_df %>%
  filter(year == 2018) %>%
  group_by(year) %>%
  summarize(mean_w = mean(white_chng))

big_df %>%
  filter(year == 2016) %>%
  group_by(year) %>%
  summarize(mean = mean(R_pv2p, na.rm = TRUE))

0.5* (predict(test_2, newdata = data.frame(
  avg_poll = 52.45,
  state = "Alaska")))  + 0.5*(predict(test, newdata = data.frame(avg_poll = 46.5, 
                                   rdi_q2=22.2,
                                   incumbent_party=TRUE,
                                   hisp_chng = 0.500,
                                   age20_chng=-0.180,
                                   age65_chng = 0.263, 
                                   white_chng = -0.441,
                                   lag_rpv2p = 58.4,
                                   state = "Florida",
                                   avg_poll = 50.2), interval = "confidence", level = .95))
summary(test)
summary(test_2)



W_dem <- 56.39
W_rep <- 46.17777

(W_rep) / (W_dem + W_rep)
(W_dem) / (W_dem + W_rep)

Iowa_dem <- 49.98
Iowa_rep <- 48.28

Fl_dem <- 53.701
Fl_rep <- 44.515

big_df %>%
  arrange(state, year) %>%
  group_by(state, year) %>%
  mutate(lag_D_pv = lead(D_pv, n = 2))

# Reading in 2020 poll data

election_2020 <- as.Date("11/3/2020", "%m/%d/%Y")

pollstate_2020 <- read_csv("https://projects.fivethirtyeight.com/polls-page/president_polls.csv") %>%
  drop_na(state) %>%
  mutate(party = case_when(candidate_name == "Donald Trump" ~ "R",
                           candidate_name == "Joseph R. Biden Jr." ~ "D"),
         end_date = mdy(end_date),
         poll_date = as.Date(end_date, "%m/%d/%Y"),
         days_left = round(difftime(election_2020, poll_date, unit="days")),
         weeks_left = round(difftime(election_2020, poll_date, unit="weeks"))) %>%
  filter(!is.na(party))

pollstate_2020 <- pollstate_2020 %>%
  filter(state != "Maine CD-1", state != "Maine CD-2") %>%
  filter(weeks_left <= 2) %>%
  group_by(state, party) %>%
  summarize(avg_pct = mean(pct)) %>%
  view()

# Building loop to make predictions using unpooled model

state_list <- state.name

predictions <- tibble(state_name = c(), dem_p= c(), rep_p= c(), d_rsquared=c(), r_rsquared=c(), d_mse = c(), r_mse = c())

uncertainty <- tibble(state_name = c(), r_polls = c(), r_polls_high = c(), r_polls_low = c(), d_polls = c(), d_polls_high = c(), d_polls_low = c(), r_plus = c(), r_plus_high=c(), r_plus_low = c(), d_plus=c(), d_plus_high=c(), d_plus_low = c())

for (s in state_list) {
  
  republicans <- big_df %>%
    filter(party == "republican", state == s, year >= 1976)
  
  democrats <- big_df %>%
    filter(party == "democrat", state == s, year >= 1976)
  
  republican_lm <- lm(R_pv ~ lag_rpv2p  + white_chng + incumbent_party, data = republicans)
  democrat_lm <- lm(D_pv ~ lag_dpv2p  + white_chng + incumbent_party, data = democrats)
  
  r_rsquared <- summary(republican_lm)$r.squared
  r_mse <- mean(republican_lm$residuals^2)
  
  
  d_rsquared <- summary(democrat_lm)$r.squared
  d_mse <- mean(democrat_lm$residuals^2)
  
  republican_poll <- pollstate_2020 %>%
    filter(state == s,
           party == "R") %>%
    select(avg_pct) %>%
    pull()
  
  republican_lag <- big_df %>%
    filter(party == "republican", state == s, year == 2016) %>%
    select(R_pv2p) %>% pull()
  
  democrat_poll <- pollstate_2020 %>%
    filter(state == s,
           party == "D") %>%
    select(avg_pct) %>%
    pull()
  
  democrat_lag <- big_df %>%
    filter(party == "democrat", state == s, year == 2016) %>%
    select(D_pv2p) %>% pull()
  
  hisp_change_state <- demog_state_df %>%
    filter(state == s,
           year == 2018) %>%
    select(hisp_chng) %>%
    pull()
  
  black_change_state <- demog_state_df %>%
    filter(state == s,
           year == 2018) %>%
    select(black_chng) %>%
    pull()
  
  white_change_state <- demog_state_df %>%
    filter(state == s,
           year == 2018) %>%
    select(white_chng) %>%
    pull()
  
  age20_change_state <- demog_state_df %>%
    filter(state == s,
           year == 2018) %>%
    select(age20_chng) %>%
    pull()
  
  republican_lm2 <- lm(R_pv ~ avg_poll, data = republicans)
  democrat_lm2 <- lm(D_pv ~ avg_poll, data = democrats)
  
  r_rsquared2 <- summary(republican_lm2)$r.squared
  r_mse2 <- mean(republican_lm2$residuals^2)
  
  d_rsquared2 <- summary(democrat_lm2)$r.squared
  d_mse2 <- mean(democrat_lm2$residuals^2)
  
  
  rep_prediction <- predict(republican_lm, newdata = data.frame(avg_poll = republican_poll, 
                                     incumbent_party=TRUE,
                                     hisp_chng = hisp_change_state,
                                     age20_chng=age20_change_state,
                                     black_chng = black_change_state,
                                     white_chng = white_change_state,
                                     lag_rpv2p = republican_lag), interval = "confidence", level = 0.95)
  dem_prediction <- predict(democrat_lm, newdata = data.frame(avg_poll = democrat_poll, 
                                                       incumbent_party=FALSE,
                                                       hisp_chng = hisp_change_state,
                                                       age20_chng=age20_change_state,
                                                       black_chng = black_change_state,
                                                       white_chng = white_change_state,
                                                       lag_dpv2p = democrat_lag), interval = "confidence", level = 0.95)
  
  rep_prediction_low <- rep_prediction[,2]
  rep_prediction_high <- rep_prediction[,3]
  rep_prediction_point <- rep_prediction[,1]
  
  dem_prediction_low <- dem_prediction[,2]
  dem_prediction_high <- dem_prediction[,3]
  dem_prediction_point <- dem_prediction[,1]
  
  rep_prediction2 <- predict(republican_lm2, newdata = data.frame(avg_poll = republican_poll),interval = "confidence", level = 0.95)[,1]
  rep_prediction2_low <- predict(republican_lm2, newdata = data.frame(avg_poll = republican_poll),interval = "confidence", level = 0.95)[,2]
  rep_prediction2_high <- predict(republican_lm2, newdata = data.frame(avg_poll = republican_poll),interval = "confidence", level = 0.95)[,3]
  
  dem_prediction2 <-predict(democrat_lm2, newdata = data.frame(avg_poll = democrat_poll), interval = "confidence", level = 0.95)[,1]
  dem_prediction2_low <-predict(democrat_lm2, newdata = data.frame(avg_poll = democrat_poll), interval = "confidence", level = 0.95)[,2]
  dem_prediction2_high <-predict(democrat_lm2, newdata = data.frame(avg_poll = democrat_poll), interval = "confidence", level = 0.95)[,3]
  
  rep_polls= (rep_prediction2)
  rep_plus= 100*(rep_prediction_point / (rep_prediction_point + dem_prediction_point))
  rep_plus2 = rep_prediction_point
  rep_pv2p = (0.5*rep_polls) + (0.5*rep_plus)
  
  rep_polls_high = (rep_prediction2_high)
  rep_plus_high = 100*(rep_prediction_high / (rep_prediction_high + dem_prediction_high))
  rep_plus_high2 = rep_prediction_high

  
  rep_polls_low = ((rep_prediction2_low))
  rep_plus_low = 100*(rep_prediction_low / (rep_prediction_low + dem_prediction_low))
  rep_plus_low2 = rep_prediction_low

  dem_polls = dem_prediction2
  dem_plus = 100*(dem_prediction_point / (dem_prediction_point + rep_prediction_point))
  dem_plus2 = dem_prediction_point
  dem_pv2p = (0.5*(dem_polls)) + (0.5*dem_plus)
  
  dem_polls_high=dem_prediction2_high
  dem_plus_high = 100*(dem_prediction_high / (dem_prediction_high + rep_prediction_high))
  dem_plus_high2 = dem_prediction_high
  
  dem_polls_low = dem_prediction2_low
  dem_plus_low = 100*(dem_prediction_low / (dem_prediction_low + rep_prediction_low))
  dem_plus_low2 = dem_prediction_low

  predictions <- predictions %>%
    add_row(state_name = s, dem_p = dem_pv2p, rep_p = rep_pv2p, r_rsquared = 0.5*(r_rsquared + r_rsquared2), d_rsquared = 0.5*(d_rsquared + d_rsquared2), d_mse = 0.5*(d_mse + d_mse2), r_mse = 0.5*(r_mse + r_mse2))
  
  uncertainty <- uncertainty %>%
    add_row(state_name = s, r_polls = rep_polls, r_polls_high = rep_polls_high, r_polls_low = rep_polls_low, d_polls = dem_polls, d_polls_high = dem_polls_high, d_polls_low = dem_polls_low, r_plus = rep_plus2, r_plus_high = rep_plus_high2, r_plus_low = rep_plus_low2, d_plus = dem_plus2, d_plus_high = dem_plus_high2, d_plus_low = dem_plus_low2)
}


predictions <- predictions %>% 
  mutate(d_margin = (dem_p - rep_p),
         state = state_name,
         Winner = case_when(dem_p > rep_p ~ "Democrat",
                            rep_p > dem_p ~ "Republican"))

predictions$ev <- c(9, 3, 11, 6, 55, 9, 7, 3, 29, 16, 4, 4, 20, 11, 6, 6, 8, 8, 4, 10, 11, 16,
                    10, 6, 10, 3, 5, 6, 4, 14, 5, 29, 15, 3, 18, 7, 7, 20, 4, 9, 3, 11, 38, 6, 3, 13, 12,
                    5, 10, 3)

predictions %>%
  group_by(Winner) %>%
  summarize(totals = sum(ev))

predictions %>%
  group_by(Winner) %>%
  summarize(totals = sum(ev)) %>%
  ggplot(aes(x=Winner, y=totals, fill = Winner)) +
  geom_col() +
  geom_text(aes(label = totals), vjust = -0.5) +
  geom_hline(yintercept = 270, color = "gold")+
  scale_fill_manual(values=c("blue", "red")) +
  labs(title = "Predicted Electoral Vote Share by Party",
       x = "Candidate Party",
       y = "Electoral College Votes") +
  theme(legend.position = "none")



plot_usmap(data = predictions, regions = "states", values = "d_margin", labels = TRUE) +
  scale_fill_gradient2(
    high = "blue",
    mid = "white",
    low = "red",
    breaks = c(-50, -25, 0, 25, 50),
    limits = c(-50, 50),
    name = "Margin of Victory") +
  labs(title = "Predicted 2020 Electoral College Outcome",
       subtitle = "Using Unpooled Model")

rsq_1 <- predictions %>%
  ggplot(aes(x=d_rsquared)) +
  geom_histogram(bins=10, fill = "blue", color = "black") +
  geom_vline(xintercept = 0.675, color = "gold") +
  labs(title = "Distribution of Average Democratic Model R-Squared Values Across States",
       x = "Average R-Squared Value",
       y = "Number of Observations")

mse_1 <- predictions %>%
  ggplot(aes(x=d_mse)) +
  geom_histogram(bins=20, fill = "blue", color = "black") +
  geom_vline(xintercept = 6.23, color = "gold") +
  labs(title = "Distribution of Average Democratic Model MSE Across States",
       x = "Average Mean Squared Error",
       y = "Number of Observations")

rsq_2 <- predictions %>%
  ggplot(aes(x=r_rsquared)) +
  geom_histogram(bins=10, fill = "red", color = "black") +
  geom_vline(xintercept = 0.665, color = "gold") +
  labs(title = "Distribution of Average Republican Model R-Squared Values Across States",
       x = "Average R-Squared Value",
       y = "Number of Observations")

mse_2 <- predictions %>%
  ggplot(aes(x=r_mse)) +
  geom_histogram(bins=20, fill = "red", color = "black") +
  geom_vline(xintercept = 12.0, color = "gold") +
  labs(title = "Distribution of Average Democratic Model MSE Across States",
       x = "Average Mean Squared Error",
       y = "Number of Observations")

grid.arrange(rsq_1,rsq_2, ncol=1)

grid.arrange(mse_1, mse_2, ncol=1)
  
dem_1 <- uncertainty %>%
  ggplot() +
  geom_point(aes(x = reorder(state_name, d_polls), y = d_polls), color = "blue") +
  geom_errorbar(aes(x = reorder(state_name, d_polls), ymin = d_polls_low, ymax= d_polls_high, width=.1)) +
  geom_hline(yintercept = 50, color = "gold") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Uncertainty in Democratic Polls Regressions by State",
       subtitle = "With a 95% Confidence Interval",
       x = "State",
       y = "Predicted Democratic Vote Share")

dem_2 <- uncertainty %>%
  ggplot() +
  geom_point(aes(x = reorder(state_name, d_plus), y = d_plus), color = "blue") +
  geom_errorbar(aes(x = reorder(state_name, d_plus), ymin = d_plus_low, ymax= d_plus_high, width=.1)) +
  geom_hline(yintercept = 50, color = "gold") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Uncertainty in Democratic Plus Regressions by State",
       subtitle = "With a 95% Confidence Interval",
       x = "State",
       y = "Predicted Democratic Vote Share")


rep_1 <- uncertainty %>%
  ggplot() +
  geom_point(aes(x = reorder(state_name, r_polls), y = r_polls), color = "red") +
  geom_errorbar(aes(x = reorder(state_name, r_polls), ymin = r_polls_low, ymax= r_polls_high, width=.1)) +
  geom_hline(yintercept = 50, color = "gold") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Uncertainty in Republican Polls Regressions by State",
       subtitle = "With a 95% Confidence Interval",
       x = "State",
       y = "Predicted Republican Vote Share")

rep_2 <- uncertainty %>%
  filter(state_name != "Alaska" & state_name != "Mississippi") %>%
  ggplot() +
  geom_point(aes(x = reorder(state_name, r_plus), y = r_plus), color = "red") +
  geom_errorbar(aes(x = reorder(state_name, r_plus), ymin = r_plus_low, ymax= r_plus_high, width=.1)) +
  geom_hline(yintercept = 50, color = "gold") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Uncertainty in Republican Plus Regressions by State",
       subtitle = "With a 95% Confidence Interval",
       x = "State",
       y = "Predicted Republican Vote Share",
       caption = "Note: Alaska and Mississippi excluded for visualization purposes")

dem_1
dem_2

rep_1
rep_2


# Loop to test pooled model

nat_predictions <- tibble(state_name = c(), dem_p= c(), rep_p= c(), d_rsquared=c(), r_rsquared=c(), d_mse = c(), r_mse = c())

nat_uncertainty <- tibble(state_name = c(), r_polls = c(), r_polls_high = c(), r_polls_low = c(), d_polls = c(), d_polls_high = c(), d_polls_low = c(), r_plus = c(), r_plus_high=c(), r_plus_low = c(), d_plus=c(), d_plus_high=c(), d_plus_low = c())

for (s in state_list) {
  
  republicans <- big_df %>%
    filter(party == "republican", year >= 1976)
  
  democrats <- big_df %>%
    filter(party == "democrat", year >= 1976)
  
  nat_republican_lm <- lm(R_pv ~ lag_rpv2p  + white_chng + state + incumbent_party, data = republicans)
  nat_democrat_lm <- lm(D_pv ~ lag_dpv2p  + white_chng + state + incumbent_party, data = democrats)
  
  nat_r_rsquared <- summary(nat_republican_lm)$r.squared
  nat_r_mse <- mean(nat_republican_lm$residuals^2)
  
  
  nat_d_rsquared <- summary(nat_democrat_lm)$r.squared
  nat_d_mse <- mean(nat_democrat_lm$residuals^2)
  
  republican_poll <- pollstate_2020 %>%
    group_by(party) %>%
    summarize(avg_poll = mean(avg_pct)) %>%
    filter(party == "R") %>%
    select(avg_poll) %>%
    pull
  
  republican_lag <- big_df %>%
    filter(year == 2016) %>%
    group_by(year) %>%
    summarize(lag_rpv2p = mean(R_pv2p, na.rm = TRUE)) %>%
    select(lag_rpv2p) %>% pull()
  
  democrat_poll <- pollstate_2020 %>%
    group_by(party) %>%
    summarize(avg_poll = mean(avg_pct)) %>%
    filter(party == "D") %>%
    select(avg_poll) %>%
    pull()
  
  democrat_lag <- big_df %>%
    filter(year == 2016) %>%
    group_by(year) %>%
    summarize(lag_dpv2p = mean(D_pv2p, na.rm = TRUE)) %>%
    select(lag_dpv2p) %>% pull()
  
  hisp_change_state <- demog_state_df %>%
    filter(year == 2018) %>%
    group_by(year) %>%
    summarize(hisp_chng = mean(hisp_chng)) %>%
    select(hisp_chng) %>% pull()
  
  black_change_state <- demog_state_df %>%
    filter(year == 2018) %>%
    group_by(year) %>%
    summarize(black_chng = mean(black_chng)) %>%
    select(black_chng) %>% pull()
  
  white_change_state <- demog_state_df %>%
    filter(year == 2018) %>%
    group_by(year) %>%
    summarize(white_chng = mean(white_chng)) %>%
    select(white_chng) %>% pull()
  
  age20_change_state <- demog_state_df %>%
    filter(state == s,
           year == 2018) %>%
    select(age20_chng) %>%
    pull()
  
  nat_republican_lm2 <- lm(R_pv ~ avg_poll + state, data = republicans)
  nat_democrat_lm2 <- lm(D_pv ~ avg_poll + state, data = democrats)
  
  nat_r_rsquared2 <- summary(nat_republican_lm2)$r.squared
  nat_r_mse2 <- mean(nat_republican_lm2$residuals^2)
  
  nat_d_rsquared2 <- summary(nat_democrat_lm2)$r.squared
  nat_d_mse2 <- mean(nat_democrat_lm2$residuals^2)
  
  
  nat_rep_prediction <- predict(nat_republican_lm, newdata = data.frame(avg_poll = republican_poll, 
                                                                incumbent_party=TRUE,
                                                                hisp_chng = hisp_change_state,
                                                                age20_chng=age20_change_state,
                                                                black_chng = black_change_state,
                                                                white_chng = white_change_state,
                                                                lag_rpv2p = republican_lag,
                                                                state = s), interval = "confidence", level = 0.95)
  nat_dem_prediction <- predict(nat_democrat_lm, newdata = data.frame(avg_poll = democrat_poll, 
                                                              incumbent_party=FALSE,
                                                              hisp_chng = hisp_change_state,
                                                              age20_chng=age20_change_state,
                                                              black_chng = black_change_state,
                                                              white_chng = white_change_state,
                                                              lag_dpv2p = democrat_lag,
                                                              state = s), interval = "confidence", level = 0.95)
  
  nat_rep_prediction_low <- nat_rep_prediction[,2]
  nat_rep_prediction_high <- nat_rep_prediction[,3]
  nat_rep_prediction_point <- nat_rep_prediction[,1]
  
  nat_dem_prediction_low <- nat_dem_prediction[,2]
  nat_dem_prediction_high <- nat_dem_prediction[,3]
  nat_dem_prediction_point <- nat_dem_prediction[,1]
  
  nat_rep_prediction2 <- predict(nat_republican_lm2, newdata = data.frame(avg_poll = republican_poll,
                                                                          state = s),interval = "confidence", level = 0.95)[,1]
  nat_rep_prediction2_low <- predict(nat_republican_lm2, newdata = data.frame(avg_poll = republican_poll,
                                                                              state=s),interval = "confidence", level = 0.95)[,2]
  nat_rep_prediction2_high <- predict(nat_republican_lm2, newdata = data.frame(avg_poll = republican_poll,
                                                                               state=s),interval = "confidence", level = 0.95)[,3]
  
  nat_dem_prediction2 <-predict(nat_democrat_lm2, newdata = data.frame(avg_poll = democrat_poll,
                                                                       state = s), interval = "confidence", level = 0.95)[,1]
  nat_dem_prediction2_low <-predict(nat_democrat_lm2, newdata = data.frame(avg_poll = democrat_poll,
                                                                           state=s), interval = "confidence", level = 0.95)[,2]
  nat_dem_prediction2_high <-predict(nat_democrat_lm2, newdata = data.frame(avg_poll = democrat_poll,
                                                                            state=s), interval = "confidence", level = 0.95)[,3]
  
  nat_rep_polls= (nat_rep_prediction2)
  nat_rep_plus= 100*(nat_rep_prediction_point / (nat_rep_prediction_point + nat_dem_prediction_point))
  nat_rep_plus2 = nat_rep_prediction_point
  nat_rep_pv2p = (0.5*nat_rep_polls) + (0.5*nat_rep_plus)
  
  nat_rep_polls_high = (nat_rep_prediction2_high)
  nat_rep_plus_high = 100*(nat_rep_prediction_high / (nat_rep_prediction_high + nat_dem_prediction_high))
  nat_rep_plus_high2 = nat_rep_prediction_high
  
  
  nat_rep_polls_low = ((nat_rep_prediction2_low))
  nat_rep_plus_low = 100*(nat_rep_prediction_low / (nat_rep_prediction_low + nat_dem_prediction_low))
  nat_rep_plus_low2 = nat_rep_prediction_low
  
  nat_dem_polls = nat_dem_prediction2
  nat_dem_plus = 100*(nat_dem_prediction_point / (nat_dem_prediction_point + nat_rep_prediction_point))
  nat_dem_plus2 = nat_dem_prediction_point
  nat_dem_pv2p = (0.5*(nat_dem_polls)) + (0.5*nat_dem_plus)
  
  nat_dem_polls_high= nat_dem_prediction2_high
  nat_dem_plus_high = 100*(nat_dem_prediction_high / (nat_dem_prediction_high + nat_rep_prediction_high))
  nat_dem_plus_high2 = nat_dem_prediction_high
  
  nat_dem_polls_low = nat_dem_prediction2_low
  nat_dem_plus_low = 100*(nat_dem_prediction_low / (nat_dem_prediction_low + nat_rep_prediction_low))
  nat_dem_plus_low2 = nat_dem_prediction_low
  
  nat_predictions <- nat_predictions %>%
    add_row(state_name = s, dem_p = nat_dem_pv2p, rep_p = nat_rep_pv2p, r_rsquared = 0.5*(nat_r_rsquared + nat_r_rsquared2), d_rsquared = 0.5*(nat_d_rsquared + nat_d_rsquared2), d_mse = 0.5*(nat_d_mse + nat_d_mse2), r_mse = 0.5*(nat_r_mse + nat_r_mse2))
  
  nat_uncertainty <- nat_uncertainty %>%
    add_row(state_name = s, r_polls = nat_rep_polls, r_polls_high = nat_rep_polls_high, r_polls_low = nat_rep_polls_low, d_polls = nat_dem_polls, d_polls_high = nat_dem_polls_high, d_polls_low = nat_dem_polls_low, r_plus = nat_rep_plus2, r_plus_high = nat_rep_plus_high2, r_plus_low = nat_rep_plus_low2, d_plus = nat_dem_plus2, d_plus_high = nat_dem_plus_high2, d_plus_low = nat_dem_plus_low2)
}

nat_predictions <- nat_predictions %>% 
  mutate(d_margin = (dem_p - rep_p),
         state = state_name,
         Winner = case_when(dem_p > rep_p ~ "Democrat",
                            rep_p > dem_p ~ "Republican"))

nat_predictions$ev <- c(9, 3, 11, 6, 55, 9, 7, 3, 29, 16, 4, 4, 20, 11, 6, 6, 8, 8, 4, 10, 11, 16,
                    10, 6, 10, 3, 5, 6, 4, 14, 5, 29, 15, 3, 18, 7, 7, 20, 4, 9, 3, 11, 38, 6, 3, 13, 12,
                    5, 10, 3)

nat_predictions %>%
  group_by(Winner) %>%
  summarize(totals = sum(ev))

nat_predictions %>%
  group_by(Winner) %>%
  summarize(totals = sum(ev)) %>%
  ggplot(aes(x=Winner, y=totals, fill = Winner)) +
  geom_col() +
  geom_text(aes(label = totals), vjust = -0.5) +
  geom_hline(yintercept = 270, color = "gold")+
  scale_fill_manual(values=c("blue", "red")) +
  labs(title = "Predicted Electoral Vote Share by Party")+
  labs(title = "Predicted Electoral Vote Share by Party",
       x = "Candidate Party",
       y = "Electoral College Votes") +
  theme(legend.position = "none")



plot_usmap(data = nat_predictions, regions = "states", values = "d_margin", labels = TRUE) +
  scale_fill_gradient2(
    high = "blue",
    mid = "white",
    low = "red",
    breaks = c(-50, -25, 0, 25, 50),
    limits = c(-50, 50),
    name = "Margin of Victory") +
  labs(title = "Predicted 2020 Electoral College Outcome",
       subtitle = "Using Pooled Model")

nat_predictions %>%
  ggplot(aes(x=d_rsquared)) +
  geom_histogram(bins=10, color = "blue")

predictions %>%
  ggplot(aes(x=d_mse)) +
  geom_histogram(bins=20, color = "blue")

predictions %>%
  ggplot(aes(x=r_rsquared)) +
  geom_histogram(bins=10, color = "red")

predictions %>%
  ggplot(aes(x=r_mse)) +
  geom_histogram(bins=20, color = "red")


# Loop to test pooled model out of sample fit

nat_predictions_oos <- tibble(state_name = c(), dem_p= c(), rep_p= c(), actual_d= c(), actual_r=c(), d_rsquared=c(), r_rsquared=c(), d_mse = c(), r_mse = c())

nat_uncertainty_oos <- tibble(state_name = c(), r_polls = c(), r_polls_high = c(), r_polls_low = c(), d_polls = c(), d_polls_high = c(), d_polls_low = c(), r_plus = c(), r_plus_high=c(), r_plus_low = c(), d_plus=c(), d_plus_high=c(), d_plus_low = c())

for (s in state_list) {
  
  republicans_oos <- big_df %>%
    filter(party == "republican", year >= 1976 & year != 2016)
  
  democrats_oos <- big_df %>%
    filter(party == "democrat", year >= 1976 & year != 2016)
  
  nat_republican_lm_oos <- lm(R_pv ~ lag_rpv2p  + white_chng + state + incumbent_party, data = republicans_oos)
  nat_democrat_lm_oos <- lm(D_pv ~ lag_dpv2p  + white_chng + state + incumbent_party, data = democrats_oos)
  
  nat_r_rsquared_oos <- summary(nat_republican_lm_oos)$r.squared
  nat_r_mse_oos <- mean(nat_republican_lm_oos$residuals^2)
  
  
  nat_d_rsquared_oos <- summary(nat_democrat_lm_oos)$r.squared
  nat_d_mse_oos <- mean(nat_democrat_lm_oos$residuals^2)
  
  republican_poll_oos <- big_df %>%
    filter(party == "republican", year == 2016, state == s) %>%
    select(avg_poll) %>%
    pull()
  
  republican_lag_oos <- big_df %>%
    filter(year == 2012) %>%
    group_by(year) %>%
    summarize(lag_rpv2p = mean(R_pv2p, na.rm = TRUE)) %>%
    select(lag_rpv2p) %>% pull()
  
  republican_actual_oos = big_df %>%
    filter(party == "republican", year == 2016, state == s) %>%
    select(R_pv2p) %>% pull()
  
  democrat_poll_oos <- big_df %>%
    filter(party == "democrat", year == 2016, state == s) %>%
    select(avg_poll) %>%
    pull()
  
  democrat_lag_oos <- big_df %>%
    filter(year == 2012) %>%
    group_by(year) %>%
    summarize(lag_dpv2p = mean(D_pv2p, na.rm = TRUE)) %>%
    select(lag_dpv2p) %>% pull()
  
  democrat_actual_oos = big_df %>%
    filter(party == "democrat", year == 2016, state == s) %>%
    select(D_pv2p) %>% pull()
  
  hisp_change_state_oos <- demog_state_df %>%
    filter(year == 2016) %>%
    group_by(year) %>%
    summarize(hisp_chng = mean(hisp_chng)) %>%
    select(hisp_chng) %>% pull()
  
  black_change_state_oos <- demog_state_df %>%
    filter(year == 2016) %>%
    group_by(year) %>%
    summarize(black_chng = mean(black_chng)) %>%
    select(black_chng) %>% pull()
  
  white_change_state_oos <- demog_state_df %>%
    filter(year == 2016) %>%
    group_by(year) %>%
    summarize(white_chng = mean(white_chng)) %>%
    select(white_chng) %>% pull()
  
  
  nat_republican_lm2_oos <- lm(R_pv ~ avg_poll + state, data = republicans_oos)
  nat_democrat_lm2_oos <- lm(D_pv ~ avg_poll + state, data = democrats_oos)
  
  nat_r_rsquared2_oos <- summary(nat_republican_lm2_oos)$r.squared
  nat_r_mse2_oos <- mean(nat_republican_lm2_oos$residuals^2)
  
  nat_d_rsquared2_oos <- summary(nat_democrat_lm2_oos)$r.squared
  nat_d_mse2_oos <- mean(nat_democrat_lm2_oos$residuals^2)
  
  
  nat_rep_prediction_oos <- predict(nat_republican_lm_oos, newdata = data.frame(avg_poll = republican_poll_oos, 
                                                                        incumbent_party=FALSE,
                                                                        hisp_chng = hisp_change_state_oos,
                                                                        black_chng = black_change_state_oos,
                                                                        white_chng = white_change_state_oos,
                                                                        lag_rpv2p = republican_lag_oos,
                                                                        state = s), interval = "confidence", level = 0.95)
  nat_dem_prediction_oos <- predict(nat_democrat_lm_oos, newdata = data.frame(avg_poll = democrat_poll_oos, 
                                                                      incumbent_party=TRUE,
                                                                      hisp_chng = hisp_change_state_oos,
                                                                      black_chng = black_change_state_oos,
                                                                      white_chng = white_change_state_oos,
                                                                      lag_dpv2p = democrat_lag_oos,
                                                                      state = s), interval = "confidence", level = 0.95)
  
  nat_rep_prediction_low_oos <- nat_rep_prediction_oos[,2]
  nat_rep_prediction_high_oos <- nat_rep_prediction_oos[,3]
  nat_rep_prediction_point_oos <- nat_rep_prediction_oos[,1]
  
  nat_dem_prediction_low_oos <- nat_dem_prediction_oos[,2]
  nat_dem_prediction_high_oos <- nat_dem_prediction_oos[,3]
  nat_dem_prediction_point_oos <- nat_dem_prediction_oos[,1]
  
  nat_rep_prediction2_oos <- predict(nat_republican_lm2_oos, newdata = data.frame(avg_poll = republican_poll_oos,
                                                                          state = s),interval = "confidence", level = 0.95)[,1]
  nat_rep_prediction2_low_oos <- predict(nat_republican_lm2_oos, newdata = data.frame(avg_poll = republican_poll_oos,
                                                                              state=s),interval = "confidence", level = 0.95)[,2]
  nat_rep_prediction2_high_oos <- predict(nat_republican_lm2_oos, newdata = data.frame(avg_poll = republican_poll_oos,
                                                                               state=s),interval = "confidence", level = 0.95)[,3]
  
  nat_dem_prediction2_oos <-predict(nat_democrat_lm2_oos, newdata = data.frame(avg_poll = democrat_poll_oos,
                                                                       state = s), interval = "confidence", level = 0.95)[,1]
  nat_dem_prediction2_low_oos <-predict(nat_democrat_lm2_oos, newdata = data.frame(avg_poll = democrat_poll_oos,
                                                                           state=s), interval = "confidence", level = 0.95)[,2]
  nat_dem_prediction2_high_oos <-predict(nat_democrat_lm2_oos, newdata = data.frame(avg_poll = democrat_poll_oos,
                                                                            state=s), interval = "confidence", level = 0.95)[,3]
  
  nat_rep_polls_oos = (nat_rep_prediction2_oos)
  nat_rep_plus_oos = 100*(nat_rep_prediction_point_oos / (nat_rep_prediction_point_oos + nat_dem_prediction_point_oos))
  nat_rep_plus2_oos = nat_rep_prediction_point_oos
  nat_rep_pv2p_oos = (0.5*nat_rep_polls_oos) + (0.5*nat_rep_plus_oos)
  
  nat_rep_polls_high_oos = (nat_rep_prediction2_high_oos)
  nat_rep_plus_high_oos = 100*(nat_rep_prediction_high_oos / (nat_rep_prediction_high_oos + nat_dem_prediction_high_oos))
  nat_rep_plus_high2_oos = nat_rep_prediction_high_oos
  
  
  nat_rep_polls_low_oos = ((nat_rep_prediction2_low_oos))
  nat_rep_plus_low_oos = 100*(nat_rep_prediction_low_oos / (nat_rep_prediction_low_oos + nat_dem_prediction_low_oos))
  nat_rep_plus_low2_oos = nat_rep_prediction_low_oos
  
  nat_dem_polls_oos = nat_dem_prediction2_oos
  nat_dem_plus_oos = 100*(nat_dem_prediction_point_oos / (nat_dem_prediction_point_oos + nat_rep_prediction_point_oos))
  nat_dem_plus2_oos = nat_dem_prediction_point_oos
  nat_dem_pv2p_oos = (0.5*(nat_dem_polls_oos)) + (0.5*nat_dem_plus_oos)
  
  nat_dem_polls_high_oos = nat_dem_prediction2_high_oos
  nat_dem_plus_high_oos = 100*(nat_dem_prediction_high_oos / (nat_dem_prediction_high_oos + nat_rep_prediction_high_oos))
  nat_dem_plus_high2_oos = nat_dem_prediction_high_oos
  
  nat_dem_polls_low_oos = nat_dem_prediction2_low_oos
  nat_dem_plus_low_oos = 100*(nat_dem_prediction_low_oos / (nat_dem_prediction_low_oos + nat_rep_prediction_low_oos))
  nat_dem_plus_low2_oos = nat_dem_prediction_low_oos
  
  nat_predictions_oos <- nat_predictions_oos %>%
    add_row(state_name = s, dem_p = nat_dem_pv2p_oos, rep_p = nat_rep_pv2p_oos, actual_d = democrat_actual_oos, actual_r = republican_actual_oos, r_rsquared = 0.5*(nat_r_rsquared_oos + nat_r_rsquared2_oos), d_rsquared = 0.5*(nat_d_rsquared_oos + nat_d_rsquared2_oos), d_mse = 0.5*(nat_d_mse_oos + nat_d_mse2_oos), r_mse = 0.5*(nat_r_mse_oos + nat_r_mse2_oos))
  
  nat_uncertainty_oos <- nat_uncertainty_oos %>%
    add_row(state_name = s, r_polls = nat_rep_polls_oos, r_polls_high = nat_rep_polls_high_oos, r_polls_low = nat_rep_polls_low_oos, d_polls = nat_dem_polls_oos, d_polls_high = nat_dem_polls_high_oos, d_polls_low = nat_dem_polls_low_oos, r_plus = nat_rep_plus2_oos, r_plus_high = nat_rep_plus_high2_oos, r_plus_low = nat_rep_plus_low2_oos, d_plus = nat_dem_plus2_oos, d_plus_high = nat_dem_plus_high2_oos, d_plus_low = nat_dem_plus_low2_oos)
}

nat_predictions_oos <- nat_predictions_oos %>% 
  mutate(d_margin = (dem_p - rep_p),
         d_actual_margin = (actual_d - actual_r),
         model_error = abs(d_actual_margin - d_margin),
         state = state_name,
         Winner = case_when(dem_p > rep_p ~ "Democrat",
                            rep_p > dem_p ~ "Republican"),
         swing = case_when(state_name == "Florida" ~ "yes",
                          state_name == "Georgia" ~ "yes",
                          state_name == "Iowa" ~ "yes",
                          state_name == "North Carolina" ~ "yes",
                          state_name == "Ohio" ~ "yes",
                          state_name == "Texas" ~ "yes",
                          state_name == "Arizona" ~ "yes",
                          state_name == "Michigan" ~ "yes",
                          state_name == "Minnesota" ~ "yes",
                          state_name == "Nevada" ~ "yes",
                          state_name == "New Hampshite" ~ "yes",
                          state_name == "Pennsylvania" ~ "yes",
                          state_name == "Wisconsin" ~ "yes"))

nat_predictions_oos$ev <- c(9, 3, 11, 6, 55, 9, 7, 3, 29, 16, 4, 4, 20, 11, 6, 6, 8, 8, 4, 10, 11, 16,
                        10, 6, 10, 3, 5, 6, 4, 14, 5, 29, 15, 3, 18, 7, 7, 20, 4, 9, 3, 11, 38, 6, 3, 13, 12,
                        5, 10, 3)

nat_predictions_oos %>%
  group_by(Winner) %>%
  summarize(totals = sum(ev)) 

nat_predictions_oos %>%
  group_by(Winner) %>%
  summarize(totals = sum(ev)) %>%
  ggplot(aes(x=Winner, y=totals, fill = Winner)) +
  geom_col() +
  geom_text(aes(label = totals), vjust = -0.5) +
  geom_hline(yintercept = 270, color = "gold")+
  scale_fill_manual(values=c("blue", "red")) +
  labs(title = "Predicted Electoral Vote Share by Party",
       x = "Candidate Party",
       y = "Electoral College Votes") +
  theme(legend.position = "none")

plot_usmap(data = nat_predictions_oos, regions = "states", values = "d_margin", labels = TRUE) +
  scale_fill_gradient2(
    high = "blue",
    mid = "white",
    low = "red",
    breaks = c(-50, -25, 0, 25, 50),
    limits = c(-50, 50),
    name = "Margin of Victory") +
  labs(title = "Predicted 2016 Electoral College Outcome",
       subtitle = "Out of Sample Fit Using Pooled Model")

# Loop to test unpooled model out of sample fit
predictions_oos <- tibble(state_name = c(), dem_p= c(), rep_p= c(), d_actual = c(), r_actual = c(), d_rsquared=c(), r_rsquared=c(), d_mse = c(), r_mse = c())

uncertainty_oos <- tibble(state_name = c(), r_polls = c(), r_polls_high = c(), r_polls_low = c(), d_polls = c(), d_polls_high = c(), d_polls_low = c(), r_plus = c(), r_plus_high=c(), r_plus_low = c(), d_plus=c(), d_plus_high=c(), d_plus_low = c())

for (s in state_list) {
  
  republicans <- big_df %>%
    filter(party == "republican", state == s, year >= 1976 & year != 2016)
  
  democrats <- big_df %>%
    filter(party == "democrat", state == s, year >= 1976 & year != 2016)
  
  republican_lm <- lm(R_pv ~ lag_rpv2p  + white_chng + incumbent_party, data = republicans)
  democrat_lm <- lm(D_pv ~ lag_dpv2p  + white_chng + incumbent_party, data = democrats)
  
  r_rsquared <- summary(republican_lm)$r.squared
  r_mse <- mean(republican_lm$residuals^2)
  
  
  d_rsquared <- summary(democrat_lm)$r.squared
  d_mse <- mean(democrat_lm$residuals^2)
  
  republican_poll <- big_df %>%
    filter(party == "republican", state == s, year == 2016) %>%
    select(avg_poll) %>%
    pull()
  
  republican_lag <- big_df %>%
    filter(party == "republican", state == s, year == 2016) %>%
    select(lag_rpv2p) %>% pull()
  
  republican_actual <- big_df %>%
    filter(party == "republican", state == s, year == 2016) %>%
    select(R_pv2p) %>% pull()
  
  democrat_poll <- big_df %>%
    filter(party == "democrat", state == s, year == 2016) %>%
    select(avg_poll) %>%
    pull()
  
  democrat_lag <- big_df %>%
    filter(party == "democrat", state == s, year == 2016) %>%
    select(lag_dpv2p) %>% pull()
  
  democrat_actual <- big_df %>%
    filter(party == "democrat", state == s, year == 2016) %>%
    select(D_pv2p) %>% pull()
  
  hisp_change_state <- demog_state_df %>%
    filter(state == s,
           year == 2016) %>%
    select(hisp_chng) %>%
    pull()
  
  black_change_state <- demog_state_df %>%
    filter(state == s,
           year == 2016) %>%
    select(black_chng) %>%
    pull()
  
  white_change_state <- demog_state_df %>%
    filter(state == s,
           year == 2016) %>%
    select(white_chng) %>%
    pull()
  
  age20_change_state <- demog_state_df %>%
    filter(state == s,
           year == 2016) %>%
    select(age20_chng) %>%
    pull()
  
  republican_lm2 <- lm(R_pv ~ avg_poll, data = republicans)
  democrat_lm2 <- lm(D_pv ~ avg_poll, data = democrats)
  
  r_rsquared2 <- summary(republican_lm2)$r.squared
  r_mse2 <- mean(republican_lm2$residuals^2)
  
  d_rsquared2 <- summary(democrat_lm2)$r.squared
  d_mse2 <- mean(democrat_lm2$residuals^2)
  
  
  rep_prediction <- predict(republican_lm, newdata = data.frame(avg_poll = republican_poll, 
                                                                incumbent_party=FALSE,
                                                                hisp_chng = hisp_change_state,
                                                                age20_chng=age20_change_state,
                                                                black_chng = black_change_state,
                                                                white_chng = white_change_state,
                                                                lag_rpv2p = republican_lag), interval = "confidence", level = 0.95)
  dem_prediction <- predict(democrat_lm, newdata = data.frame(avg_poll = democrat_poll, 
                                                              incumbent_party=TRUE,
                                                              hisp_chng = hisp_change_state,
                                                              age20_chng=age20_change_state,
                                                              black_chng = black_change_state,
                                                              white_chng = white_change_state,
                                                              lag_dpv2p = democrat_lag), interval = "confidence", level = 0.95)
  
  rep_prediction_low <- rep_prediction[,2]
  rep_prediction_high <- rep_prediction[,3]
  rep_prediction_point <- rep_prediction[,1]
  
  dem_prediction_low <- dem_prediction[,2]
  dem_prediction_high <- dem_prediction[,3]
  dem_prediction_point <- dem_prediction[,1]
  
  rep_prediction2 <- predict(republican_lm2, newdata = data.frame(avg_poll = republican_poll),interval = "confidence", level = 0.95)[,1]
  rep_prediction2_low <- predict(republican_lm2, newdata = data.frame(avg_poll = republican_poll),interval = "confidence", level = 0.95)[,2]
  rep_prediction2_high <- predict(republican_lm2, newdata = data.frame(avg_poll = republican_poll),interval = "confidence", level = 0.95)[,3]
  
  dem_prediction2 <-predict(democrat_lm2, newdata = data.frame(avg_poll = democrat_poll), interval = "confidence", level = 0.95)[,1]
  dem_prediction2_low <-predict(democrat_lm2, newdata = data.frame(avg_poll = democrat_poll), interval = "confidence", level = 0.95)[,2]
  dem_prediction2_high <-predict(democrat_lm2, newdata = data.frame(avg_poll = democrat_poll), interval = "confidence", level = 0.95)[,3]
  
  rep_polls= (rep_prediction2)
  rep_plus= 100*(rep_prediction_point / (rep_prediction_point + dem_prediction_point))
  rep_plus2 = rep_prediction_point
  rep_pv2p = (0.5*rep_polls) + (0.5*rep_plus)
  
  rep_polls_high = (rep_prediction2_high)
  rep_plus_high = 100*(rep_prediction_high / (rep_prediction_high + dem_prediction_high))
  rep_plus_high2 = rep_prediction_high
  
  
  rep_polls_low = ((rep_prediction2_low))
  rep_plus_low = 100*(rep_prediction_low / (rep_prediction_low + dem_prediction_low))
  rep_plus_low2 = rep_prediction_low
  
  dem_polls = dem_prediction2
  dem_plus = 100*(dem_prediction_point / (dem_prediction_point + rep_prediction_point))
  dem_plus2 = dem_prediction_point
  dem_pv2p = (0.5*(dem_polls)) + (0.5*dem_plus)
  
  dem_polls_high=dem_prediction2_high
  dem_plus_high = 100*(dem_prediction_high / (dem_prediction_high + rep_prediction_high))
  dem_plus_high2 = dem_prediction_high
  
  dem_polls_low = dem_prediction2_low
  dem_plus_low = 100*(dem_prediction_low / (dem_prediction_low + rep_prediction_low))
  dem_plus_low2 = dem_prediction_low
  
  predictions_oos <- predictions_oos %>%
    add_row(state_name = s, dem_p = dem_pv2p, rep_p = rep_pv2p, d_actual = democrat_actual, r_actual = republican_actual, r_rsquared = 0.5*(r_rsquared + r_rsquared2), d_rsquared = 0.5*(d_rsquared + d_rsquared2), d_mse = 0.5*(d_mse + d_mse2), r_mse = 0.5*(r_mse + r_mse2))
  
  uncertainty_oos <- uncertainty_oos %>%
    add_row(state_name = s, r_polls = rep_polls, r_polls_high = rep_polls_high, r_polls_low = rep_polls_low, d_polls = dem_polls, d_polls_high = dem_polls_high, d_polls_low = dem_polls_low, r_plus = rep_plus2, r_plus_high = rep_plus_high2, r_plus_low = rep_plus_low2, d_plus = dem_plus2, d_plus_high = dem_plus_high2, d_plus_low = dem_plus_low2)
}

predictions_oos <- predictions_oos %>% 
  mutate(d_margin = (dem_p - rep_p),
         d_actual_margin = (d_actual - r_actual),
         model_error = abs(d_actual_margin - d_margin),
         state = state_name,
         Winner = case_when(dem_p > rep_p ~ "Democrat",
                            rep_p > dem_p ~ "Republican"),
         swing = case_when(state_name == "Florida" ~ "yes",
                           state_name == "Georgia" ~ "yes",
                           state_name == "Iowa" ~ "yes",
                           state_name == "North Carolina" ~ "yes",
                           state_name == "Ohio" ~ "yes",
                           state_name == "Texas" ~ "yes",
                           state_name == "Arizona" ~ "yes",
                           state_name == "Michigan" ~ "yes",
                           state_name == "Minnesota" ~ "yes",
                           state_name == "Nevada" ~ "yes",
                           state_name == "New Hampshite" ~ "yes",
                           state_name == "Pennsylvania" ~ "yes",
                           state_name == "Wisconsin" ~ "yes"))

predictions_oos$ev <- c(9, 3, 11, 6, 55, 9, 7, 3, 29, 16, 4, 4, 20, 11, 6, 6, 8, 8, 4, 10, 11, 16,
                    10, 6, 10, 3, 5, 6, 4, 14, 5, 29, 15, 3, 18, 7, 7, 20, 4, 9, 3, 11, 38, 6, 3, 13, 12,
                    5, 10, 3)

predictions_oos %>%
  group_by(Winner) %>%
  summarize(totals = sum(ev)) 

predictions_oos %>%
  group_by(Winner) %>%
  summarize(totals = sum(ev)) %>%
  ggplot(aes(x=Winner, y=totals, fill = Winner)) +
  geom_col() +
  geom_text(aes(label = totals), vjust = -0.5) +
  geom_hline(yintercept = 270, color = "gold")+
  scale_fill_manual(values=c("blue", "red")) +
  labs(title = "Predicted Electoral Vote Share by Party",
       x = "Candidate Party",
       y = "Electoral College Votes") +
  theme(legend.position = "none")

plot_usmap(data = predictions_oos, regions = "states", values = "d_margin", labels = TRUE) +
  scale_fill_gradient2(
    high = "blue",
    mid = "white",
    low = "red",
    breaks = c(-50, -25, 0, 25, 50),
    limits = c(-50, 50),
    name = "Margin of Victory") +
  labs(title = "Predicted 2016 Electoral College Outcome",
       subtitle = "Out of Sample Fit Using Unpooled Model")


predictions_oos %>%
  summarize(sum = sum(ev))


# Creating summary gt table

gt = data.table(
  `Metric` = c("Rep. Candidate Predicted Electoral College Votes",
               "Dem. Candidate Predicted Electoral College Votes",
               "Average Dem. R-Squared Value",
               "Average Rep. R-Squared Value",
               "Average Dem. MSE",
               "Average Rep. MSE",
               "Average Error in Predicting 2016 Dem. Vote Share - All States",
               "Average Error in Predicting 2016 Rep. Vote Share - Battleground States"),
  `Pooled State Model` = c("126", "409", 0.870, 0.759, 10.1, 22.2, 8.09, 5.10),
  `Unpooled State Model` = c("259", "276", 0.675, 0.665, 6.23, 12.0, 10.5, 5.58)) %>%
  gt()  %>%
  tab_header(title = "Summary of Models")


gt
######  Scrap code
  `Republican Candidate Electoral College Votes` = c("126", "259"),
  `Democratic Candidate Electoral College Votes` = c("409", "276"),
  `Average Democratic R-Squared Value`= c("0.870", "0.675"),
  `Average Republican R-Squared Value` = c("0.759", "0.665"),
  `Average Democratic MSE` = c("10.1", "6.23"),
  `Average Republican MSE` = c("22.2", "12.0"),
  `Average Error in Predicting 2016 Democratic Vote Share - All States` = c(8.09, 10.5),
  `Average Error in Predicting 2016 Republican Vote Share - Battleground States` = c(5.10, 5.58)
  
) %>%
  gt()
  cols_align(align="center") %>%
  tab_header(title = "Summary of Models")
  
  
predictions %>% 
  select(state, d_margin) %>%
  filter(state == "Arizona" | state == "Texas" | state == "Georgia" | state == "North Carolina" | 
           state == "Florida" | state == "Ohio" | state == "Pennsylvania" | state == "Michigan" |
           state == "Wisconsin" | state == "Iowa" | state == "Nevada" | state == "New Hampshire" |
           state == "Minnesota") %>%
  arrange(d_margin) %>%
  mutate(State = state,
         `Dem. Win Margin` = d_margin) %>% 
  select(-state, -d_margin) %>% 
  gt() %>% 
  tab_header(title = "Predicted Dem. Win Margins",
             subtitle = "In Battleground States")
