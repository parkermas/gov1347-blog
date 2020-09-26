library(tidyverse)
library(ggplot2)

popvote_df <- read_csv("popvote_1948-2016.csv")
economy_df <- read_csv("econ.csv")
poll_avgs_df <- read_csv("pollavg_1968-2016.csv")
raw_polls_df <- read_csv("raw-polls.csv")
pollsters_df <- read_csv("pollster-ratings.csv")

pollsters_df$latest_poll <- as.Date(pollsters_df$latest_poll, format = "%m/%d/%y")

pollsters_df %>%
  filter(latest_poll >= "2015-01-01") %>%
  count(`538 Grade`) %>%
  ggplot(aes(x = `538 Grade`, y = n)) +
  geom_col(fill = "aquamarine3", color = "black") + 
  theme_bw() +
  labs(title = "Distribution of 538 Pollster Ratings",
       caption = "Including only pollsters who have released polls since 2015",
       x = "538 Pollster Grade",
       y = "Number of Pollsters with X Grade") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        )


polls_ratings_df <- raw_polls_df %>%
  left_join(pollsters_df, by= pollster_rating_id)
