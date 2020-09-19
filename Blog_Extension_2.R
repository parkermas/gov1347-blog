library(tidyverse)
library(ggplot2)
library(usmap)
library(ggthemes)
library(gt)

economy_df <- read_csv("econ.csv") 
popvote_df <- read_csv("popvote_1948-2016.csv") 
local_econ_df <- read_csv("local.csv")
state_popvote_df <- read_csv("1976-2016-president.csv")

state_popvote_df <- state_popvote_df %>%
  filter(party == "democrat" | party == "republican") %>%
  mutate(pct_vote = candidatevotes/totalvotes)

popvote_1976 <- state_popvote_df[state_popvote_df$year == 1976 & state_popvote_df$candidate != "Carter, Jimmy", ]
popvote_1980 <- state_popvote_df[state_popvote_df$year == 1980 & state_popvote_df$candidate != "Reagan, Ronald", ]
popvote_1984 <- state_popvote_df[state_popvote_df$year == 1984 & state_popvote_df$candidate != "Mondale, Walter", ]
popvote_1988 <- state_popvote_df[state_popvote_df$year == 1988 & state_popvote_df$candidate != "Dukakis, Michael", ]
popvote_1992 <- state_popvote_df[state_popvote_df$year == 1992 & state_popvote_df$candidate != "Clinton, Bill", ]
popvote_1996 <- state_popvote_df[state_popvote_df$year == 1996 & state_popvote_df$candidate != "Dole, Robert", ]
popvote_2000 <- state_popvote_df[state_popvote_df$year == 2000 & state_popvote_df$candidate != "Bush, George W.", ]
popvote_2004 <- state_popvote_df[state_popvote_df$year == 2004 & state_popvote_df$candidate != "Kerry, John", ]
popvote_2008 <- state_popvote_df[state_popvote_df$year == 2008 & state_popvote_df$candidate != "Obama, Barack H.", ]
popvote_2012 <- state_popvote_df[state_popvote_df$year == 2012 & state_popvote_df$candidate != "Romney, Mitt", ]
popvote_2016 <- state_popvote_df[state_popvote_df$year == 2016 & state_popvote_df$candidate != "Trump, Donald J.", ]

df_1 <- rbind(popvote_1976, popvote_1980)
df_2 <- rbind(df_1, popvote_1984)
df_3 <- rbind(df_2, popvote_1988)
df_4 <- rbind(df_3, popvote_1992)
df_5 <- rbind(df_4, popvote_1996)
df_6 <- rbind(df_5, popvote_2000)
df_7 <- rbind(df_6, popvote_2004)
df_8 <- rbind(df_7, popvote_2008)
df_9 <- rbind(df_8, popvote_2012)
df_10 <- rbind(df_9, popvote_2016)



local_econ_df$fips <- as.numeric(local_econ_df$`FIPS Code`)
  
local_econ_df <- local_econ_df %>%
  select(-"FIPS Code")


 popvote_df <- popvote_df %>%
    filter(incumbent_party == TRUE) %>%
    select(year, party, pv2p) 

popvote_df %>%
  left_join(economy_df, by = "year") %>%
  filter(quarter == 2) %>%
  ggplot(aes(x = unemployment, y = pv2p)) +
  geom_smooth(formula = y ~ x, method = "lm") +
  theme_bw() +
  labs(title = "Relationship between Q2 national unemployment rate and incumbent party PV share",
       x = "Q2 national unemployment rate (X)",
       y = "Incumbent party popular vote share (Y)") + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 13))



cor(test$unemployment, test$pv2p)


local_econ_df$year <- local_econ_df$Year

local_econ_df$state <- local_econ_df$`State and area`



local_econ_m5 <- local_econ_df %>%
  filter(Month == "10") 

full_df$pct_vote <- as.numeric(full_df$pct_vote)


full_df <- df_10 %>%
  left_join(local_econ_m5, by = c("year" = "year", "state" = "state")) %>%
  select(-"Year", "State and area") %>%
  filter(`State and area` != "Los Angeles County") %>%
  filter(`State and area` != "New York city") 
  
full_df %>%
  ggplot(aes(x=Unemployed_prce, y=pct_vote)) + 
  geom_smooth(method="lm", formula = y ~ x) +
  
  xlab("State unemployment rate, October (X)") +
  ylab("Incumbent party popular vote share (Y)") +
  theme_bw() +
  ggtitle("Relationship between October state unemployment rate and incumbent party PV share") + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 13))


  

cor(full_df$pct_vote, full_df$Unemployed_prce)

lm_econ <- lm(pct_vote ~ Unemployed_prce, data = full_df)
summary(lm_econ)

full_df %>%
  ggplot(aes(x = Unemployed_prce, y = pct_vote)) +
  geom_smooth(method="lm", formula = y ~ x) 


gt = data.table::data.table(
  Model = 1:3,
  Predictors = c("Q2 GDP Growth", "Q2 National Unemployment Rate", "October State Unemployment Rates"),
  `Correlation between predictor and PV share` = c(0.571, 0.0067, 0.017),
  `P-value` = c(0.0133, 0.979, 0.682),
  `Adjusted R-squared` = c(0.284, -0.06245, -0.001485)
) %>%
  gt() %>%
  cols_align(align="center")


gt
