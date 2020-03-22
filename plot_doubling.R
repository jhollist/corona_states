library(readr)
library(ggplot2)
library(dplyr)

source("doubling_data.R")

covid_states <- read_csv("http://covidtracking.com/api/states/daily.csv") %>%
  select(date, state:total) %>%
  mutate(date = ymd(date))

covid_states_gg <- covid_states %>%
  filter(state == "RI" | state == "NY") %>%
  ggplot(aes(x = date, y = positive, color = state)) +
  geom_line() +
  geom_line(data = every_1_day, aes(x=date_1, y = cases), color = "black", size = 1.5) +
  geom_line(data = every_2_day, aes(x=date_2, y = cases), color = "red", size = 1.5) +
  geom_line(data = every_3_day, aes(x=date_3, y = cases), color = "yellow", size = 1.5) +
  geom_line(data = every_4_day, aes(x=date_4, y = cases), color = "green", size = 1.5) +
  scale_y_log10()
covid_states_gg
