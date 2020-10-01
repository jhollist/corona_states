library(readr)
library(ggplot2)
library(dplyr)

source("doubling_data.R")

covid_states <- read_csv("http://covidtracking.com/api/states/daily.csv") %>%
  select(date, state:total) %>%
  mutate(date = ymd(date)) %>%
  ungroup() %>%
  filter(total > 0) %>%
  select(date, state, positive, negative, pending, hospitalized, death, total) %>%
  #rbind(tibble(date = "2020-04-08", state = "RI", positive = 1450, negative = NA, 
  #             pending = NA, hospitalized = NA, death = NA, total = NA)) %>%
  arrange(date) %>% 
  group_by(state) %>%
  mutate(days_since = as.numeric(date - min(date))) %>%
  ungroup()

covid_us <- read_csv("http://covidtracking.com/api/us/daily.csv") %>% 
  select(date, positive, negative, pending, hospitalized, death, total) %>%
  mutate(date = ymd(date)) %>%
  filter(total > 0) %>%
  arrange(date) %>%
  mutate(days_since = as.numeric(date - min(date)))

covid_states_gg <- covid_states %>%
  filter(state %in% c("RI", "NY", "MA", "CT", "NH", "VT", "ME", "KS", "MO")) %>%
  ggplot(aes(x = days_since, y = positive, color = state)) +
  geom_line() +
  geom_line(data = every_1_day, aes(x=days_since, y = cases), color = "black", size = 1.5) +
  geom_line(data = every_2_day, aes(x=days_since, y = cases), color = "red", size = 1.5) +
  geom_line(data = every_3_day, aes(x=days_since, y = cases), color = "yellow", size = 1.5) +
  geom_line(data = every_4_day, aes(x=days_since, y = cases), color = "green", size = 1.5) +
  scale_y_log10()
plotly::ggplotly(covid_states_gg)

max_date <- max_date <- max(covid_states$date, na.rm = TRUE)

covid_states_gg_2 <- covid_states %>%
  filter(state %in% c("RI", "NY", "MA", "CT", "NH", "VT", "ME", "KS", "MO")) %>%
  ggplot(aes(x = date, y = death, color = state)) +
  geom_line(size = 1.2) +
  geom_line(data = covid_us, aes(x = date, y = death), color = "darkgrey", size = 1.2) +
  theme_minimal() +
  labs(x = "Days since first case →", y = "", color = "Country: Total active cases",
       title = paste("COVID-19 Outbreaks Can Vary Dramatically ( updated on:", max_date , ")"),
       subtitle = "The way a country responds to a COVID-19 outbreak has an impact on the speed and degree of spread.") +
  theme(axis.title.x = element_text(hjust = 0, vjust = 0.1)) +
  scale_y_log10()
plotly::ggplotly(covid_states_gg_2)

